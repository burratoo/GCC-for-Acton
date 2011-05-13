/* Inlining decision heuristics.
   Copyright (C) 2003, 2004, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Jan Hubicka

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Analysis used by the inliner and other passes limiting code size growth.

   We estimate for each function
     - function body size
     - average function execution time
     - inlining size benefit (that is how much of function body size
       and its call sequence is expected to disappear by inlining)
     - inlining time benefit
     - function frame size
   For each call
     - call statement size and time

   inlinie_summary datastructures store above information locally (i.e.
   parameters of the function itself) and globally (i.e. parameters of
   the function created by applying all the inline decisions already
   present in the callgraph).

   We provide accestor to the inline_summary datastructure and
   basic logic updating the parameters when inlining is performed. 

   The summaries are context sensitive.  Context means
     1) partial assignment of known constant values of operands
     2) whether function is inlined into the call or not.
   It is easy to add more variants.  To represent function size and time
   that depends on context (i.e. it is known to be optimized away when
   context is known either by inlining or from IP-CP and clonning),
   we use predicates. Predicates are logical formulas in
   conjunctive-disjunctive form consisting of clauses. Clauses are bitmaps
   specifying what conditions must be true. Conditions are simple test
   of the form described above.

   In order to make predicate (possibly) true, all of its clauses must
   be (possibly) true. To make clause (possibly) true, one of conditions
   it mentions must be (possibly) true.  There are fixed bounds on
   number of clauses and conditions and all the manipulation functions
   are conservative in positive direction. I.e. we may lose precision
   by thinking that predicate may be true even when it is not.

   estimate_edge_size and estimate_edge_growth can be used to query
   function size/time in the given context.  inline_merge_summary merges
   properties of caller and callee after inlining.

   Finally pass_inline_parameters is exported.  This is used to drive
   computation of function parameters used by the early inliner. IPA
   inlined performs analysis via its analyze_function method. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "flags.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "params.h"
#include "tree-pass.h"
#include "coverage.h"
#include "ggc.h"
#include "tree-flow.h"
#include "ipa-prop.h"
#include "lto-streamer.h"
#include "ipa-inline.h"
#include "alloc-pool.h"

/* Estimate runtime of function can easilly run into huge numbers with many
   nested loops.  Be sure we can compute time * INLINE_SIZE_SCALE in integer.
   For anything larger we use gcov_type.  */
#define MAX_TIME 1000000

/* Number of bits in integer, but we really want to be stable across different
   hosts.  */
#define NUM_CONDITIONS 32

enum predicate_conditions
{
  predicate_false_condition = 0,
  predicate_not_inlined_condition = 1,
  predicate_first_dynamic_condition = 2
};

/* Special condition code we use to represent test that operand is compile time
   constant.  */
#define IS_NOT_CONSTANT ERROR_MARK

/* Holders of ipa cgraph hooks: */
static struct cgraph_node_hook_list *function_insertion_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_2edge_hook_list *edge_duplication_hook_holder;
static struct cgraph_edge_hook_list *edge_removal_hook_holder;
static void inline_node_removal_hook (struct cgraph_node *, void *);
static void inline_node_duplication_hook (struct cgraph_node *,
					  struct cgraph_node *, void *);
static void inline_edge_removal_hook (struct cgraph_edge *, void *);
static void inline_edge_duplication_hook (struct cgraph_edge *,
					  struct cgraph_edge *,
				          void *);

/* VECtor holding inline summaries.  
   In GGC memory because conditions might point to constant trees.  */
VEC(inline_summary_t,gc) *inline_summary_vec;
VEC(inline_edge_summary_t,heap) *inline_edge_summary_vec;

/* Cached node/edge growths.  */
VEC(int,heap) *node_growth_cache;
VEC(edge_growth_cache_entry,heap) *edge_growth_cache;

/* Edge predicates goes here.  */
static alloc_pool edge_predicate_pool;

/* Return true predicate (tautology).
   We represent it by empty list of clauses.  */

static inline struct predicate
true_predicate (void)
{
  struct predicate p;
  p.clause[0]=0;
  return p;
}


/* Return predicate testing single condition number COND.  */

static inline struct predicate
single_cond_predicate (int cond)
{
  struct predicate p;
  p.clause[0]=1 << cond;
  p.clause[1]=0;
  return p;
}


/* Return false predicate.  First clause require false condition.  */

static inline struct predicate
false_predicate (void)
{
  return single_cond_predicate (predicate_false_condition);
}


/* Return true if P is (false).  */

static inline bool
true_predicate_p (struct predicate *p)
{
  return !p->clause[0];
}


/* Return true if P is (false).  */

static inline bool
false_predicate_p (struct predicate *p)
{
  if (p->clause[0] == (1 << predicate_false_condition))
    {
      gcc_checking_assert (!p->clause[1]
			   && p->clause[0] == 1 << predicate_false_condition);
      return true;
    }
  return false;
}


/* Return predicate that is set true when function is not inlined.  */
static inline struct predicate
not_inlined_predicate (void)
{
  return single_cond_predicate (predicate_not_inlined_condition);
}


/* Add condition to condition list CONDS.  */

static struct predicate
add_condition (struct inline_summary *summary, int operand_num,
	       enum tree_code code, tree val)
{
  int i;
  struct condition *c;
  struct condition new_cond;

  for (i = 0; VEC_iterate (condition, summary->conds, i, c); i++)
    {
      if (c->operand_num == operand_num
	  && c->code == code
	  && c->val == val)
        return single_cond_predicate (i + predicate_first_dynamic_condition);
    }
  /* Too many conditions.  Give up and return constant true.  */
  if (i == NUM_CONDITIONS - predicate_first_dynamic_condition)
    return true_predicate ();

  new_cond.operand_num = operand_num;
  new_cond.code = code;
  new_cond.val = val;
  VEC_safe_push (condition, gc, summary->conds, &new_cond);
  return single_cond_predicate (i + predicate_first_dynamic_condition);
}


/* Add clause CLAUSE into the predicate P.  */

static inline void
add_clause (struct predicate *p, clause_t clause)
{
  int i;
  int i2;
  int insert_here = -1;

  /* True clause.  */
  if (!clause)
    return;

  /* False clause makes the whole predicate false.  Kill the other variants.  */
  if (clause == (1 << predicate_false_condition))
    {
      p->clause[0] = (1 << predicate_false_condition);
      p->clause[1] = 0;
      return;
    }
  if (false_predicate_p (p))
    return;

  /* No one should be sily enough to add false into nontrivial clauses.  */
  gcc_checking_assert (!(clause & (1 << predicate_false_condition)));

  /* Look where to insert the clause.  At the same time prune out
     clauses of P that are implied by the new clause and thus
     redundant.  */
  for (i = 0, i2 = 0; i <= MAX_CLAUSES; i++)
    {
      p->clause[i2] = p->clause[i];

      if (!p->clause[i])
	break;

      /* If p->clause[i] implies clause, there is nothing to add.  */
      if ((p->clause[i] & clause) == p->clause[i])
	{
	  /* We had nothing to add, none of clauses should've become redundant.  */
	  gcc_checking_assert (i == i2);
	  return;
	}

      if (p->clause[i] < clause && insert_here < 0)
	insert_here = i2;

      /* If clause implies p->clause[i], then p->clause[i] becomes redundant.
	 Otherwise the p->clause[i] has to stay.  */
      if ((p->clause[i] & clause) != clause)
	i2++;
    }
  /* We run out of variants.  Be conservative in positive direction.  */
  if (i2 == MAX_CLAUSES)
    return;
  /* Keep clauses in decreasing order. This makes equivalence testing easy.  */
  p->clause[i2 + 1] = 0;
  if (insert_here >= 0)
    for (;i2 > insert_here; i2--)
      p->clause[i2] = p->clause[i2 - 1];
  else
    insert_here = i2;
  p->clause[insert_here] = clause;
}


/* Return P & P2.  */

static struct predicate
and_predicates (struct predicate *p, struct predicate *p2)
{
  struct predicate out = *p;
  int i;

  /* Avoid busy work.  */
  if (false_predicate_p (p2) || true_predicate_p (p))
    return *p2;
  if (false_predicate_p (p) || true_predicate_p (p2))
    return *p;

  /* See how far predicates match.  */
  for (i = 0; p->clause[i] && p->clause[i] == p2->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
    }
    
  /* Combine the predicates rest.  */
  for (; p2->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      add_clause (&out, p2->clause[i]);
    }
  return out;
}


/* Return true if predicates are obviously equal.  */

static inline bool
predicates_equal_p (struct predicate *p, struct predicate *p2)
{
  int i;
  for (i = 0; p->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      gcc_checking_assert (p->clause [i] > p->clause[i + 1]);
      gcc_checking_assert (!p2->clause[i] || p2->clause [i] > p2->clause[i + 1]);
      if (p->clause[i] != p2->clause[i])
        return false;
    }
  return !p2->clause[i];
}


/* Return P | P2.  */

static struct predicate
or_predicates (struct predicate *p, struct predicate *p2)
{
  struct predicate out = true_predicate ();
  int i,j;

  /* Avoid busy work.  */
  if (false_predicate_p (p2) || true_predicate_p (p))
    return *p;
  if (false_predicate_p (p) || true_predicate_p (p2))
    return *p2;
  if (predicates_equal_p (p, p2))
    return *p;

  /* OK, combine the predicates.  */
  for (i = 0; p->clause[i]; i++)
    for (j = 0; p2->clause[j]; j++)
      {
        gcc_checking_assert (i < MAX_CLAUSES && j < MAX_CLAUSES);
        add_clause (&out, p->clause[i] | p2->clause[j]);
      }
  return out;
}


/* Having partial truth assignment in POSSIBLE_TRUTHS, return false if predicate P
   to be false.  */

static bool
evaluate_predicate (struct predicate *p, clause_t possible_truths)
{
  int i;

  /* True remains true.  */
  if (true_predicate_p (p))
    return true;

  gcc_assert (!(possible_truths & (1 << predicate_false_condition)));

  /* See if we can find clause we can disprove.  */
  for (i = 0; p->clause[i]; i++)
    {
      gcc_checking_assert (i < MAX_CLAUSES);
      if (!(p->clause[i] & possible_truths))
        return false;
    }
  return true;
}


/* Dump conditional COND.  */

static void
dump_condition (FILE *f, conditions conditions, int cond)
{
  condition *c;
  if (cond == predicate_false_condition)
    fprintf (f, "false");
  else if (cond == predicate_not_inlined_condition)
    fprintf (f, "not inlined");
  else
    {
      c = VEC_index (condition, conditions, cond - predicate_first_dynamic_condition);
      fprintf (f, "op%i", c->operand_num);
      if (c->code == IS_NOT_CONSTANT)
	{
	  fprintf (f, " not constant");
	  return;
	}
      fprintf (f, " %s ", op_symbol_code (c->code));
      print_generic_expr (f, c->val, 1);
    }
}


/* Dump clause CLAUSE.  */

static void
dump_clause (FILE *f, conditions conds, clause_t clause)
{
  int i;
  bool found = false;
  fprintf (f, "(");
  if (!clause)
    fprintf (f, "true");
  for (i = 0; i < NUM_CONDITIONS; i++)
    if (clause & (1 << i))
      {
	if (found)
	  fprintf (f, " || ");
	found = true;
        dump_condition (f, conds, i);
      }
  fprintf (f, ")");
}


/* Dump predicate PREDICATE.  */

static void
dump_predicate (FILE *f, conditions conds, struct predicate *pred)
{
  int i;
  if (true_predicate_p (pred))
    dump_clause (f, conds, 0);
  else
    for (i = 0; pred->clause[i]; i++)
      {
	if (i)
	  fprintf (f, " && ");
        dump_clause (f, conds, pred->clause[i]);
      }
  fprintf (f, "\n");
}


/* Record SIZE and TIME under condition PRED into the inline summary.  */

static void
account_size_time (struct inline_summary *summary, int size, int time, struct predicate *pred)
{
  size_time_entry *e;
  bool found = false;
  int i;

  if (false_predicate_p (pred))
    return;

  /* We need to create initial empty unconitional clause, but otherwie
     we don't need to account empty times and sizes.  */
  if (!size && !time && summary->entry)
    return;

  /* Watch overflow that might result from insane profiles.  */
  if (time > MAX_TIME * INLINE_TIME_SCALE)
    time = MAX_TIME * INLINE_TIME_SCALE;
  gcc_assert (time >= 0);

  for (i = 0; VEC_iterate (size_time_entry, summary->entry, i, e); i++)
    if (predicates_equal_p (&e->predicate, pred))
      {
	found = true;
        break;
      }
  if (i == 32)
    {
      i = 0;
      found = true;
      e = VEC_index (size_time_entry, summary->entry, 0);
      gcc_assert (!e->predicate.clause[0]);
    }
  if (dump_file && (dump_flags & TDF_DETAILS) && (time || size))
    {
      fprintf (dump_file, "\t\tAccounting size:%3.2f, time:%3.2f on %spredicate:",
	       ((double)size) / INLINE_SIZE_SCALE, ((double)time) / INLINE_TIME_SCALE,
	       found ? "" : "new ");
      dump_predicate (dump_file, summary->conds, pred);
    }
  if (!found)
    {
      struct size_time_entry new_entry;
      new_entry.size = size;
      new_entry.time = time;
      new_entry.predicate = *pred;
      VEC_safe_push (size_time_entry, gc, summary->entry, &new_entry);
    }
  else
    {
      e->size += size;
      e->time += time;
      if (e->time > MAX_TIME * INLINE_TIME_SCALE)
	e->time = MAX_TIME * INLINE_TIME_SCALE;
    }
}

/* Set predicate for edge E.  */

static void
edge_set_predicate (struct cgraph_edge *e, struct predicate *predicate)
{
  struct inline_edge_summary *es = inline_edge_summary (e);
  if (predicate && !true_predicate_p (predicate))
    {
      if (!es->predicate)
        es->predicate = (struct predicate *)pool_alloc (edge_predicate_pool);
      *es->predicate = *predicate;
    }
  else
    {
      if (es->predicate)
        pool_free (edge_predicate_pool, es->predicate);
      es->predicate = NULL;
    }
}


/* KNOWN_VALS is partial mapping of parameters of NODE to constant values.
   Return clause of possible truths. When INLINE_P is true, assume that
   we are inlining.  */

static clause_t
evaluate_conditions_for_known_args (struct cgraph_node *node,
				    bool inline_p,
				    VEC (tree, heap) *known_vals)
{
  clause_t clause = inline_p ? 0 : 1 << predicate_not_inlined_condition;
  struct inline_summary *info = inline_summary (node);
  int i;
  struct condition *c;

  for (i = 0; VEC_iterate (condition, info->conds, i, c); i++)
    {
      tree val = VEC_index (tree, known_vals, c->operand_num);
      tree res;

      if (!val)
	{
	  clause |= 1 << (i + predicate_first_dynamic_condition);
	  continue;
	}
      if (c->code == IS_NOT_CONSTANT)
	continue;
      res = fold_binary_to_constant (c->code, boolean_type_node, val, c->val);
      if (res
	  && integer_zerop (res))
	continue;
      clause |= 1 << (i + predicate_first_dynamic_condition);
    }
  return clause;
}


/* Work out what conditions might be true at invocation of E.  */

static clause_t
evaluate_conditions_for_edge (struct cgraph_edge *e, bool inline_p)
{
  clause_t clause = inline_p ? 0 : 1 << predicate_not_inlined_condition;
  struct inline_summary *info = inline_summary (e->callee);
  int i;

  if (ipa_node_params_vector && info->conds
      /* FIXME: it seems that we forget to get argument count in some cases,
	 probaby for previously indirect edges or so.  */
      && ipa_get_cs_argument_count (IPA_EDGE_REF (e)))
    {
      struct ipa_node_params *parms_info;
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      int i, count = ipa_get_cs_argument_count (args);
      struct ipcp_lattice lat;
      VEC (tree, heap) *known_vals = NULL;

      if (e->caller->global.inlined_to)
        parms_info = IPA_NODE_REF (e->caller->global.inlined_to);
      else
        parms_info = IPA_NODE_REF (e->caller);

      VEC_safe_grow_cleared (tree, heap, known_vals, count);
      for (i = 0; i < count; i++)
	{
	  ipa_lattice_from_jfunc (parms_info, &lat, ipa_get_ith_jump_func (args, i));
	  if (lat.type == IPA_CONST_VALUE)
	    VEC_replace (tree, known_vals, i, lat.constant);
	}
      clause = evaluate_conditions_for_known_args (e->callee,
						   inline_p, known_vals);
      VEC_free (tree, heap, known_vals);
    }
  else
    for (i = 0; i < (int)VEC_length (condition, info->conds); i++)
      clause |= 1 << (i + predicate_first_dynamic_condition);

  return clause;
}


/* Work out what conditions might be true at invocation of NODE
   that is (future) ipa-cp clone.  */

static clause_t
evaluate_conditions_for_ipcp_clone (struct cgraph_node *node)
{
  struct ipa_node_params *parms_info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (parms_info);
  struct ipcp_lattice *lat;
  VEC (tree, heap) *known_vals = NULL;
  clause_t clause;

  VEC_safe_grow_cleared (tree, heap, known_vals, count);
  for (i = 0; i < count; i++)
    {
      lat = ipa_get_lattice (parms_info, i);
      if (lat->type == IPA_CONST_VALUE)
	VEC_replace (tree, known_vals, i, lat->constant);
    }
  clause = evaluate_conditions_for_known_args (node, false, known_vals);
  VEC_free (tree, heap, known_vals);
  return clause;
}


/* Allocate the inline summary vector or resize it to cover all cgraph nodes. */

static void
inline_summary_alloc (void)
{
  if (!node_removal_hook_holder)
    node_removal_hook_holder =
      cgraph_add_node_removal_hook (&inline_node_removal_hook, NULL);
  if (!edge_removal_hook_holder)
    edge_removal_hook_holder =
      cgraph_add_edge_removal_hook (&inline_edge_removal_hook, NULL);
  if (!node_duplication_hook_holder)
    node_duplication_hook_holder =
      cgraph_add_node_duplication_hook (&inline_node_duplication_hook, NULL);
  if (!edge_duplication_hook_holder)
    edge_duplication_hook_holder =
      cgraph_add_edge_duplication_hook (&inline_edge_duplication_hook, NULL);

  if (VEC_length (inline_summary_t, inline_summary_vec)
      <= (unsigned) cgraph_max_uid)
    VEC_safe_grow_cleared (inline_summary_t, gc,
			   inline_summary_vec, cgraph_max_uid + 1);
  if (VEC_length (inline_edge_summary_t, inline_edge_summary_vec)
      <= (unsigned) cgraph_edge_max_uid)
    VEC_safe_grow_cleared (inline_edge_summary_t, heap,
			   inline_edge_summary_vec, cgraph_edge_max_uid + 1);
  if (!edge_predicate_pool)
    edge_predicate_pool = create_alloc_pool ("edge predicates", sizeof (struct predicate),
					     10);
}

/* Hook that is called by cgraph.c when a node is removed.  */

static void
inline_node_removal_hook (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  struct inline_summary *info;
  if (VEC_length (inline_summary_t, inline_summary_vec)
      <= (unsigned)node->uid)
    return;
  info = inline_summary (node);
  reset_node_growth_cache (node);
  VEC_free (condition, gc, info->conds);
  VEC_free (size_time_entry, gc, info->entry);
  info->conds = NULL;
  info->entry = NULL;
  memset (info, 0, sizeof (inline_summary_t));
}


/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
inline_node_duplication_hook (struct cgraph_node *src, struct cgraph_node *dst,
			      ATTRIBUTE_UNUSED void *data)
{
  struct inline_summary *info;
  inline_summary_alloc ();
  info = inline_summary (dst);
  memcpy (info, inline_summary (src),
	  sizeof (struct inline_summary));
  /* TODO: as an optimization, we may avoid copying conditions
     that are known to be false or true.  */
  info->conds = VEC_copy (condition, gc, info->conds);

  /* When there are any replacements in the function body, see if we can figure
     out that something was optimized out.  */
  if (ipa_node_params_vector && dst->clone.tree_map)
    {
      VEC(size_time_entry,gc) *entry = info->entry;
      /* Use SRC parm info since it may not be copied yet.  */
      struct ipa_node_params *parms_info = IPA_NODE_REF (src);
      VEC (tree, heap) *known_vals = NULL;
      int count = ipa_get_param_count (parms_info);
      int i,j;
      clause_t possible_truths;
      struct predicate true_pred = true_predicate ();
      size_time_entry *e;
      int optimized_out_size = 0;
      gcov_type optimized_out_time = 0;
      bool inlined_to_p = false;
      struct cgraph_edge *edge;

      info->entry = false;
      VEC_safe_grow_cleared (tree, heap, known_vals, count);
      for (i = 0; i < count; i++)
        {
	  tree t = ipa_get_param (parms_info, i);
	  struct ipa_replace_map *r;

	  for (j = 0;
	       VEC_iterate (ipa_replace_map_p, dst->clone.tree_map, j, r);
	       j++)
	    {
	      if (r->old_tree == t
		  && r->replace_p
		  && !r->ref_p)
		{
		  VEC_replace (tree, known_vals, i, r->new_tree);
		  break;
		}
	    }
	}
      possible_truths = evaluate_conditions_for_known_args (dst,
							    false, known_vals);
      VEC_free (tree, heap, known_vals);

      account_size_time (info, 0, 0, &true_pred);

      /* Remap size_time vectors.
	 Simplify the predicate by prunning out alternatives that are known
	 to be false.
	 TODO: as on optimization, we can also eliminate conditions known to be true.  */
      for (i = 0; VEC_iterate (size_time_entry, entry, i, e); i++)
	{
	  struct predicate new_predicate = true_predicate ();
	  for (j = 0; e->predicate.clause[j]; j++)
	    if (!(possible_truths & e->predicate.clause[j]))
	      {
		new_predicate = false_predicate ();
		break;
	      }
	    else
	      add_clause (&new_predicate,
			  possible_truths & e->predicate.clause[j]);
	  if (false_predicate_p (&new_predicate))
	    {
	      optimized_out_size += e->size;
	      optimized_out_time += e->time;
	    }
	  else
	    account_size_time (info, e->size, e->time, &new_predicate);
	}

      /* Remap edge predicates with the same simplificaiton as above.  */
      for (edge = dst->callees; edge; edge = edge->next_callee)
	{
	  struct predicate new_predicate = true_predicate ();
	  struct inline_edge_summary *es = inline_edge_summary (edge);

	  if (!edge->inline_failed)
	    inlined_to_p = true;
	  if (!es->predicate)
	    continue;
	  for (j = 0; es->predicate->clause[j]; j++)
	    if (!(possible_truths & es->predicate->clause[j]))
	      {
		new_predicate = false_predicate ();
		break;
	      }
	    else
	      add_clause (&new_predicate,
			  possible_truths & es->predicate->clause[j]);
	  if (false_predicate_p (&new_predicate)
	      && !false_predicate_p (es->predicate))
	    {
	      optimized_out_size += es->call_stmt_size * INLINE_SIZE_SCALE;
	      optimized_out_time += (es->call_stmt_time
				     * (INLINE_TIME_SCALE / CGRAPH_FREQ_BASE)
				     * edge->frequency);
	      edge->frequency = 0;
	    }
	  *es->predicate = new_predicate;
	}

      /* Remap indirect edge predicates with the same simplificaiton as above.  */
      for (edge = dst->indirect_calls; edge; edge = edge->next_callee)
	{
	  struct predicate new_predicate = true_predicate ();
	  struct inline_edge_summary *es = inline_edge_summary (edge);

	  if (!edge->inline_failed)
	    inlined_to_p = true;
	  if (!es->predicate)
	    continue;
	  for (j = 0; es->predicate->clause[j]; j++)
	    if (!(possible_truths & es->predicate->clause[j]))
	      {
		new_predicate = false_predicate ();
		break;
	      }
	    else
	      add_clause (&new_predicate,
			  possible_truths & es->predicate->clause[j]);
	  if (false_predicate_p (&new_predicate)
	      && !false_predicate_p (es->predicate))
	    {
	      optimized_out_size += es->call_stmt_size * INLINE_SIZE_SCALE;
	      optimized_out_time += (es->call_stmt_time
				     * (INLINE_TIME_SCALE / CGRAPH_FREQ_BASE)
				     * edge->frequency);
	      edge->frequency = 0;
	    }
	  *es->predicate = new_predicate;
	}

      /* If inliner or someone after inliner will ever start producing
	 non-trivial clones, we will get trouble with lack of information
	 about updating self sizes, because size vectors already contains
	 sizes of the calees.  */
      gcc_assert (!inlined_to_p 
		  || (!optimized_out_size && !optimized_out_time));

      info->size -= optimized_out_size / INLINE_SIZE_SCALE;
      info->self_size -= optimized_out_size / INLINE_SIZE_SCALE;
      gcc_assert (info->size > 0);
      gcc_assert (info->self_size > 0);

      optimized_out_time /= INLINE_TIME_SCALE;
      if (optimized_out_time > MAX_TIME)
	optimized_out_time = MAX_TIME;
      info->time -= optimized_out_time;
      info->self_time -= optimized_out_time;
      if (info->time < 0)
	info->time = 0;
      if (info->self_time < 0)
	info->self_time = 0;
    }
  else
    info->entry = VEC_copy (size_time_entry, gc, info->entry);
}


/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
inline_edge_duplication_hook (struct cgraph_edge *src, struct cgraph_edge *dst,
			      ATTRIBUTE_UNUSED void *data)
{
  struct inline_edge_summary *info;
  struct inline_edge_summary *srcinfo;
  inline_summary_alloc ();
  info = inline_edge_summary (dst);
  srcinfo = inline_edge_summary (src);
  memcpy (info, srcinfo,
	  sizeof (struct inline_edge_summary));
  info->predicate = NULL;
  edge_set_predicate (dst, srcinfo->predicate);
}


/* Keep edge cache consistent across edge removal.  */

static void
inline_edge_removal_hook (struct cgraph_edge *edge, void *data ATTRIBUTE_UNUSED)
{
  if (edge_growth_cache)
    reset_edge_growth_cache (edge);
  if (edge->uid < (int)VEC_length (inline_edge_summary_t, inline_edge_summary_vec))
    {
      edge_set_predicate (edge, NULL);
      memset (inline_edge_summary (edge), 0, sizeof (struct inline_edge_summary));
    }
}


/* Initialize growth caches.  */

void
initialize_growth_caches (void)
{
  if (cgraph_edge_max_uid)
    VEC_safe_grow_cleared (edge_growth_cache_entry, heap, edge_growth_cache,
			   cgraph_edge_max_uid);
  if (cgraph_max_uid)
    VEC_safe_grow_cleared (int, heap, node_growth_cache, cgraph_max_uid);
}


/* Free growth caches.  */

void
free_growth_caches (void)
{
  VEC_free (edge_growth_cache_entry, heap, edge_growth_cache);
  edge_growth_cache = 0;
  VEC_free (int, heap, node_growth_cache);
  node_growth_cache = 0;
}


/* Dump edge summaries associated to NODE and recursively to all clones.
   Indent by INDENT.  */

static void
dump_inline_edge_summary (FILE * f, int indent, struct cgraph_node *node,
			  struct inline_summary *info)
{
  struct cgraph_edge *edge;
  for (edge = node->callees; edge; edge = edge->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (edge);
      fprintf (f, "%*s%s/%i %s\n%*s  loop depth:%2i freq:%4i size:%2i time: %2i callee size:%2i stack:%2i",
	       indent, "", cgraph_node_name (edge->callee),
	       edge->callee->uid, 
	       !edge->inline_failed ? "inlined"
	       : cgraph_inline_failed_string (edge->inline_failed),
	       indent, "",
	       es->loop_depth,	
               edge->frequency,
	       es->call_stmt_size,
	       es->call_stmt_time,
	       (int)inline_summary (edge->callee)->size,
	       (int)inline_summary (edge->callee)->estimated_stack_size);
      if (es->predicate)
	{
	  fprintf (f, " predicate: ");
	  dump_predicate (f, info->conds, es->predicate);
	}
      else
	  fprintf (f, "\n");
      if (!edge->inline_failed)
	{
          fprintf (f, "%*sStack frame offset %i, callee self size %i, callee size %i\n",
		   indent+2, "",
		   (int)inline_summary (edge->callee)->stack_frame_offset,
		   (int)inline_summary (edge->callee)->estimated_self_stack_size,
		   (int)inline_summary (edge->callee)->estimated_stack_size);
	  dump_inline_edge_summary (f, indent+2, edge->callee, info);
	}
    }
  for (edge = node->indirect_calls; edge; edge = edge->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (edge);
      fprintf (f, "%*sindirect call loop depth:%2i freq:%4i size:%2i time: %2i\n",
	       indent, "",
	       es->loop_depth,	
               edge->frequency,
	       es->call_stmt_size,
	       es->call_stmt_time);
      if (es->predicate)
	{
	  fprintf (f, "predicate: ");
	  dump_predicate (f, info->conds, es->predicate);
	}
      else
	  fprintf (f, "\n");
    }
}


void
dump_inline_summary (FILE * f, struct cgraph_node *node)
{
  if (node->analyzed)
    {
      struct inline_summary *s = inline_summary (node);
      size_time_entry *e;
      int i;
      fprintf (f, "Inline summary for %s/%i", cgraph_node_name (node),
	       node->uid);
      if (DECL_DISREGARD_INLINE_LIMITS (node->decl))
	fprintf (f, " always_inline");
      if (s->inlinable)
	fprintf (f, " inlinable");
      if (s->versionable)
	fprintf (f, " versionable");
      fprintf (f, "\n  self time:       %i\n",
	       s->self_time);
      fprintf (f, "  global time:     %i\n", s->time);
      fprintf (f, "  self size:       %i\n",
	       s->self_size);
      fprintf (f, "  global size:     %i\n", s->size);
      fprintf (f, "  self stack:      %i\n",
	       (int) s->estimated_self_stack_size);
      fprintf (f, "  global stack:    %i\n",
	       (int) s->estimated_stack_size);
      for (i = 0;
	   VEC_iterate (size_time_entry, s->entry, i, e);
	   i++)
	{
	  fprintf (f, "    size:%f, time:%f, predicate:",
		   (double) e->size / INLINE_SIZE_SCALE,
		   (double) e->time / INLINE_TIME_SCALE);
	  dump_predicate (f, s->conds, &e->predicate);
	}
      fprintf (f, "  calls:\n");
      dump_inline_edge_summary (f, 4, node, s);
      fprintf (f, "\n");
    }
}

DEBUG_FUNCTION void
debug_inline_summary (struct cgraph_node *node)
{
  dump_inline_summary (stderr, node);
}

void
dump_inline_summaries (FILE *f)
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed && !node->global.inlined_to)
      dump_inline_summary (f, node);
}

/* Give initial reasons why inlining would fail on EDGE.  This gets either
   nullified or usually overwritten by more precise reasons later.  */

void
initialize_inline_failed (struct cgraph_edge *e)
{
  struct cgraph_node *callee = e->callee;

  if (e->indirect_unknown_callee)
    e->inline_failed = CIF_INDIRECT_UNKNOWN_CALL;
  else if (!callee->analyzed)
    e->inline_failed = CIF_BODY_NOT_AVAILABLE;
  else if (callee->local.redefined_extern_inline)
    e->inline_failed = CIF_REDEFINED_EXTERN_INLINE;
  else if (e->call_stmt && gimple_call_cannot_inline_p (e->call_stmt))
    e->inline_failed = CIF_MISMATCHED_ARGUMENTS;
  else
    e->inline_failed = CIF_FUNCTION_NOT_CONSIDERED;
}

/* See if statement might disappear after inlining.
   0 - means not eliminated
   1 - half of statements goes away
   2 - for sure it is eliminated.
   We are not terribly sophisticated, basically looking for simple abstraction
   penalty wrappers.  */

static int
eliminated_by_inlining_prob (gimple stmt)
{
  enum gimple_code code = gimple_code (stmt);
  switch (code)
    {
      case GIMPLE_RETURN:
        return 2;
      case GIMPLE_ASSIGN:
	if (gimple_num_ops (stmt) != 2)
	  return 0;

	/* Casts of parameters, loads from parameters passed by reference
	   and stores to return value or parameters are often free after
	   inlining dua to SRA and further combining.
	   Assume that half of statements goes away.  */
	if (gimple_assign_rhs_code (stmt) == CONVERT_EXPR
	    || gimple_assign_rhs_code (stmt) == NOP_EXPR
	    || gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR
	    || gimple_assign_rhs_class (stmt) == GIMPLE_SINGLE_RHS)
	  {
	    tree rhs = gimple_assign_rhs1 (stmt);
            tree lhs = gimple_assign_lhs (stmt);
	    tree inner_rhs = rhs;
	    tree inner_lhs = lhs;
	    bool rhs_free = false;
	    bool lhs_free = false;

 	    while (handled_component_p (inner_lhs)
		   || TREE_CODE (inner_lhs) == MEM_REF)
	      inner_lhs = TREE_OPERAND (inner_lhs, 0);
 	    while (handled_component_p (inner_rhs)
	           || TREE_CODE (inner_rhs) == ADDR_EXPR
		   || TREE_CODE (inner_rhs) == MEM_REF)
	      inner_rhs = TREE_OPERAND (inner_rhs, 0);


	    if (TREE_CODE (inner_rhs) == PARM_DECL
	        || (TREE_CODE (inner_rhs) == SSA_NAME
		    && SSA_NAME_IS_DEFAULT_DEF (inner_rhs)
		    && TREE_CODE (SSA_NAME_VAR (inner_rhs)) == PARM_DECL))
	      rhs_free = true;
	    if (rhs_free && is_gimple_reg (lhs))
	      lhs_free = true;
	    if (((TREE_CODE (inner_lhs) == PARM_DECL
	          || (TREE_CODE (inner_lhs) == SSA_NAME
		      && SSA_NAME_IS_DEFAULT_DEF (inner_lhs)
		      && TREE_CODE (SSA_NAME_VAR (inner_lhs)) == PARM_DECL))
		 && inner_lhs != lhs)
	        || TREE_CODE (inner_lhs) == RESULT_DECL
	        || (TREE_CODE (inner_lhs) == SSA_NAME
		    && TREE_CODE (SSA_NAME_VAR (inner_lhs)) == RESULT_DECL))
	      lhs_free = true;
	    if (lhs_free
		&& (is_gimple_reg (rhs) || is_gimple_min_invariant (rhs)))
	      rhs_free = true;
	    if (lhs_free && rhs_free)
	      return 1;
	  }
	return 0;
      default:
	return 0;
    }
}


/* If BB ends by a conditional we can turn into predicates, attach corresponding
   predicates to the CFG edges.   */

static void
set_cond_stmt_execution_predicate (struct ipa_node_params *info,
			           struct inline_summary *summary,
			           basic_block bb)
{
  gimple last;
  tree op;
  int index;
  enum tree_code code, inverted_code;
  edge e;
  edge_iterator ei;
  gimple set_stmt;
  tree op2;

  last = last_stmt (bb);
  if (!last
      || gimple_code (last) != GIMPLE_COND)
    return;
  if (!is_gimple_ip_invariant (gimple_cond_rhs (last)))
    return;
  op = gimple_cond_lhs (last);
  /* TODO: handle conditionals like
     var = op0 < 4;
     if (var != 0).  */
  if (TREE_CODE (op) != SSA_NAME)
    return;
  if (SSA_NAME_IS_DEFAULT_DEF (op))
    {
      index = ipa_get_param_decl_index (info, SSA_NAME_VAR (op));
      if (index == -1)
	return;
      code = gimple_cond_code (last);
      inverted_code = invert_tree_comparison (code,
					      HONOR_NANS (TYPE_MODE (TREE_TYPE (op))));

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  struct predicate p = add_condition (summary,
					      index,
					      e->flags & EDGE_TRUE_VALUE
					      ? code : inverted_code,
					      gimple_cond_rhs (last));
	  e->aux = pool_alloc (edge_predicate_pool);
	  *(struct predicate *)e->aux = p;
	}
    }

  /* Special case
     if (builtin_constant_p (op))
       constant_code
     else
       nonconstant_code.
     Here we can predicate nonconstant_code.  We can't
     really handle constant_code since we have no predicate
     for this and also the constant code is not known to be
     optimized away when inliner doen't see operand is constant.
     Other optimizers might think otherwise.  */
  set_stmt = SSA_NAME_DEF_STMT (op);
  if (!gimple_call_builtin_p (set_stmt, BUILT_IN_CONSTANT_P)
      || gimple_call_num_args (set_stmt) != 1)
    return;
  op2 = gimple_call_arg (set_stmt, 0);
  if (!SSA_NAME_IS_DEFAULT_DEF (op2))
    return;
  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (op2));
  if (index == -1)
    return;
  if (gimple_cond_code (last) != NE_EXPR
      || !integer_zerop (gimple_cond_rhs (last)))
    return;
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_FALSE_VALUE)
      {
	struct predicate p = add_condition (summary,
					    index,
					    IS_NOT_CONSTANT,
					    NULL);
	e->aux = pool_alloc (edge_predicate_pool);
	*(struct predicate *)e->aux = p;
      }
}


/* If BB ends by a switch we can turn into predicates, attach corresponding
   predicates to the CFG edges.   */

static void
set_switch_stmt_execution_predicate (struct ipa_node_params *info,
			           struct inline_summary *summary,
			           basic_block bb)
{
  gimple last;
  tree op;
  int index;
  edge e;
  edge_iterator ei;
  size_t n;
  size_t case_idx;

  last = last_stmt (bb);
  if (!last
      || gimple_code (last) != GIMPLE_SWITCH)
    return;
  op = gimple_switch_index (last);
  if (TREE_CODE (op) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (op))
    return;
  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (op));
  if (index == -1)
    return;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      e->aux = pool_alloc (edge_predicate_pool);
      *(struct predicate *)e->aux = false_predicate ();
    }
  n = gimple_switch_num_labels(last);
  for (case_idx = 0; case_idx < n; ++case_idx)
    {
      tree cl = gimple_switch_label (last, case_idx);
      tree min, max;
      struct predicate p;

      e = find_edge (bb, label_to_block (CASE_LABEL (cl)));
      min = CASE_LOW (cl);
      max = CASE_HIGH (cl);

      /* For default we might want to construct predicate that none
	 of cases is met, but it is bit hard to do not having negations
	 of conditionals handy.  */
      if (!min && !max)
	p = true_predicate ();
      else if (!max)
	p = add_condition (summary, index,
			   EQ_EXPR,
			   min);
      else
	{
	  struct predicate p1, p2;
	  p1 = add_condition (summary, index,
			      GE_EXPR,
			      min);
	  p2 = add_condition (summary, index,
			      LE_EXPR,
			      max);
	  p = and_predicates (&p1, &p2);
	}
      *(struct predicate *)e->aux
	= or_predicates (&p, (struct predicate *)e->aux);
    }
}


/* For each BB in NODE attach to its AUX pointer predicate under
   which it is executable.  */

static void
compute_bb_predicates (struct cgraph_node *node,
		       struct ipa_node_params *parms_info,
		       struct inline_summary *summary)
{
  struct function *my_function = DECL_STRUCT_FUNCTION (node->decl);
  bool done = false;
  basic_block bb;

  FOR_EACH_BB_FN (bb, my_function)
    {
      set_cond_stmt_execution_predicate (parms_info, summary, bb);
      set_switch_stmt_execution_predicate (parms_info, summary, bb);
    }

  /* Entry block is always executable.  */
  ENTRY_BLOCK_PTR_FOR_FUNCTION (my_function)->aux = pool_alloc (edge_predicate_pool);
  *(struct predicate *)ENTRY_BLOCK_PTR_FOR_FUNCTION (my_function)->aux
    = true_predicate ();

  /* A simple dataflow propagation of predicates forward in the CFG.
     TODO: work in reverse postorder.  */
  while (!done)
    {
      done = true;
      FOR_EACH_BB_FN (bb, my_function)
	{
          struct predicate p = false_predicate ();
          edge e;
          edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      if (e->src->aux)
		{
		  struct predicate this_bb_predicate = *(struct predicate *)e->src->aux;
		  if (e->aux)
		    this_bb_predicate = and_predicates (&this_bb_predicate,
							(struct predicate *)e->aux);
		  p = or_predicates (&p, &this_bb_predicate);
		  if (true_predicate_p (&p))
		    break;
		}
	    }
	  if (false_predicate_p (&p))
	    gcc_assert (!bb->aux);
	  else
	    {
	      if (!bb->aux)
		{
		  done = false;
		  bb->aux = pool_alloc (edge_predicate_pool);
		  *((struct predicate *)bb->aux) = p;
		}
	      else if (!predicates_equal_p (&p, (struct predicate *)bb->aux))
		{
		  done = false;
		  *((struct predicate *)bb->aux) = p;
		}
	    }
	}
    }
}


/* We keep info about constantness of SSA names.  */

typedef struct predicate predicate_t;
DEF_VEC_O (predicate_t);
DEF_VEC_ALLOC_O (predicate_t, heap);


/* Return predicate specifying when the STMT might have result that is not a compile
   time constant.  */

static struct predicate
will_be_nonconstant_predicate (struct ipa_node_params *info,
			       struct inline_summary *summary,
			       gimple stmt,
			       VEC (predicate_t, heap) *nonconstant_names)
			      
{
  struct predicate p = true_predicate ();
  ssa_op_iter iter;
  tree use;
  struct predicate op_non_const;

  /* What statments might be optimized away
     when their arguments are constant
     TODO: also trivial builtins.
     builtin_constant_p is already handled later.  */
  if (gimple_code (stmt) != GIMPLE_ASSIGN
      && gimple_code (stmt) != GIMPLE_COND
      && gimple_code (stmt) != GIMPLE_SWITCH)
    return p;

  /* Stores and loads will stay anyway.
     TODO: Constant memory accesses could be handled here, too.  */
  if (gimple_vuse (stmt))
    return p;

  /* See if we understand all operands before we start
     adding conditionals.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      if (TREE_CODE (use) != SSA_NAME)
	return p;
      /* For arguments we can build a condition.  */
      if (SSA_NAME_IS_DEFAULT_DEF (use)
	  && ipa_get_param_decl_index (info, SSA_NAME_VAR (use)) >= 0)
	continue;
      /* If we know when operand is constant,
	 we still can say something useful.  */
      if (!true_predicate_p (VEC_index (predicate_t, nonconstant_names,
					SSA_NAME_VERSION (use))))
	continue;
      return p;
    }
  op_non_const = false_predicate ();
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (use)
	  && ipa_get_param_decl_index (info, SSA_NAME_VAR (use)) >= 0)
	p = add_condition (summary,
			   ipa_get_param_decl_index (info, SSA_NAME_VAR (use)),
			   IS_NOT_CONSTANT, NULL);
      else
	p = *VEC_index (predicate_t, nonconstant_names,
			SSA_NAME_VERSION (use));
      op_non_const = or_predicates (&p, &op_non_const);
    }
  if (gimple_code (stmt) == GIMPLE_ASSIGN
      && TREE_CODE (gimple_assign_lhs (stmt)) == SSA_NAME)
    VEC_replace (predicate_t, nonconstant_names,
		 SSA_NAME_VERSION (gimple_assign_lhs (stmt)), &op_non_const);
  return op_non_const;
}


/* Compute function body size parameters for NODE.
   When EARLY is true, we compute only simple summaries without
   non-trivial predicates to drive the early inliner.  */

static void
estimate_function_body_sizes (struct cgraph_node *node, bool early)
{
  gcov_type time = 0;
  /* Estimate static overhead for function prologue/epilogue and alignment. */
  int size = 2;
  /* Benefits are scaled by probability of elimination that is in range
     <0,2>.  */
  basic_block bb;
  gimple_stmt_iterator bsi;
  struct function *my_function = DECL_STRUCT_FUNCTION (node->decl);
  int freq;
  struct inline_summary *info = inline_summary (node);
  struct predicate bb_predicate;
  struct ipa_node_params *parms_info = NULL;
  VEC (predicate_t, heap) *nonconstant_names = NULL;

  if (ipa_node_params_vector && !early && optimize)
    {
      parms_info = IPA_NODE_REF (node);
      VEC_safe_grow_cleared (predicate_t, heap, nonconstant_names,
			     VEC_length (tree, SSANAMES (my_function)));
    }

  info->conds = 0;
  info->entry = 0;


  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function body size: %s\n",
	     cgraph_node_name (node));

  /* When we run into maximal number of entries, we assign everything to the
     constant truth case.  Be sure to have it in list. */
  bb_predicate = true_predicate ();
  account_size_time (info, 0, 0, &bb_predicate);

  bb_predicate = not_inlined_predicate ();
  account_size_time (info, 2 * INLINE_SIZE_SCALE, 0, &bb_predicate);

  gcc_assert (my_function && my_function->cfg);
  if (parms_info)
    compute_bb_predicates (node, parms_info, info);
  FOR_EACH_BB_FN (bb, my_function)
    {
      freq = compute_call_stmt_bb_frequency (node->decl, bb);

      /* TODO: Obviously predicates can be propagated down across CFG.  */
      if (parms_info)
	{
	  if (bb->aux)
	    bb_predicate = *(struct predicate *)bb->aux;
	  else
	    bb_predicate = false_predicate ();
	}
      else
	bb_predicate = true_predicate ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n BB %i predicate:", bb->index);
	  dump_predicate (dump_file, info->conds, &bb_predicate);
	}
      
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);
	  int this_size = estimate_num_insns (stmt, &eni_size_weights);
	  int this_time = estimate_num_insns (stmt, &eni_time_weights);
	  int prob;
	  struct predicate will_be_nonconstant;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  ");
	      print_gimple_stmt (dump_file, stmt, 0, 0);
	      fprintf (dump_file, "\t\tfreq:%3.2f size:%3i time:%3i\n",
		       ((double)freq)/CGRAPH_FREQ_BASE, this_size, this_time);
	    }

	  if (is_gimple_call (stmt))
	    {
	      struct cgraph_edge *edge = cgraph_edge (node, stmt);
	      struct inline_edge_summary *es = inline_edge_summary (edge);

	      /* Special case: results of BUILT_IN_CONSTANT_P will be always
		 resolved as constant.  We however don't want to optimize
		 out the cgraph edges.  */
	      if (nonconstant_names
		  && gimple_call_builtin_p (stmt, BUILT_IN_CONSTANT_P)
		  && gimple_call_lhs (stmt)
		  && TREE_CODE (gimple_call_lhs (stmt)) == SSA_NAME)
		{
		  struct predicate false_p = false_predicate ();
		  VEC_replace (predicate_t, nonconstant_names,
			       SSA_NAME_VERSION (gimple_call_lhs (stmt)), &false_p);
		}

	      es->call_stmt_size = this_size;
	      es->call_stmt_time = this_time;
	      es->loop_depth = bb->loop_depth;
	      edge_set_predicate (edge, &bb_predicate);

	      /* Do not inline calls where we cannot triviall work around
		 mismatches in argument or return types.  */
	      if (edge->callee
		  && !gimple_check_call_matching_types (stmt, edge->callee->decl))
		{
		  edge->call_stmt_cannot_inline_p = true;
		  gimple_call_set_cannot_inline (stmt, true);
		}
	      else
		gcc_assert (!gimple_call_cannot_inline_p (stmt));
	    }

	  /* TODO: When conditional jump or swithc is known to be constant, but
 	     we did not translate it into the predicates, we really can account
	     just maximum of the possible paths.  */
	  if (parms_info)
	    will_be_nonconstant
	       = will_be_nonconstant_predicate (parms_info, info,
						stmt, nonconstant_names);
	  if (this_time || this_size)
	    {
	      struct predicate p;

	      this_time *= freq;
	      time += this_time;
	      size += this_size;

	      prob = eliminated_by_inlining_prob (stmt);
	      if (prob == 1 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\t\t50%% will be eliminated by inlining\n");
	      if (prob == 2 && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\t\twill eliminated by inlining\n");

	      if (parms_info)
		p = and_predicates (&bb_predicate, &will_be_nonconstant);
	      else
		p = true_predicate ();

	      /* We account everything but the calls.  Calls have their own
		 size/time info attached to cgraph edges.  This is neccesary
		 in order to make the cost disappear after inlining.  */
	      if (!is_gimple_call (stmt))
		{
		  if (prob)
		    {
		      struct predicate ip = not_inlined_predicate ();
		      ip = and_predicates (&ip, &p);
		      account_size_time (info, this_size * prob,
					 this_time * prob, &ip);
		    }
		  if (prob != 2)
		    account_size_time (info, this_size * (2 - prob),
				       this_time * (2 - prob), &p);
		}

	      gcc_assert (time >= 0);
	      gcc_assert (size >= 0);
	    }
	}
    }
  FOR_ALL_BB_FN (bb, my_function)
    {
      edge e;
      edge_iterator ei;

      if (bb->aux)
	pool_free (edge_predicate_pool, bb->aux);
      bb->aux = NULL;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->aux)
	    pool_free (edge_predicate_pool, e->aux);
	  e->aux = NULL;
	}
    }
  time = (time + CGRAPH_FREQ_BASE / 2) / CGRAPH_FREQ_BASE;
  if (time > MAX_TIME)
    time = MAX_TIME;
  inline_summary (node)->self_time = time;
  inline_summary (node)->self_size = size;
  VEC_free (predicate_t, heap, nonconstant_names);
  if (dump_file)
    {
      fprintf (dump_file, "\n");
      dump_inline_summary (dump_file, node);
    }
}


/* Compute parameters of functions used by inliner.
   EARLY is true when we compute parameters for the early inliner  */

void
compute_inline_parameters (struct cgraph_node *node, bool early)
{
  HOST_WIDE_INT self_stack_size;
  struct cgraph_edge *e;
  struct inline_summary *info;

  gcc_assert (!node->global.inlined_to);

  inline_summary_alloc ();

  info = inline_summary (node);

  /* FIXME: Thunks are inlinable, but tree-inline don't know how to do that.
     Once this happen, we will need to more curefully predict call
     statement size.  */
  if (node->thunk.thunk_p)
    {
      struct inline_edge_summary *es = inline_edge_summary (node->callees);
      struct predicate t = true_predicate ();

      info->inlinable = info->versionable = 0;
      node->callees->call_stmt_cannot_inline_p = true;
      node->local.can_change_signature = false;
      es->call_stmt_time = 1;
      es->call_stmt_size = 1;
      account_size_time (info, 0, 0, &t);
      return;
    }

  /* Estimate the stack size for the function if we're optimizing.  */
  self_stack_size = optimize ? estimated_stack_frame_size (node) : 0;
  info->estimated_self_stack_size = self_stack_size;
  info->estimated_stack_size = self_stack_size;
  info->stack_frame_offset = 0;

  /* Can this function be inlined at all?  */
  info->inlinable = tree_inlinable_function_p (node->decl);

  /* Inlinable functions always can change signature.  */
  if (info->inlinable)
    node->local.can_change_signature = true;
  else
    {
      /* Functions calling builtin_apply can not change signature.  */
      for (e = node->callees; e; e = e->next_callee)
	if (DECL_BUILT_IN (e->callee->decl)
	    && DECL_BUILT_IN_CLASS (e->callee->decl) == BUILT_IN_NORMAL
	    && DECL_FUNCTION_CODE (e->callee->decl) == BUILT_IN_APPLY_ARGS)
	  break;
      node->local.can_change_signature = !e;
    }
  estimate_function_body_sizes (node, early);

  /* Inlining characteristics are maintained by the cgraph_mark_inline.  */
  info->time = info->self_time;
  info->size = info->self_size;
  info->stack_frame_offset = 0;
  info->estimated_stack_size = info->estimated_self_stack_size;
}


/* Compute parameters of functions used by inliner using
   current_function_decl.  */

static unsigned int
compute_inline_parameters_for_current (void)
{
  compute_inline_parameters (cgraph_get_node (current_function_decl), true);
  return 0;
}

struct gimple_opt_pass pass_inline_parameters =
{
 {
  GIMPLE_PASS,
  "inline_param",			/* name */
  NULL,					/* gate */
  compute_inline_parameters_for_current,/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_INLINE_HEURISTICS,			/* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};


/* Increase SIZE and TIME for size and time needed to handle edge E.  */

static void
estimate_edge_size_and_time (struct cgraph_edge *e, int *size, int *time)
{
  struct inline_edge_summary *es = inline_edge_summary (e);
  *size += es->call_stmt_size * INLINE_SIZE_SCALE;
  *time += (es->call_stmt_time
	    * e->frequency * (INLINE_TIME_SCALE / CGRAPH_FREQ_BASE));
  if (*time > MAX_TIME * INLINE_TIME_SCALE)
    *time = MAX_TIME * INLINE_TIME_SCALE;
}


/* Increase SIZE and TIME for size and time needed to handle all calls in NODE.  */

static void
estimate_calls_size_and_time (struct cgraph_node *node, int *size, int *time,
			      clause_t possible_truths)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (e);
      if (!es->predicate || evaluate_predicate (es->predicate, possible_truths))
	{
	  if (e->inline_failed)
	    estimate_edge_size_and_time (e, size, time);
	  else
	    estimate_calls_size_and_time (e->callee, size, time,
					  possible_truths);
	}
    }
  /* TODO: look for devirtualizing oppurtunities.  */
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (e);
      if (!es->predicate || evaluate_predicate (es->predicate, possible_truths))
        estimate_edge_size_and_time (e, size, time);
    }
}


/* Estimate size and time needed to execute NODE assuming
   POSSIBLE_TRUTHS clause. */

static void
estimate_node_size_and_time (struct cgraph_node *node,
			     clause_t possible_truths,
		       	     int *ret_size, int *ret_time)
{
  struct inline_summary *info = inline_summary (node);
  size_time_entry *e;
  int size = 0, time = 0;
  int i;

  if (dump_file
      && (dump_flags & TDF_DETAILS))
    {
      bool found = false;
      fprintf (dump_file, "   Estimating body: %s/%i\n"
			  "   Known to be false: ",
	       cgraph_node_name (node),
	       node->uid);

      for (i = predicate_not_inlined_condition;
	   i < (predicate_first_dynamic_condition
		+ (int)VEC_length (condition, info->conds)); i++)
	if (!(possible_truths & (1 << i)))
	  {
	    if (found)
	      fprintf (dump_file, ", ");
	    found = true;
            dump_condition (dump_file, info->conds, i);
	  }
    }

  for (i = 0; VEC_iterate (size_time_entry, info->entry, i, e); i++)
    if (evaluate_predicate (&e->predicate, possible_truths))
      time += e->time, size += e->size;

  if (time > MAX_TIME * INLINE_TIME_SCALE)
    time = MAX_TIME * INLINE_TIME_SCALE;

  estimate_calls_size_and_time (node, &size, &time, possible_truths);
  time = (time + INLINE_TIME_SCALE / 2) / INLINE_TIME_SCALE;
  size = (size + INLINE_SIZE_SCALE / 2) / INLINE_SIZE_SCALE;


  if (dump_file
      && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n   size:%i time:%i\n", size, time);
  if (ret_time)
    *ret_time = time;
  if (ret_size)
    *ret_size = size;
  return;
}


/* Estimate size and time needed to execute callee of EDGE assuming
   that parameters known to be constant at caller of EDGE are
   propagated.  If INLINE_P is true, it is assumed that call will
   be inlined.  */

void
estimate_ipcp_clone_size_and_time (struct cgraph_node *node,
		                   int *ret_size, int *ret_time)
{
  estimate_node_size_and_time (node,
			       evaluate_conditions_for_ipcp_clone (node),
			       ret_size, ret_time);
}


/* Translate all conditions from callee representation into caller representation and
   symbolically evaluate predicate P into new predicate.

   INFO is inline_summary of function we are adding predicate into, CALLEE_INFO is summary
   of function predicate P is from. OPERAND_MAP is array giving callee formal IDs the
   caller formal IDs. POSSSIBLE_TRUTHS is clausule of all callee conditions that
   may be true in caller context. TOPLEV_PREDICATE is predicate under which callee
   is executed.  */

static struct predicate
remap_predicate (struct inline_summary *info, struct inline_summary *callee_info,
		 struct predicate *p,
		 VEC (int, heap) *operand_map,
		 clause_t possible_truths,
		 struct predicate *toplev_predicate)
{
  int i;
  struct predicate out = true_predicate ();

  /* True predicate is easy.  */
  if (true_predicate_p (p))
    return *toplev_predicate;
  for (i = 0; p->clause[i]; i++)
    {
      clause_t clause = p->clause[i];
      int cond;
      struct predicate clause_predicate = false_predicate ();

      gcc_assert (i < MAX_CLAUSES);

      for (cond = 0; cond < NUM_CONDITIONS; cond ++)
	/* Do we have condition we can't disprove?   */
	if (clause & possible_truths & (1 << cond))
	  {
	    struct predicate cond_predicate;
	    /* Work out if the condition can translate to predicate in the
	       inlined function.  */
	    if (cond >= predicate_first_dynamic_condition)
	      {
		 struct condition *c;

		 c = VEC_index (condition, callee_info->conds,
				cond - predicate_first_dynamic_condition);
		 /* See if we can remap condition operand to caller's operand.
		    Otherwise give up.  */
		 if (!operand_map
		     || VEC_index (int, operand_map, c->operand_num) == -1)
		   cond_predicate = true_predicate ();
		 else
		   cond_predicate = add_condition (info,
						   VEC_index (int, operand_map,
							      c->operand_num),
						   c->code, c->val);
	      }
	    /* Fixed conditions remains same, construct single
	       condition predicate.  */
	    else
	      {
		cond_predicate.clause[0] = 1 << cond;
		cond_predicate.clause[1] = 0;
	      }
	    clause_predicate = or_predicates (&clause_predicate, &cond_predicate);
	  }
      out = and_predicates (&out, &clause_predicate);
    }
  return and_predicates (&out, toplev_predicate);
}


/* Update summary information of inline clones after inlining.
   Compute peak stack usage.  */

static void
inline_update_callee_summaries (struct cgraph_node *node,
			        int depth)
{
  struct cgraph_edge *e;
  struct inline_summary *callee_info = inline_summary (node);
  struct inline_summary *caller_info = inline_summary (node->callers->caller);
  HOST_WIDE_INT peak;

  callee_info->stack_frame_offset
    = caller_info->stack_frame_offset
      + caller_info->estimated_self_stack_size;
  peak = callee_info->stack_frame_offset
      + callee_info->estimated_self_stack_size;
  if (inline_summary (node->global.inlined_to)->estimated_stack_size
      < peak)
    inline_summary (node->global.inlined_to)->estimated_stack_size = peak;
  cgraph_propagate_frequency (node);
  for (e = node->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	inline_update_callee_summaries (e->callee, depth);
      inline_edge_summary (e)->loop_depth += depth;
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    inline_edge_summary (e)->loop_depth += depth;
}


/* Remap predicates of callees of NODE.  Rest of arguments match
   remap_predicate.  */

static void
remap_edge_predicates (struct cgraph_node *node,
		       struct inline_summary *info,
		       struct inline_summary *callee_info,
		       VEC (int, heap) *operand_map,
		       clause_t possible_truths,
		       struct predicate *toplev_predicate)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (e);
      struct predicate p;
      if (es->predicate)
	{
	  p = remap_predicate (info, callee_info,
			       es->predicate, operand_map, possible_truths,
			       toplev_predicate);
	  edge_set_predicate (e, &p);
	  /* TODO: We should remove the edge for code that will be optimized out,
	     but we need to keep verifiers and tree-inline happy.
	     Make it cold for now.  */
	  if (false_predicate_p (&p))
	    {
	      e->count = 0;
	      e->frequency = 0;
	    }
	}
      if (!e->inline_failed)
	remap_edge_predicates (e->callee, info, callee_info, operand_map,
			       possible_truths, toplev_predicate);
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      struct inline_edge_summary *es = inline_edge_summary (e);
      struct predicate p;
      if (es->predicate)
	{
	  p = remap_predicate (info, callee_info,
			       es->predicate, operand_map, possible_truths,
			       toplev_predicate);
	  edge_set_predicate (e, &p);
	  /* TODO: We should remove the edge for code that will be optimized out,
	     but we need to keep verifiers and tree-inline happy.
	     Make it cold for now.  */
	  if (false_predicate_p (&p))
	    {
	      e->count = 0;
	      e->frequency = 0;
	    }
	}
    }
}


/* We inlined EDGE.  Update summary of the function we inlined into.  */

void
inline_merge_summary (struct cgraph_edge *edge)
{
  struct inline_summary *callee_info = inline_summary (edge->callee);
  struct cgraph_node *to = (edge->caller->global.inlined_to
			    ? edge->caller->global.inlined_to : edge->caller);
  struct inline_summary *info = inline_summary (to);
  clause_t clause = 0;		/* not_inline is known to be false.  */
  size_time_entry *e;
  VEC (int, heap) *operand_map = NULL;
  int i;
  struct predicate toplev_predicate;
  struct inline_edge_summary *es = inline_edge_summary (edge);

  if (es->predicate)
    toplev_predicate = *es->predicate;
  else
    toplev_predicate = true_predicate ();

  if (ipa_node_params_vector && callee_info->conds
      /* FIXME: it seems that we forget to get argument count in some cases,
	 probaby for previously indirect edges or so.
	 Removing the test leads to ICE on tramp3d.  */
      && ipa_get_cs_argument_count (IPA_EDGE_REF (edge)))
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (edge);
      int count = ipa_get_cs_argument_count (args);
      int i;

      clause = evaluate_conditions_for_edge (edge, true);
      VEC_safe_grow_cleared (int, heap, operand_map, count);
      for (i = 0; i < count; i++)
	{
	  struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, i);
	  int map = -1;
	  /* TODO: handle non-NOPs when merging.  */
	  if (jfunc->type == IPA_JF_PASS_THROUGH
	      && jfunc->value.pass_through.operation == NOP_EXPR)
	    map = jfunc->value.pass_through.formal_id;
	  VEC_replace (int, operand_map, i, map);
	  gcc_assert (map < ipa_get_param_count (IPA_NODE_REF (to)));
	}
    }
  for (i = 0; VEC_iterate (size_time_entry, callee_info->entry, i, e); i++)
    {
      struct predicate p = remap_predicate (info, callee_info,
					    &e->predicate, operand_map, clause,
					    &toplev_predicate);
      gcov_type add_time = ((gcov_type)e->time * edge->frequency
			    + CGRAPH_FREQ_BASE / 2) / CGRAPH_FREQ_BASE;
      if (add_time > MAX_TIME)
	add_time = MAX_TIME;
      account_size_time (info, e->size, add_time, &p);
    }
  remap_edge_predicates (edge->callee, info, callee_info, operand_map,
			 clause, &toplev_predicate);
  info->size = 0;
  info->time = 0;
  for (i = 0; VEC_iterate (size_time_entry, info->entry, i, e); i++)
    info->size += e->size, info->time += e->time;
  estimate_calls_size_and_time (to, &info->size, &info->time,
				~(clause_t)(1 << predicate_false_condition));

  inline_update_callee_summaries (edge->callee,
				  inline_edge_summary (edge)->loop_depth);

  info->time = (info->time + INLINE_TIME_SCALE / 2) / INLINE_TIME_SCALE;
  info->size = (info->size + INLINE_SIZE_SCALE / 2) / INLINE_SIZE_SCALE;
}


/* Estimate the time cost for the caller when inlining EDGE.
   Only to be called via estimate_edge_time, that handles the
   caching mechanism.

   When caching, also update the cache entry.  Compute both time and
   size, since we always need both metrics eventually.  */

int
do_estimate_edge_time (struct cgraph_edge *edge)
{
  int time;
  int size;
  gcov_type ret;
  struct inline_edge_summary *es = inline_edge_summary (edge);

  gcc_checking_assert (edge->inline_failed);
  estimate_node_size_and_time (edge->callee,
			       evaluate_conditions_for_edge (edge, true),
			       &size, &time);

  ret = (((gcov_type)time - es->call_stmt_time) * edge->frequency
	 + CGRAPH_FREQ_BASE / 2) / CGRAPH_FREQ_BASE;
  if (ret > MAX_TIME)
    ret = MAX_TIME;

  /* When caching, update the cache entry.  */
  if (edge_growth_cache)
    {
      int ret_size;
      if ((int)VEC_length (edge_growth_cache_entry, edge_growth_cache)
	  <= edge->uid)
	VEC_safe_grow_cleared (edge_growth_cache_entry, heap, edge_growth_cache,
			       cgraph_edge_max_uid);
      VEC_index (edge_growth_cache_entry, edge_growth_cache, edge->uid)->time
	= ret + (ret >= 0);

      ret_size = size - es->call_stmt_size;
      gcc_checking_assert (es->call_stmt_size);
      VEC_index (edge_growth_cache_entry, edge_growth_cache, edge->uid)->size
	= ret_size + (ret_size >= 0);
    }
  return ret;
}


/* Estimate the growth of the caller when inlining EDGE.
   Only to be called via estimate_edge_size.  */

int
do_estimate_edge_growth (struct cgraph_edge *edge)
{
  int size;

  /* When we do caching, use do_estimate_edge_time to populate the entry.  */

  if (edge_growth_cache)
    {
      do_estimate_edge_time (edge);
      size = VEC_index (edge_growth_cache_entry,
			edge_growth_cache,
			edge->uid)->size;
      gcc_checking_assert (size);
      return size - (size > 0);
    }

  /* Early inliner runs without caching, go ahead and do the dirty work.  */
  gcc_checking_assert (edge->inline_failed);
  estimate_node_size_and_time (edge->callee,
			       evaluate_conditions_for_edge (edge, true),
			       &size, NULL);
  gcc_checking_assert (inline_edge_summary (edge)->call_stmt_size);
  return size - inline_edge_summary (edge)->call_stmt_size;
}


/* Estimate self time of the function NODE after inlining EDGE.  */

int
estimate_time_after_inlining (struct cgraph_node *node,
			      struct cgraph_edge *edge)
{
  struct inline_edge_summary *es = inline_edge_summary (edge);
  if (!es->predicate || !false_predicate_p (es->predicate))
    {
      gcov_type time = inline_summary (node)->time + estimate_edge_time (edge);
      if (time < 0)
	time = 0;
      if (time > MAX_TIME)
	time = MAX_TIME;
      return time;
    }
  return inline_summary (node)->time;
}


/* Estimate the size of NODE after inlining EDGE which should be an
   edge to either NODE or a call inlined into NODE.  */

int
estimate_size_after_inlining (struct cgraph_node *node,
			      struct cgraph_edge *edge)
{
  struct inline_edge_summary *es = inline_edge_summary (edge);
  if (!es->predicate || !false_predicate_p (es->predicate))
    {
      int size = inline_summary (node)->size + estimate_edge_growth (edge);
      gcc_assert (size >= 0);
      return size;
    }
  return inline_summary (node)->size;
}


/* Estimate the growth caused by inlining NODE into all callees.  */

int
do_estimate_growth (struct cgraph_node *node)
{
  int growth = 0;
  struct cgraph_edge *e;
  bool self_recursive = false;
  struct inline_summary *info = inline_summary (node);

  for (e = node->callers; e; e = e->next_caller)
    {
      gcc_checking_assert (e->inline_failed);

      if (e->caller == node
	  || (e->caller->global.inlined_to
	      && e->caller->global.inlined_to == node))
        self_recursive = true;
      growth += estimate_edge_growth (e);
    }
     

  /* For self recursive functions the growth estimation really should be
     infinity.  We don't want to return very large values because the growth
     plays various roles in badness computation fractions.  Be sure to not
     return zero or negative growths. */
  if (self_recursive)
    growth = growth < info->size ? info->size : growth;
  else
    {
      if (cgraph_will_be_removed_from_program_if_no_direct_calls (node)
	  && !DECL_EXTERNAL (node->decl))
	growth -= info->size;
      /* COMDAT functions are very often not shared across multiple units since they
	 come from various template instantiations.  Take this into account.  */
      else  if (DECL_COMDAT (node->decl)
		&& cgraph_can_remove_if_no_direct_calls_p (node))
	growth -= (info->size
		   * (100 - PARAM_VALUE (PARAM_COMDAT_SHARING_PROBABILITY)) + 50) / 100;
    }

  if (node_growth_cache)
    {
      if ((int)VEC_length (int, node_growth_cache) <= node->uid)
	VEC_safe_grow_cleared (int, heap, node_growth_cache, cgraph_max_uid);
      VEC_replace (int, node_growth_cache, node->uid, growth + (growth >= 0));
    }
  return growth;
}


/* This function performs intraprocedural analysis in NODE that is required to
   inline indirect calls.  */

static void
inline_indirect_intraprocedural_analysis (struct cgraph_node *node)
{
  ipa_analyze_node (node);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      ipa_print_node_params (dump_file, node);
      ipa_print_node_jump_functions (dump_file, node);
    }
}


/* Note function body size.  */

static void
inline_analyze_function (struct cgraph_node *node)
{
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  current_function_decl = node->decl;

  if (dump_file)
    fprintf (dump_file, "\nAnalyzing function: %s/%u\n",
	     cgraph_node_name (node), node->uid);
  /* FIXME: We should remove the optimize check after we ensure we never run
     IPA passes when not optimizing.  */
  if (flag_indirect_inlining && optimize && !node->thunk.thunk_p)
    inline_indirect_intraprocedural_analysis (node);
  compute_inline_parameters (node, false);

  current_function_decl = NULL;
  pop_cfun ();
}


/* Called when new function is inserted to callgraph late.  */

static void
add_new_function (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  inline_analyze_function (node);
}


/* Note function body size.  */

void
inline_generate_summary (void)
{
  struct cgraph_node *node;

  function_insertion_hook_holder =
      cgraph_add_function_insertion_hook (&add_new_function, NULL);

  if (flag_indirect_inlining)
    ipa_register_cgraph_hooks ();

  FOR_EACH_DEFINED_FUNCTION (node)
      inline_analyze_function (node);
}


/* Read predicate from IB.  */

static struct predicate
read_predicate (struct lto_input_block *ib)
{
  struct predicate out;
  clause_t clause;
  int k = 0;

  do 
    {
      gcc_assert (k <= MAX_CLAUSES);
      clause = out.clause[k++] = lto_input_uleb128 (ib);
    }
  while (clause);
  return out;
}


/* Write inline summary for edge E to OB.  */

static void
read_inline_edge_summary (struct lto_input_block *ib, struct cgraph_edge *e)
{
  struct inline_edge_summary *es = inline_edge_summary (e);
  struct predicate p;

  es->call_stmt_size = lto_input_uleb128 (ib);
  es->call_stmt_time = lto_input_uleb128 (ib);
  es->loop_depth = lto_input_uleb128 (ib);
  p = read_predicate (ib);
  edge_set_predicate (e, &p);
}


/* Stream in inline summaries from the section.  */

static void
inline_read_section (struct lto_file_decl_data *file_data, const char *data,
		     size_t len)
{
  const struct lto_function_header *header =
    (const struct lto_function_header *) data;
  const int32_t cfg_offset = sizeof (struct lto_function_header);
  const int32_t main_offset = cfg_offset + header->cfg_size;
  const int32_t string_offset = main_offset + header->main_size;
  struct data_in *data_in;
  struct lto_input_block ib;
  unsigned int i, count2, j;
  unsigned int f_count;

  LTO_INIT_INPUT_BLOCK (ib, (const char *) data + main_offset, 0,
			header->main_size);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, NULL);
  f_count = lto_input_uleb128 (&ib);
  for (i = 0; i < f_count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      struct inline_summary *info;
      lto_cgraph_encoder_t encoder;
      struct bitpack_d bp;
      struct cgraph_edge *e;

      index = lto_input_uleb128 (&ib);
      encoder = file_data->cgraph_node_encoder;
      node = lto_cgraph_encoder_deref (encoder, index);
      info = inline_summary (node);

      info->estimated_stack_size
	= info->estimated_self_stack_size = lto_input_uleb128 (&ib);
      info->size = info->self_size = lto_input_uleb128 (&ib);
      info->time = info->self_time = lto_input_uleb128 (&ib);

      bp = lto_input_bitpack (&ib);
      info->inlinable = bp_unpack_value (&bp, 1);
      info->versionable = bp_unpack_value (&bp, 1);

      count2 = lto_input_uleb128 (&ib);
      gcc_assert (!info->conds);
      for (j = 0; j < count2; j++)
	{
	  struct condition c;
	  c.operand_num = lto_input_uleb128 (&ib);
	  c.code = (enum tree_code) lto_input_uleb128 (&ib);
	  c.val = lto_input_tree (&ib, data_in);
	  VEC_safe_push (condition, gc, info->conds, &c);
	}
      count2 = lto_input_uleb128 (&ib);
      gcc_assert (!info->entry);
      for (j = 0; j < count2; j++)
	{
	  struct size_time_entry e;

	  e.size = lto_input_uleb128 (&ib);
	  e.time = lto_input_uleb128 (&ib);
	  e.predicate = read_predicate (&ib);

	  VEC_safe_push (size_time_entry, gc, info->entry, &e);
	}
      for (e = node->callees; e; e = e->next_callee)
	read_inline_edge_summary (&ib, e);
      for (e = node->indirect_calls; e; e = e->next_callee)
	read_inline_edge_summary (&ib, e);
    }

  lto_free_section_data (file_data, LTO_section_inline_summary, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}


/* Read inline summary.  Jump functions are shared among ipa-cp
   and inliner, so when ipa-cp is active, we don't need to write them
   twice.  */

void
inline_read_summary (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  inline_summary_alloc ();

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data = lto_get_section_data (file_data, LTO_section_inline_summary, NULL, &len);
      if (data)
        inline_read_section (file_data, data, len);
      else
	/* Fatal error here.  We do not want to support compiling ltrans units with
	   different version of compiler or different flags than the WPA unit, so
	   this should never happen.  */
	fatal_error ("ipa inline summary is missing in input file");
    }
  if (flag_indirect_inlining)
    {
      ipa_register_cgraph_hooks ();
      if (!flag_ipa_cp)
        ipa_prop_read_jump_functions ();
    }
  function_insertion_hook_holder =
      cgraph_add_function_insertion_hook (&add_new_function, NULL);
}


/* Write predicate P to OB.  */

static void
write_predicate (struct output_block *ob, struct predicate *p)
{
  int j;
  if (p)
    for (j = 0; p->clause[j]; j++)
      {
	 gcc_assert (j < MAX_CLAUSES);
	 lto_output_uleb128_stream (ob->main_stream,
				    p->clause[j]);
      }
  lto_output_uleb128_stream (ob->main_stream, 0);
}


/* Write inline summary for edge E to OB.  */

static void
write_inline_edge_summary (struct output_block *ob, struct cgraph_edge *e)
{
  struct inline_edge_summary *es = inline_edge_summary (e);
  lto_output_uleb128_stream (ob->main_stream, es->call_stmt_size);
  lto_output_uleb128_stream (ob->main_stream, es->call_stmt_time);
  lto_output_uleb128_stream (ob->main_stream, es->loop_depth);
  write_predicate (ob, es->predicate);
}


/* Write inline summary for node in SET.
   Jump functions are shared among ipa-cp and inliner, so when ipa-cp is
   active, we don't need to write them twice.  */

void
inline_write_summary (cgraph_node_set set,
		      varpool_node_set vset ATTRIBUTE_UNUSED)
{
  struct cgraph_node *node;
  struct output_block *ob = create_output_block (LTO_section_inline_summary);
  lto_cgraph_encoder_t encoder = ob->decl_state->cgraph_node_encoder;
  unsigned int count = 0;
  int i;

  for (i = 0; i < lto_cgraph_encoder_size (encoder); i++)
    if (lto_cgraph_encoder_deref (encoder, i)->analyzed)
      count++;
  lto_output_uleb128_stream (ob->main_stream, count);

  for (i = 0; i < lto_cgraph_encoder_size (encoder); i++)
    {
      node = lto_cgraph_encoder_deref (encoder, i);
      if (node->analyzed)
	{
	  struct inline_summary *info = inline_summary (node);
	  struct bitpack_d bp;
	  struct cgraph_edge *edge;
	  int i;
	  size_time_entry *e;
	  struct condition *c;
	  

	  lto_output_uleb128_stream (ob->main_stream,
				     lto_cgraph_encoder_encode (encoder, node));
	  lto_output_sleb128_stream (ob->main_stream,
				     info->estimated_self_stack_size);
	  lto_output_sleb128_stream (ob->main_stream,
				     info->self_size);
	  lto_output_sleb128_stream (ob->main_stream,
				     info->self_time);
	  bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, info->inlinable, 1);
	  bp_pack_value (&bp, info->versionable, 1);
	  lto_output_bitpack (&bp);
	  lto_output_uleb128_stream (ob->main_stream,
				     VEC_length (condition, info->conds));
	  for (i = 0; VEC_iterate (condition, info->conds, i, c); i++)
	    {
	      lto_output_uleb128_stream (ob->main_stream,
					 c->operand_num);
	      lto_output_uleb128_stream (ob->main_stream,
					 c->code);
	      lto_output_tree (ob, c->val, true);
	    }
	  lto_output_uleb128_stream (ob->main_stream,
				     VEC_length (size_time_entry, info->entry));
	  for (i = 0;
	       VEC_iterate (size_time_entry, info->entry, i, e);
	       i++)
	    {
	      lto_output_uleb128_stream (ob->main_stream,
					 e->size);
	      lto_output_uleb128_stream (ob->main_stream,
					 e->time);
	      write_predicate (ob, &e->predicate);
	    }
	  for (edge = node->callees; edge; edge = edge->next_callee)
	    write_inline_edge_summary (ob, edge);
	  for (edge = node->indirect_calls; edge; edge = edge->next_callee)
	    write_inline_edge_summary (ob, edge);
	}
    }
  lto_output_1_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);

  if (flag_indirect_inlining && !flag_ipa_cp)
    ipa_prop_write_jump_functions (set);
}


/* Release inline summary.  */

void
inline_free_summary (void)
{
  if (function_insertion_hook_holder)
    cgraph_remove_function_insertion_hook (function_insertion_hook_holder);
  function_insertion_hook_holder = NULL;
  if (node_removal_hook_holder)
    cgraph_remove_node_removal_hook (node_removal_hook_holder);
  if (edge_removal_hook_holder)
    cgraph_remove_edge_removal_hook (edge_removal_hook_holder);
  node_removal_hook_holder = NULL;
  if (node_duplication_hook_holder)
    cgraph_remove_node_duplication_hook (node_duplication_hook_holder);
  if (edge_duplication_hook_holder)
    cgraph_remove_edge_duplication_hook (edge_duplication_hook_holder);
  node_duplication_hook_holder = NULL;
  VEC_free (inline_summary_t, gc, inline_summary_vec);
  inline_summary_vec = NULL;
  VEC_free (inline_edge_summary_t, heap, inline_edge_summary_vec);
  inline_edge_summary_vec = NULL;
  if (edge_predicate_pool)
    free_alloc_pool (edge_predicate_pool);
  edge_predicate_pool = 0;
}
