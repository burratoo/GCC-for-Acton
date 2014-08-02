/* Callgraph handling code.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "varasm.h"
#include "cgraph.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "hashtab.h"
#include "timevar.h"
#include "debug.h"
#include "target.h"
#include "output.h"
#include "gimple-expr.h"
#include "flags.h"
#include "pointer-set.h"
#include "tree-ssa-alias.h"
#include "gimple.h"
#include "lto-streamer.h"

const char * const tls_model_names[]={"none", "tls-emulated", "tls-real",
				      "tls-global-dynamic", "tls-local-dynamic",
				      "tls-initial-exec", "tls-local-exec"};

/* List of hooks triggered on varpool_node events.  */
struct varpool_node_hook_list {
  varpool_node_hook hook;
  void *data;
  struct varpool_node_hook_list *next;
};

/* List of hooks triggered when a node is removed.  */
struct varpool_node_hook_list *first_varpool_node_removal_hook;
/* List of hooks triggered when an variable is inserted.  */
struct varpool_node_hook_list *first_varpool_variable_insertion_hook;

/* Register HOOK to be called with DATA on each removed node.  */
struct varpool_node_hook_list *
varpool_add_node_removal_hook (varpool_node_hook hook, void *data)
{
  struct varpool_node_hook_list *entry;
  struct varpool_node_hook_list **ptr = &first_varpool_node_removal_hook;

  entry = (struct varpool_node_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on removing nodes.  */
void
varpool_remove_node_removal_hook (struct varpool_node_hook_list *entry)
{
  struct varpool_node_hook_list **ptr = &first_varpool_node_removal_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node removal hooks.  */
static void
varpool_call_node_removal_hooks (varpool_node *node)
{
  struct varpool_node_hook_list *entry = first_varpool_node_removal_hook;
  while (entry)
  {
    entry->hook (node, entry->data);
    entry = entry->next;
  }
}

/* Register HOOK to be called with DATA on each inserted node.  */
struct varpool_node_hook_list *
varpool_add_variable_insertion_hook (varpool_node_hook hook, void *data)
{
  struct varpool_node_hook_list *entry;
  struct varpool_node_hook_list **ptr = &first_varpool_variable_insertion_hook;

  entry = (struct varpool_node_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on inserted nodes.  */
void
varpool_remove_variable_insertion_hook (struct varpool_node_hook_list *entry)
{
  struct varpool_node_hook_list **ptr = &first_varpool_variable_insertion_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node insertion hooks.  */
void
varpool_call_variable_insertion_hooks (varpool_node *node)
{
  struct varpool_node_hook_list *entry = first_varpool_variable_insertion_hook;
  while (entry)
  {
    entry->hook (node, entry->data);
    entry = entry->next;
  }
}

/* Allocate new callgraph node and insert it into basic data structures.  */

varpool_node *
varpool_node::create_empty (void)
{   
  varpool_node *node = ggc_cleared_alloc<varpool_node> ();
  node->type = SYMTAB_VARIABLE;
  return node;
}   

/* Return varpool node assigned to DECL.  Create new one when needed.  */
varpool_node *
varpool_node::get_create (tree decl)
{
  varpool_node *node = varpool_node::get (decl);
  gcc_checking_assert (TREE_CODE (decl) == VAR_DECL);
  if (node)
    return node;

  node = varpool_node::create_empty ();
  node->decl = decl;
  node->register_symbol ();
  return node;
}

/* Remove variable from symbol table.  */

void
varpool_node::remove (void)
{
  varpool_call_node_removal_hooks (this);
  unregister ();

  /* When streaming we can have multiple nodes associated with decl.  */
  if (cgraph_state == CGRAPH_LTO_STREAMING)
    ;
  /* Keep constructor when it may be used for folding. We remove
     references to external variables before final compilation.  */
  else if (DECL_INITIAL (decl) && DECL_INITIAL (decl) != error_mark_node
	   && !ctor_useable_for_folding_p ())
    remove_initializer ();
  ggc_free (this);
}

/* Remove node initializer when it is no longer needed.  */
void
varpool_node::remove_initializer (void)
{
  if (DECL_INITIAL (decl)
      && !DECL_IN_CONSTANT_POOL (decl)
      /* Keep vtables for BINFO folding.  */
      && !DECL_VIRTUAL_P (decl)
      /* FIXME: http://gcc.gnu.org/PR55395 */
      && debug_info_level == DINFO_LEVEL_NONE
      /* When doing declaration merging we have duplicate
	 entries for given decl.  Do not attempt to remove
	 the boides, or we will end up remiving
	 wrong one.  */
      && cgraph_state != CGRAPH_LTO_STREAMING)
    DECL_INITIAL (decl) = error_mark_node;
}

/* Dump given varpool node to F.  */
void
varpool_node::dump (FILE *f)
{
  dump_base (f);
  fprintf (f, "  Availability: %s\n",
	   cgraph_function_flags_ready
	   ? cgraph_availability_names[get_availability ()]
	   : "not-ready");
  fprintf (f, "  Varpool flags:");
  if (DECL_INITIAL (decl))
    fprintf (f, " initialized");
  if (output)
    fprintf (f, " output");
  if (used_by_single_function)
    fprintf (f, " used-by-single-function");
  if (TREE_READONLY (decl))
    fprintf (f, " read-only");
  if (ctor_useable_for_folding_p ())
    fprintf (f, " const-value-known");
  if (writeonly)
    fprintf (f, " write-only");
  if (tls_model)
    fprintf (f, " %s", tls_model_names [tls_model]);
  fprintf (f, "\n");
}


/* Dump given varpool node to stderr.  */
void varpool_node::debug (void)
{
  varpool_node::dump (stderr);
}

/* Dump the variable pool to F.  */
void
varpool_node::dump_varpool (FILE *f)
{
  varpool_node *node;

  fprintf (f, "variable pool:\n\n");
  FOR_EACH_VARIABLE (node)
    node->dump (f);
}

/* Dump the variable pool to stderr.  */

DEBUG_FUNCTION void
varpool_node::debug_varpool (void)
{
  dump_varpool (stderr);
}

/* Given an assembler name, lookup node.  */
varpool_node *
varpool_node::get_for_asmname (tree asmname)
{
  if (symtab_node *node = symtab_node_for_asm (asmname))
    return dyn_cast <varpool_node *> (node);
  else
    return NULL;
}

/* When doing LTO, read variable's constructor from disk if
   it is not already present.  */

tree
varpool_node::get_constructor (void)
{
  struct lto_file_decl_data *file_data;
  const char *data, *name;
  size_t len;

  if (DECL_INITIAL (decl) != error_mark_node
      || !in_lto_p)
    return DECL_INITIAL (decl);

  timevar_push (TV_IPA_LTO_CTORS_IN);

  file_data = lto_file_data;
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  /* We may have renamed the declaration, e.g., a static function.  */
  name = lto_get_decl_name_mapping (file_data, name);

  data = lto_get_section_data (file_data, LTO_section_function_body,
			       name, &len);
  if (!data)
    fatal_error ("%s: section %s is missing",
		 file_data->file_name,
		 name);

  lto_input_variable_constructor (file_data, this, data);
  lto_stats.num_function_bodies++;
  lto_free_section_data (file_data, LTO_section_function_body, name,
			 data, len);
  lto_free_function_in_decl_state_for_node (this);
  timevar_pop (TV_IPA_LTO_CTORS_IN);
  return DECL_INITIAL (decl);
}

/* Return true if variable has constructor that can be used for folding.  */

bool
varpool_node::ctor_useable_for_folding_p (void)
{
  varpool_node *real_node = this;

  if (real_node->alias && real_node->definition)
    real_node = ultimate_alias_target ();

  if (TREE_CODE (decl) == CONST_DECL
      || DECL_IN_CONSTANT_POOL (decl))
    return true;
  if (TREE_THIS_VOLATILE (decl))
    return false;

  /* If we do not have a constructor, we can't use it.  */
  if (DECL_INITIAL (real_node->decl) == error_mark_node
      && !real_node->lto_file_data)
    return false;

  /* Vtables are defined by their types and must match no matter of interposition
     rules.  */
  if (DECL_VIRTUAL_P (decl))
    {
      /* The C++ front end creates VAR_DECLs for vtables of typeinfo
	 classes not defined in the current TU so that it can refer
	 to them from typeinfo objects.  Avoid returning NULL_TREE.  */
      return DECL_INITIAL (real_node->decl) != NULL;
    }

  /* Alias of readonly variable is also readonly, since the variable is stored
     in readonly memory.  We also accept readonly aliases of non-readonly
     locations assuming that user knows what he is asking for.  */
  if (!TREE_READONLY (decl) && !TREE_READONLY (real_node->decl))
    return false;

  /* Variables declared 'const' without an initializer
     have zero as the initializer if they may not be
     overridden at link or run time.  */
  if (!DECL_INITIAL (real_node->decl)
      && (DECL_EXTERNAL (decl) || decl_replaceable_p (decl)))
    return false;

  /* Variables declared `const' with an initializer are considered
     to not be overwritable with different initializer by default. 

     ??? Previously we behaved so for scalar variables but not for array
     accesses.  */
  return true;
}

/* If DECLARATION is constant variable and its initial value is known
   (so we can do constant folding), return its constructor (DECL_INITIAL).
   This may be an expression or NULL when DECL is initialized to 0.
   Return ERROR_MARK_NODE otherwise.

   In LTO this may actually trigger reading the constructor from disk.
   For this reason varpool_ctor_useable_for_folding_p should be used when
   the actual constructor value is not needed.  */

tree
ctor_for_folding (tree decl)
{
  varpool_node *node, *real_node;
  tree real_decl;

  if (TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != CONST_DECL)
    return error_mark_node;

  if (TREE_CODE (decl) == CONST_DECL
      || DECL_IN_CONSTANT_POOL (decl))
    return DECL_INITIAL (decl);

  if (TREE_THIS_VOLATILE (decl))
    return error_mark_node;

  /* Do not care about automatic variables.  Those are never initialized
     anyway, because gimplifier exapnds the code.  */
  if (!TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
    {
      gcc_assert (!TREE_PUBLIC (decl));
      return error_mark_node;
    }

  gcc_assert (TREE_CODE (decl) == VAR_DECL);

  real_node = node = varpool_node::get (decl);
  if (node)
    {
      real_node = node->ultimate_alias_target ();
      real_decl = real_node->decl;
    }
  else
    real_decl = decl;

  /* See if we are dealing with alias.
     In most cases alias is just alternative symbol pointing to a given
     constructor.  This allows us to use interposition rules of DECL
     constructor of REAL_NODE.  However weakrefs are special by being just
     alternative name of their target (if defined).  */
  if (decl != real_decl)
    {
      gcc_assert (!DECL_INITIAL (decl)
		  || (node->alias && node->get_alias_target () == real_node)
		  || DECL_INITIAL (decl) == error_mark_node);
      if (node->weakref)
	{
	  node = node->get_alias_target ();
	  decl = node->decl;
	}
    }

  if ((!DECL_VIRTUAL_P (real_decl)
       || DECL_INITIAL (real_decl) == error_mark_node
       || !DECL_INITIAL (real_decl))
      && (!node || !node->ctor_useable_for_folding_p ()))
    return error_mark_node;

  /* OK, we can return constructor.  See if we need to fetch it from disk
     in LTO mode.  */
  if (DECL_INITIAL (real_decl) != error_mark_node
      || !in_lto_p)
    return DECL_INITIAL (real_decl);
  return real_node->get_constructor ();
}

/* Add the variable DECL to the varpool.
   Unlike varpool_finalize_decl function is intended to be used
   by middle end and allows insertion of new variable at arbitrary point
   of compilation.  */
void
varpool_add_new_variable (tree decl)
{
  varpool_node *node;
  varpool_node::finalize_decl (decl);
  node = varpool_node::get_create (decl);
  varpool_call_variable_insertion_hooks (node);
  if (node->externally_visible_p ())
    node->externally_visible = true;
}

/* Return variable availability.  See cgraph.h for description of individual
   return values.  */
enum availability
varpool_node::get_availability (void)
{
  if (!definition)
    return AVAIL_NOT_AVAILABLE;
  if (!TREE_PUBLIC (decl))
    return AVAIL_AVAILABLE;
  if (DECL_IN_CONSTANT_POOL (decl)
      || DECL_VIRTUAL_P (decl))
    return AVAIL_AVAILABLE;
  if (alias && weakref)
    {
      enum availability avail;

      ultimate_alias_target (&avail)->get_availability ();
      return avail;
    }
  /* If the variable can be overwritten, return OVERWRITABLE.  Takes
     care of at least one notable extension - the COMDAT variables
     used to share template instantiations in C++.  */
  if (decl_replaceable_p (decl)
      || DECL_EXTERNAL (decl))
    return AVAIL_INTERPOSABLE;
  return AVAIL_AVAILABLE;
}

void
varpool_node::analyze (void)
{
  /* When reading back varpool at LTO time, we re-construct the queue in order
     to have "needed" list right by inserting all needed nodes into varpool.
     We however don't want to re-analyze already analyzed nodes.  */
  if (!analyzed)
    {
      gcc_assert (!in_lto_p || cgraph_function_flags_ready);
      /* Compute the alignment early so function body expanders are
	 already informed about increased alignment.  */
      align_variable (decl, 0);
    }
  if (alias)
    resolve_alias (varpool_node::get (alias_target));
  else if (DECL_INITIAL (decl))
    record_references_in_initializer (decl, analyzed);
  analyzed = true;
}

/* Assemble thunks and aliases associated to varpool node.  */

void
varpool_node::assemble_aliases (void)
{
  struct ipa_ref *ref;

  FOR_EACH_ALIAS (this, ref)
    {
      varpool_node *alias = dyn_cast <varpool_node *> (ref->referring);
      do_assemble_alias (alias->decl,
			 DECL_ASSEMBLER_NAME (decl));
      alias->assemble_aliases ();
    }
}

/* Output one variable, if necessary.  Return whether we output it.  */

bool
varpool_node::assemble_decl (void)
{
  /* Aliases are outout when their target is produced or by
     output_weakrefs.  */
  if (alias)
    return false;

  /* Constant pool is output from RTL land when the reference
     survive till this level.  */
  if (DECL_IN_CONSTANT_POOL (decl) && TREE_ASM_WRITTEN (decl))
    return false;

  /* Decls with VALUE_EXPR should not be in the varpool at all.  They
     are not real variables, but just info for debugging and codegen.
     Unfortunately at the moment emutls is not updating varpool correctly
     after turning real vars into value_expr vars.  */
  if (DECL_HAS_VALUE_EXPR_P (decl)
      && !targetm.have_tls)
    return false;

  /* Hard register vars do not need to be output.  */
  if (DECL_HARD_REGISTER (decl))
    return false;

  gcc_checking_assert (!TREE_ASM_WRITTEN (decl)
		       && TREE_CODE (decl) == VAR_DECL
		       && !DECL_HAS_VALUE_EXPR_P (decl));

  if (!in_other_partition
      && !DECL_EXTERNAL (decl))
    {
      get_constructor ();
      assemble_variable (decl, 0, 1, 0);
      gcc_assert (TREE_ASM_WRITTEN (decl));
      gcc_assert (definition);
      assemble_aliases ();
      return true;
    }

  return false;
}

/* Add NODE to queue starting at FIRST. 
   The queue is linked via AUX pointers and terminated by pointer to 1.  */

static void
enqueue_node (varpool_node *node, varpool_node **first)
{
  if (node->aux)
    return;
  gcc_checking_assert (*first);
  node->aux = *first;
  *first = node;
}

/* Optimization of function bodies might've rendered some variables as
   unnecessary so we want to avoid these from being compiled.  Re-do
   reachability starting from variables that are either externally visible
   or was referred from the asm output routines.  */

static void
varpool_remove_unreferenced_decls (void)
{
  varpool_node *next, *node;
  varpool_node *first = (varpool_node *)(void *)1;
  int i;
  struct ipa_ref *ref = NULL;
  struct pointer_set_t *referenced = pointer_set_create ();

  if (seen_error ())
    return;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Trivially needed variables:");
  FOR_EACH_DEFINED_VARIABLE (node)
    {
      if (node->analyzed
	  && (!node->can_remove_if_no_refs_p ()
	      /* We just expanded all function bodies.  See if any of
		 them needed the variable.  */
	      || DECL_RTL_SET_P (node->decl)))
	{
	  enqueue_node (node, &first);
          if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", node->asm_name ());
	}
    }
  while (first != (varpool_node *)(void *)1)
    {
      node = first;
      first = (varpool_node *)first->aux;

      if (node->same_comdat_group)
	{
	  symtab_node *next;
	  for (next = node->same_comdat_group;
	       next != node;
	       next = next->same_comdat_group)
	    {
	      varpool_node *vnext = dyn_cast <varpool_node *> (next);
	      if (vnext && vnext->analyzed && !next->comdat_local_p ())
		enqueue_node (vnext, &first);
	    }
	}
      for (i = 0; node->iterate_reference (i, ref); i++)
	{
	  varpool_node *vnode = dyn_cast <varpool_node *> (ref->referred);
	  if (vnode
	      && !vnode->in_other_partition
	      && (!DECL_EXTERNAL (ref->referred->decl)
		  || vnode->alias)
	      && vnode->analyzed)
	    enqueue_node (vnode, &first);
	  else
	    pointer_set_insert (referenced, node);
	}
    }
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nRemoving variables:");
  for (node = varpool_first_defined_variable (); node; node = next)
    {
      next = varpool_next_defined_variable (node);
      if (!node->aux)
	{
          if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", node->asm_name ());
	  if (pointer_set_contains (referenced, node))
	    node->remove_initializer ();
	  else
	    node->remove ();
	}
    }
  pointer_set_destroy (referenced);
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\n");
}

/* For variables in named sections make sure get_variable_section
   is called before we switch to those sections.  Then section
   conflicts between read-only and read-only requiring relocations
   sections can be resolved.  */
void
varpool_node::finalize_named_section_flags (void)
{
  if (!TREE_ASM_WRITTEN (decl)
      && !alias
      && !in_other_partition
      && !DECL_EXTERNAL (decl)
      && TREE_CODE (decl) == VAR_DECL
      && !DECL_HAS_VALUE_EXPR_P (decl)
      && get_section ())
    get_variable_section (decl, false);
}

/* Output all variables enqueued to be assembled.  */
bool
varpool_node::output_variables (void)
{
  bool changed = false;
  varpool_node *node;

  if (seen_error ())
    return false;

  varpool_remove_unreferenced_decls ();

  timevar_push (TV_VAROUT);

  FOR_EACH_DEFINED_VARIABLE (node)
    node->finalize_named_section_flags ();

  FOR_EACH_DEFINED_VARIABLE (node)
    if (node->assemble_decl ())
      changed = true;
  timevar_pop (TV_VAROUT);
  return changed;
}

/* Create a new global variable of type TYPE.  */
tree
add_new_static_var (tree type)
{
  tree new_decl;
  varpool_node *new_node;

  new_decl = create_tmp_var_raw (type, NULL);
  DECL_NAME (new_decl) = create_tmp_var_name (NULL);
  TREE_READONLY (new_decl) = 0;
  TREE_STATIC (new_decl) = 1;
  TREE_USED (new_decl) = 1;
  DECL_CONTEXT (new_decl) = NULL_TREE;
  DECL_ABSTRACT (new_decl) = 0;
  lang_hooks.dup_lang_specific_decl (new_decl);
  new_node = varpool_node::get_create (new_decl);
  varpool_node::finalize_decl (new_decl);

  return new_node->decl;
}

/* Attempt to mark ALIAS as an alias to DECL.  Return TRUE if successful.
   Extra name aliases are output whenever DECL is output.  */

varpool_node *
varpool_node::create_alias (tree alias, tree decl)
{
  varpool_node *alias_node;

  gcc_assert (TREE_CODE (decl) == VAR_DECL);
  gcc_assert (TREE_CODE (alias) == VAR_DECL);
  alias_node = varpool_node::get_create (alias);
  alias_node->alias = true;
  alias_node->definition = true;
  alias_node->alias_target = decl;
  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (alias)) != NULL)
    alias_node->weakref = true;
  return alias_node;
}

/* Attempt to mark ALIAS as an alias to DECL.  Return TRUE if successful.
   Extra name aliases are output whenever DECL is output.  */

varpool_node *
varpool_node::create_extra_name_alias (tree alias, tree decl)
{
  varpool_node *alias_node;

#ifndef ASM_OUTPUT_DEF
  /* If aliases aren't supported by the assembler, fail.  */
  return NULL;
#endif
  alias_node = varpool_node::create_alias (alias, decl);
  alias_node->cpp_implicit_alias = true;

  /* Extra name alias mechanizm creates aliases really late
     via DECL_ASSEMBLER_NAME mechanizm.
     This is unfortunate because they are not going through the
     standard channels.  Ensure they get output.  */
  if (cpp_implicit_aliases_done)
    alias_node->resolve_alias (varpool_node::get_create (decl));
  return alias_node;
}

/* Call calback on varpool symbol and aliases associated to varpool symbol.
   When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
   skipped. */

bool
varpool_node::call_for_node_and_aliases (bool (*callback) (varpool_node *,
							   void *),
					 void *data,
					 bool include_overwritable)
{
  struct ipa_ref *ref;

  if (callback (this, data))
    return true;

  FOR_EACH_ALIAS (this, ref)
    {
      varpool_node *alias = dyn_cast <varpool_node *> (ref->referring);
      if (include_overwritable
	  || alias->get_availability () > AVAIL_INTERPOSABLE)
	if (alias->call_for_node_and_aliases (callback, data,
					      include_overwritable))
	  return true;
    }
  return false;
}
