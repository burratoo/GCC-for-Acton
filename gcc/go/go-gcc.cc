// go-gcc.cc -- Go frontend to gcc IR.
// Copyright (C) 2011 Free Software Foundation, Inc.
// Contributed by Ian Lance Taylor, Google.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "go-system.h"

// This has to be included outside of extern "C", so we have to
// include it here before tree.h includes it later.
#include <gmp.h>

#ifndef ENABLE_BUILD_WITH_CXX
extern "C"
{
#endif

#include "tree.h"
#include "tree-iterator.h"
#include "gimple.h"

#ifndef ENABLE_BUILD_WITH_CXX
}
#endif

#include "go-c.h"

#include "gogo.h"
#include "backend.h"

// A class wrapping a tree.

class Gcc_tree
{
 public:
  Gcc_tree(tree t)
    : t_(t)
  { }

  tree
  get_tree() const
  { return this->t_; }

 private:
  tree t_;
};

// In gcc, types, expressions, and statements are all trees.
class Btype : public Gcc_tree
{
 public:
  Btype(tree t)
    : Gcc_tree(t)
  { }
};

class Bexpression : public Gcc_tree
{
 public:
  Bexpression(tree t)
    : Gcc_tree(t)
  { }
};

class Bstatement : public Gcc_tree
{
 public:
  Bstatement(tree t)
    : Gcc_tree(t)
  { }
};

class Bfunction : public Gcc_tree
{
 public:
  Bfunction(tree t)
    : Gcc_tree(t)
  { }
};

class Bblock : public Gcc_tree
{
 public:
  Bblock(tree t)
    : Gcc_tree(t)
  { }
};

class Bvariable : public Gcc_tree
{
 public:
  Bvariable(tree t)
    : Gcc_tree(t)
  { }
};

class Blabel : public Gcc_tree
{
 public:
  Blabel(tree t)
    : Gcc_tree(t)
  { }
};

// This file implements the interface between the Go frontend proper
// and the gcc IR.  This implements specific instantiations of
// abstract classes defined by the Go frontend proper.  The Go
// frontend proper class methods of these classes to generate the
// backend representation.

class Gcc_backend : public Backend
{
 public:
  // Types.

  Btype*
  error_type()
  { return this->make_type(error_mark_node); }

  Btype*
  void_type()
  { return this->make_type(void_type_node); }

  Btype*
  bool_type()
  { return this->make_type(boolean_type_node); }

  Btype*
  integer_type(bool, int);

  Btype*
  float_type(int);

  Btype*
  complex_type(int);

  Btype*
  pointer_type(Btype*);

  Btype*
  function_type(const Btyped_identifier&,
		const std::vector<Btyped_identifier>&,
		const std::vector<Btyped_identifier>&,
		source_location);

  Btype*
  struct_type(const std::vector<Btyped_identifier>&);

  Btype*
  array_type(Btype*, Bexpression*);

  Btype*
  placeholder_pointer_type(const std::string&, source_location, bool);

  bool
  set_placeholder_pointer_type(Btype*, Btype*);

  bool
  set_placeholder_function_type(Btype*, Btype*);

  Btype*
  placeholder_struct_type(const std::string&, source_location);

  bool
  set_placeholder_struct_type(Btype* placeholder,
			      const std::vector<Btyped_identifier>&);

  Btype*
  placeholder_array_type(const std::string&, source_location);

  bool
  set_placeholder_array_type(Btype*, Btype*, Bexpression*);

  Btype*
  named_type(const std::string&, Btype*, source_location);

  Btype*
  circular_pointer_type(Btype*, bool);

  bool
  is_circular_pointer_type(Btype*);

  // Statements.

  Bstatement*
  error_statement()
  { return this->make_statement(error_mark_node); }

  Bstatement*
  expression_statement(Bexpression*);

  Bstatement*
  init_statement(Bvariable* var, Bexpression* init);

  Bstatement*
  assignment_statement(Bexpression* lhs, Bexpression* rhs, source_location);

  Bstatement*
  return_statement(Bfunction*, const std::vector<Bexpression*>&,
		   source_location);

  Bstatement*
  if_statement(Bexpression* condition, Bblock* then_block, Bblock* else_block,
	       source_location);

  Bstatement*
  switch_statement(Bexpression* value,
		   const std::vector<std::vector<Bexpression*> >& cases,
		   const std::vector<Bstatement*>& statements,
		   source_location);

  Bstatement*
  compound_statement(Bstatement*, Bstatement*);

  Bstatement*
  statement_list(const std::vector<Bstatement*>&);

  // Blocks.

  Bblock*
  block(Bfunction*, Bblock*, const std::vector<Bvariable*>&,
	source_location, source_location);

  void
  block_add_statements(Bblock*, const std::vector<Bstatement*>&);

  Bstatement*
  block_statement(Bblock*);

  // Variables.

  Bvariable*
  error_variable()
  { return new Bvariable(error_mark_node); }

  Bvariable*
  global_variable(const std::string& package_name,
		  const std::string& unique_prefix,
		  const std::string& name,
		  Btype* btype,
		  bool is_external,
		  bool is_hidden,
		  source_location location);

  void
  global_variable_set_init(Bvariable*, Bexpression*);

  Bvariable*
  local_variable(Bfunction*, const std::string&, Btype*, bool,
		 source_location);

  Bvariable*
  parameter_variable(Bfunction*, const std::string&, Btype*, bool,
		     source_location);

  Bvariable*
  temporary_variable(Bfunction*, Bblock*, Btype*, Bexpression*, bool,
		     source_location, Bstatement**);

  // Labels.

  Blabel*
  label(Bfunction*, const std::string& name, source_location);

  Bstatement*
  label_definition_statement(Blabel*);

  Bstatement*
  goto_statement(Blabel*, source_location);

  Bexpression*
  label_address(Blabel*, source_location);

 private:
  // Make a Bexpression from a tree.
  Bexpression*
  make_expression(tree t)
  { return new Bexpression(t); }

  // Make a Bstatement from a tree.
  Bstatement*
  make_statement(tree t)
  { return new Bstatement(t); }

  // Make a Btype from a tree.
  Btype*
  make_type(tree t)
  { return new Btype(t); }

  Btype*
  fill_in_struct(Btype*, const std::vector<Btyped_identifier>&);

  Btype*
  fill_in_array(Btype*, Btype*, Bexpression*);
};

// A helper function.

static inline tree
get_identifier_from_string(const std::string& str)
{
  return get_identifier_with_length(str.data(), str.length());
}

// Get an unnamed integer type.

Btype*
Gcc_backend::integer_type(bool is_unsigned, int bits)
{
  tree type;
  if (is_unsigned)
    {
      if (bits == INT_TYPE_SIZE)
        type = unsigned_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = unsigned_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_unsigned_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_unsigned_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_unsigned_type_node;
      else
        type = make_unsigned_type(bits);
    }
  else
    {
      if (bits == INT_TYPE_SIZE)
        type = integer_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = signed_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_integer_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_integer_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_integer_type_node;
      else
        type = make_signed_type(bits);
    }
  return this->make_type(type);
}

// Get an unnamed float type.

Btype*
Gcc_backend::float_type(int bits)
{
  tree type;
  if (bits == FLOAT_TYPE_SIZE)
    type = float_type_node;
  else if (bits == DOUBLE_TYPE_SIZE)
    type = double_type_node;
  else if (bits == LONG_DOUBLE_TYPE_SIZE)
    type = long_double_type_node;
  else
    {
      type = make_node(REAL_TYPE);
      TYPE_PRECISION(type) = bits;
      layout_type(type);
    }
  return this->make_type(type);
}

// Get an unnamed complex type.

Btype*
Gcc_backend::complex_type(int bits)
{
  tree type;
  if (bits == FLOAT_TYPE_SIZE * 2)
    type = complex_float_type_node;
  else if (bits == DOUBLE_TYPE_SIZE * 2)
    type = complex_double_type_node;
  else if (bits == LONG_DOUBLE_TYPE_SIZE * 2)
    type = complex_long_double_type_node;
  else
    {
      type = make_node(REAL_TYPE);
      TYPE_PRECISION(type) = bits / 2;
      layout_type(type);
      type = build_complex_type(type);
    }
  return this->make_type(type);
}

// Get a pointer type.

Btype*
Gcc_backend::pointer_type(Btype* to_type)
{
  tree to_type_tree = to_type->get_tree();
  if (to_type_tree == error_mark_node)
    return this->error_type();
  tree type = build_pointer_type(to_type_tree);
  return this->make_type(type);
}

// Make a function type.

Btype*
Gcc_backend::function_type(const Btyped_identifier& receiver,
			   const std::vector<Btyped_identifier>& parameters,
			   const std::vector<Btyped_identifier>& results,
			   source_location location)
{
  tree args = NULL_TREE;
  tree* pp = &args;
  if (receiver.btype != NULL)
    {
      tree t = receiver.btype->get_tree();
      if (t == error_mark_node)
	return this->error_type();
      *pp = tree_cons(NULL_TREE, t, NULL_TREE);
      pp = &TREE_CHAIN(*pp);
    }

  for (std::vector<Btyped_identifier>::const_iterator p = parameters.begin();
       p != parameters.end();
       ++p)
    {
      tree t = p->btype->get_tree();
      if (t == error_mark_node)
	return this->error_type();
      *pp = tree_cons(NULL_TREE, t, NULL_TREE);
      pp = &TREE_CHAIN(*pp);
    }

  // Varargs is handled entirely at the Go level.  When converted to
  // GENERIC functions are not varargs.
  *pp = void_list_node;

  tree result;
  if (results.empty())
    result = void_type_node;
  else if (results.size() == 1)
    result = results.front().btype->get_tree();
  else
    {
      result = make_node(RECORD_TYPE);
      tree field_trees = NULL_TREE;
      pp = &field_trees;
      for (std::vector<Btyped_identifier>::const_iterator p = results.begin();
	   p != results.end();
	   ++p)
	{
	  const std::string name = (p->name.empty()
				    ? "UNNAMED"
				    : p->name);
	  tree name_tree = get_identifier_from_string(name);
	  tree field_type_tree = p->btype->get_tree();
	  if (field_type_tree == error_mark_node)
	    return this->error_type();
	  gcc_assert(TYPE_SIZE(field_type_tree) != NULL_TREE);
	  tree field = build_decl(location, FIELD_DECL, name_tree,
				  field_type_tree);
	  DECL_CONTEXT(field) = result;
	  *pp = field;
	  pp = &DECL_CHAIN(field);
	}
      TYPE_FIELDS(result) = field_trees;
      layout_type(result);
    }
  if (result == error_mark_node)
    return this->error_type();

  tree fntype = build_function_type(result, args);
  if (fntype == error_mark_node)
    return this->error_type();

  return this->make_type(build_pointer_type(fntype));
}

// Make a struct type.

Btype*
Gcc_backend::struct_type(const std::vector<Btyped_identifier>& fields)
{
  return this->fill_in_struct(this->make_type(make_node(RECORD_TYPE)), fields);
}

// Fill in the fields of a struct type.

Btype*
Gcc_backend::fill_in_struct(Btype* fill,
			    const std::vector<Btyped_identifier>& fields)
{
  tree fill_tree = fill->get_tree();
  tree field_trees = NULL_TREE;
  tree* pp = &field_trees;
  for (std::vector<Btyped_identifier>::const_iterator p = fields.begin();
       p != fields.end();
       ++p)
    {
      tree name_tree = get_identifier_from_string(p->name);
      tree type_tree = p->btype->get_tree();
      if (type_tree == error_mark_node)
	return this->error_type();
      tree field = build_decl(p->location, FIELD_DECL, name_tree, type_tree);
      DECL_CONTEXT(field) = fill_tree;
      *pp = field;
      pp = &DECL_CHAIN(field);
    }
  TYPE_FIELDS(fill_tree) = field_trees;
  layout_type(fill_tree);
  return fill;
}

// Make an array type.

Btype*
Gcc_backend::array_type(Btype* element_btype, Bexpression* length)
{
  return this->fill_in_array(this->make_type(make_node(ARRAY_TYPE)),
			     element_btype, length);
}

// Fill in an array type.

Btype*
Gcc_backend::fill_in_array(Btype* fill, Btype* element_type,
			   Bexpression* length)
{
  tree element_type_tree = element_type->get_tree();
  tree length_tree = length->get_tree();
  if (element_type_tree == error_mark_node || length_tree == error_mark_node)
    return this->error_type();

  gcc_assert(TYPE_SIZE(element_type_tree) != NULL_TREE);

  length_tree = fold_convert(sizetype, length_tree);

  // build_index_type takes the maximum index, which is one less than
  // the length.
  tree index_type_tree = build_index_type(fold_build2(MINUS_EXPR, sizetype,
						      length_tree,
						      size_one_node));

  tree fill_tree = fill->get_tree();
  TREE_TYPE(fill_tree) = element_type_tree;
  TYPE_DOMAIN(fill_tree) = index_type_tree;
  TYPE_ADDR_SPACE(fill_tree) = TYPE_ADDR_SPACE(element_type_tree);
  layout_type(fill_tree);

  if (TYPE_STRUCTURAL_EQUALITY_P(element_type_tree))
    SET_TYPE_STRUCTURAL_EQUALITY(fill_tree);
  else if (TYPE_CANONICAL(element_type_tree) != element_type_tree
	   || TYPE_CANONICAL(index_type_tree) != index_type_tree)
    TYPE_CANONICAL(fill_tree) =
      build_array_type(TYPE_CANONICAL(element_type_tree),
		       TYPE_CANONICAL(index_type_tree));

  return fill;
}

// Create a placeholder for a pointer type.

Btype*
Gcc_backend::placeholder_pointer_type(const std::string& name,
				      source_location location, bool)
{
  tree ret = build_variant_type_copy(ptr_type_node);
  if (!name.empty())
    {
      tree decl = build_decl(location, TYPE_DECL,
			     get_identifier_from_string(name),
			     ret);
      TYPE_NAME(ret) = decl;
    }
  return this->make_type(ret);
}

// Set the real target type for a placeholder pointer type.

bool
Gcc_backend::set_placeholder_pointer_type(Btype* placeholder,
					  Btype* to_type)
{
  tree pt = placeholder->get_tree();
  if (pt == error_mark_node)
    return false;
  gcc_assert(TREE_CODE(pt) == POINTER_TYPE);
  tree tt = to_type->get_tree();
  if (tt == error_mark_node)
    {
      TREE_TYPE(pt) = tt;
      return false;
    }
  gcc_assert(TREE_CODE(tt) == POINTER_TYPE);
  TREE_TYPE(pt) = TREE_TYPE(tt);
  return true;
}

// Set the real values for a placeholder function type.

bool
Gcc_backend::set_placeholder_function_type(Btype* placeholder, Btype* ft)
{
  return this->set_placeholder_pointer_type(placeholder, ft);
}

// Create a placeholder for a struct type.

Btype*
Gcc_backend::placeholder_struct_type(const std::string& name,
				     source_location location)
{
  tree ret = make_node(RECORD_TYPE);
  tree decl = build_decl(location, TYPE_DECL,
			 get_identifier_from_string(name),
			 ret);
  TYPE_NAME(ret) = decl;
  return this->make_type(ret);
}

// Fill in the fields of a placeholder struct type.

bool
Gcc_backend::set_placeholder_struct_type(
    Btype* placeholder,
    const std::vector<Btyped_identifier>& fields)
{
  tree t = placeholder->get_tree();
  gcc_assert(TREE_CODE(t) == RECORD_TYPE && TYPE_FIELDS(t) == NULL_TREE);
  Btype* r = this->fill_in_struct(placeholder, fields);
  return r->get_tree() != error_mark_node;
}

// Create a placeholder for an array type.

Btype*
Gcc_backend::placeholder_array_type(const std::string& name,
				    source_location location)
{
  tree ret = make_node(ARRAY_TYPE);
  tree decl = build_decl(location, TYPE_DECL,
			 get_identifier_from_string(name),
			 ret);
  TYPE_NAME(ret) = decl;
  return this->make_type(ret);
}

// Fill in the fields of a placeholder array type.

bool
Gcc_backend::set_placeholder_array_type(Btype* placeholder,
					Btype* element_btype,
					Bexpression* length)
{
  tree t = placeholder->get_tree();
  gcc_assert(TREE_CODE(t) == ARRAY_TYPE && TREE_TYPE(t) == NULL_TREE);
  Btype* r = this->fill_in_array(placeholder, element_btype, length);
  return r->get_tree() != error_mark_node;
}

// Return a named version of a type.

Btype*
Gcc_backend::named_type(const std::string& name, Btype* btype,
			source_location location)
{
  tree type = btype->get_tree();
  if (type == error_mark_node)
    return this->error_type();
  type = build_variant_type_copy(type);
  tree decl = build_decl(location, TYPE_DECL,
			 get_identifier_from_string(name),
			 type);
  TYPE_NAME(type) = decl;
  return this->make_type(type);
}

// Return a pointer type used as a marker for a circular type.

Btype*
Gcc_backend::circular_pointer_type(Btype*, bool)
{
  return this->make_type(ptr_type_node);
}

// Return whether we might be looking at a circular type.

bool
Gcc_backend::is_circular_pointer_type(Btype* btype)
{
  return btype->get_tree() == ptr_type_node;
}

// An expression as a statement.

Bstatement*
Gcc_backend::expression_statement(Bexpression* expr)
{
  return this->make_statement(expr->get_tree());
}

// Variable initialization.

Bstatement*
Gcc_backend::init_statement(Bvariable* var, Bexpression* init)
{
  tree var_tree = var->get_tree();
  tree init_tree = init->get_tree();
  if (var_tree == error_mark_node || init_tree == error_mark_node)
    return this->error_statement();
  gcc_assert(TREE_CODE(var_tree) == VAR_DECL);
  DECL_INITIAL(var_tree) = init_tree;
  return this->make_statement(build1_loc(DECL_SOURCE_LOCATION(var_tree),
					 DECL_EXPR, void_type_node, var_tree));
}

// Assignment.

Bstatement*
Gcc_backend::assignment_statement(Bexpression* lhs, Bexpression* rhs,
				  source_location location)
{
  tree lhs_tree = lhs->get_tree();
  tree rhs_tree = rhs->get_tree();
  if (lhs_tree == error_mark_node || rhs_tree == error_mark_node)
    return this->error_statement();
  return this->make_statement(fold_build2_loc(location, MODIFY_EXPR,
					      void_type_node,
					      lhs_tree, rhs_tree));
}

// Return.

Bstatement*
Gcc_backend::return_statement(Bfunction* bfunction,
			      const std::vector<Bexpression*>& vals,
			      source_location location)
{
  tree fntree = bfunction->get_tree();
  if (fntree == error_mark_node)
    return this->error_statement();
  tree result = DECL_RESULT(fntree);
  if (result == error_mark_node)
    return this->error_statement();
  tree ret;
  if (vals.empty())
    ret = fold_build1_loc(location, RETURN_EXPR, void_type_node, NULL_TREE);
  else if (vals.size() == 1)
    {
      tree val = vals.front()->get_tree();
      if (val == error_mark_node)
	return this->error_statement();
      tree set = fold_build2_loc(location, MODIFY_EXPR, void_type_node,
				 result, vals.front()->get_tree());
      ret = fold_build1_loc(location, RETURN_EXPR, void_type_node, set);
    }
  else
    {
      // To return multiple values, copy the values into a temporary
      // variable of the right structure type, and then assign the
      // temporary variable to the DECL_RESULT in the return
      // statement.
      tree stmt_list = NULL_TREE;
      tree rettype = TREE_TYPE(result);
      tree rettmp = create_tmp_var(rettype, "RESULT");
      tree field = TYPE_FIELDS(rettype);
      for (std::vector<Bexpression*>::const_iterator p = vals.begin();
	   p != vals.end();
	   p++, field = DECL_CHAIN(field))
	{
	  gcc_assert(field != NULL_TREE);
	  tree ref = fold_build3_loc(location, COMPONENT_REF, TREE_TYPE(field),
				     rettmp, field, NULL_TREE);
	  tree val = (*p)->get_tree();
	  if (val == error_mark_node)
	    return this->error_statement();
	  tree set = fold_build2_loc(location, MODIFY_EXPR, void_type_node,
				     ref, (*p)->get_tree());
	  append_to_statement_list(set, &stmt_list);
	}
      gcc_assert(field == NULL_TREE);
      tree set = fold_build2_loc(location, MODIFY_EXPR, void_type_node,
				 result, rettmp);
      tree ret_expr = fold_build1_loc(location, RETURN_EXPR, void_type_node,
				      set);
      append_to_statement_list(ret_expr, &stmt_list);
      ret = stmt_list;
    }
  return this->make_statement(ret);
}

// If.

Bstatement*
Gcc_backend::if_statement(Bexpression* condition, Bblock* then_block,
			  Bblock* else_block, source_location location)
{
  tree cond_tree = condition->get_tree();
  tree then_tree = then_block->get_tree();
  tree else_tree = else_block == NULL ? NULL_TREE : else_block->get_tree();
  if (cond_tree == error_mark_node
      || then_tree == error_mark_node
      || else_tree == error_mark_node)
    return this->error_statement();
  tree ret = build3_loc(location, COND_EXPR, void_type_node, cond_tree,
			then_tree, else_tree);
  return this->make_statement(ret);
}

// Switch.

Bstatement*
Gcc_backend::switch_statement(
    Bexpression* value,
    const std::vector<std::vector<Bexpression*> >& cases,
    const std::vector<Bstatement*>& statements,
    source_location switch_location)
{
  gcc_assert(cases.size() == statements.size());

  tree stmt_list = NULL_TREE;
  std::vector<std::vector<Bexpression*> >::const_iterator pc = cases.begin();
  for (std::vector<Bstatement*>::const_iterator ps = statements.begin();
       ps != statements.end();
       ++ps, ++pc)
    {
      if (pc->empty())
	{
	  source_location loc = (*ps != NULL
				 ? EXPR_LOCATION((*ps)->get_tree())
				 : UNKNOWN_LOCATION);
	  tree label = create_artificial_label(loc);
	  tree c = build_case_label(NULL_TREE, NULL_TREE, label);
	  append_to_statement_list(c, &stmt_list);
	}
      else
	{
	  for (std::vector<Bexpression*>::const_iterator pcv = pc->begin();
	       pcv != pc->end();
	       ++pcv)
	    {
	      tree t = (*pcv)->get_tree();
	      if (t == error_mark_node)
		return this->error_statement();
	      source_location loc = EXPR_LOCATION(t);
	      tree label = create_artificial_label(loc);
	      tree c = build_case_label((*pcv)->get_tree(), NULL_TREE, label);
	      append_to_statement_list(c, &stmt_list);
	    }
	}

      if (*ps != NULL)
	{
	  tree t = (*ps)->get_tree();
	  if (t == error_mark_node)
	    return this->error_statement();
	  append_to_statement_list(t, &stmt_list);
	}
    }

  tree tv = value->get_tree();
  if (tv == error_mark_node)
    return this->error_statement();
  tree t = build3_loc(switch_location, SWITCH_EXPR, void_type_node,
		      tv, stmt_list, NULL_TREE);
  return this->make_statement(t);
}

// Pair of statements.

Bstatement*
Gcc_backend::compound_statement(Bstatement* s1, Bstatement* s2)
{
  tree stmt_list = NULL_TREE;
  tree t = s1->get_tree();
  if (t == error_mark_node)
    return this->error_statement();
  append_to_statement_list(t, &stmt_list);
  t = s2->get_tree();
  if (t == error_mark_node)
    return this->error_statement();
  append_to_statement_list(t, &stmt_list);
  return this->make_statement(stmt_list);
}

// List of statements.

Bstatement*
Gcc_backend::statement_list(const std::vector<Bstatement*>& statements)
{
  tree stmt_list = NULL_TREE;
  for (std::vector<Bstatement*>::const_iterator p = statements.begin();
       p != statements.end();
       ++p)
    {
      tree t = (*p)->get_tree();
      if (t == error_mark_node)
	return this->error_statement();
      append_to_statement_list(t, &stmt_list);
    }
  return this->make_statement(stmt_list);
}

// Make a block.  For some reason gcc uses a dual structure for
// blocks: BLOCK tree nodes and BIND_EXPR tree nodes.  Since the
// BIND_EXPR node points to the BLOCK node, we store the BIND_EXPR in
// the Bblock.

Bblock*
Gcc_backend::block(Bfunction* function, Bblock* enclosing,
		   const std::vector<Bvariable*>& vars,
		   source_location start_location,
		   source_location)
{
  tree block_tree = make_node(BLOCK);
  if (enclosing == NULL)
    {
      // FIXME: Permitting FUNCTION to be NULL is a temporary measure
      // until we have a proper representation of the init function.
      tree fndecl;
      if (function == NULL)
	fndecl = current_function_decl;
      else
	fndecl = function->get_tree();
      gcc_assert(fndecl != NULL_TREE);

      // We may have already created a block for local variables when
      // we take the address of a parameter.
      if (DECL_INITIAL(fndecl) == NULL_TREE)
	{
	  BLOCK_SUPERCONTEXT(block_tree) = fndecl;
	  DECL_INITIAL(fndecl) = block_tree;
	}
      else
	{
	  tree superblock_tree = DECL_INITIAL(fndecl);
	  BLOCK_SUPERCONTEXT(block_tree) = superblock_tree;
	  tree* pp;
	  for (pp = &BLOCK_SUBBLOCKS(superblock_tree);
	       *pp != NULL_TREE;
	       pp = &BLOCK_CHAIN(*pp))
	    ;
	  *pp = block_tree;
	}
    }
  else
    {
      tree superbind_tree = enclosing->get_tree();
      tree superblock_tree = BIND_EXPR_BLOCK(superbind_tree);
      gcc_assert(TREE_CODE(superblock_tree) == BLOCK);

      BLOCK_SUPERCONTEXT(block_tree) = superblock_tree;
      tree* pp;
      for (pp = &BLOCK_SUBBLOCKS(superblock_tree);
	   *pp != NULL_TREE;
	   pp = &BLOCK_CHAIN(*pp))
	;
      *pp = block_tree;
    }

  tree* pp = &BLOCK_VARS(block_tree);
  for (std::vector<Bvariable*>::const_iterator pv = vars.begin();
       pv != vars.end();
       ++pv)
    {
      *pp = (*pv)->get_tree();
      if (*pp != error_mark_node)
	pp = &DECL_CHAIN(*pp);
    }
  *pp = NULL_TREE;

  TREE_USED(block_tree) = 1;

  tree bind_tree = build3_loc(start_location, BIND_EXPR, void_type_node,
			      BLOCK_VARS(block_tree), NULL_TREE, block_tree);
  TREE_SIDE_EFFECTS(bind_tree) = 1;

  return new Bblock(bind_tree);
}

// Add statements to a block.

void
Gcc_backend::block_add_statements(Bblock* bblock,
				  const std::vector<Bstatement*>& statements)
{
  tree stmt_list = NULL_TREE;
  for (std::vector<Bstatement*>::const_iterator p = statements.begin();
       p != statements.end();
       ++p)
    {
      tree s = (*p)->get_tree();
      if (s != error_mark_node)
	append_to_statement_list(s, &stmt_list);
    }

  tree bind_tree = bblock->get_tree();
  gcc_assert(TREE_CODE(bind_tree) == BIND_EXPR);
  BIND_EXPR_BODY(bind_tree) = stmt_list;
}

// Return a block as a statement.

Bstatement*
Gcc_backend::block_statement(Bblock* bblock)
{
  tree bind_tree = bblock->get_tree();
  gcc_assert(TREE_CODE(bind_tree) == BIND_EXPR);
  return this->make_statement(bind_tree);
}

// Make a global variable.

Bvariable*
Gcc_backend::global_variable(const std::string& package_name,
			     const std::string& unique_prefix,
			     const std::string& name,
			     Btype* btype,
			     bool is_external,
			     bool is_hidden,
			     source_location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();

  std::string var_name(package_name);
  var_name.push_back('.');
  var_name.append(name);
  tree decl = build_decl(location, VAR_DECL,
			 get_identifier_from_string(var_name),
			 type_tree);
  if (is_external)
    DECL_EXTERNAL(decl) = 1;
  else
    TREE_STATIC(decl) = 1;
  if (!is_hidden)
    {
      TREE_PUBLIC(decl) = 1;

      std::string asm_name(unique_prefix);
      asm_name.push_back('.');
      asm_name.append(var_name);
      SET_DECL_ASSEMBLER_NAME(decl, get_identifier_from_string(asm_name));
    }
  TREE_USED(decl) = 1;

  go_preserve_from_gc(decl);

  return new Bvariable(decl);
}

// Set the initial value of a global variable.

void
Gcc_backend::global_variable_set_init(Bvariable* var, Bexpression* expr)
{
  tree expr_tree = expr->get_tree();
  if (expr_tree == error_mark_node)
    return;
  gcc_assert(TREE_CONSTANT(expr_tree));
  tree var_decl = var->get_tree();
  if (var_decl == error_mark_node)
    return;
  DECL_INITIAL(var_decl) = expr_tree;
}

// Make a local variable.

Bvariable*
Gcc_backend::local_variable(Bfunction* function, const std::string& name,
			    Btype* btype, bool is_address_taken,
			    source_location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();
  tree decl = build_decl(location, VAR_DECL,
			 get_identifier_from_string(name),
			 type_tree);
  DECL_CONTEXT(decl) = function->get_tree();
  TREE_USED(decl) = 1;
  if (is_address_taken)
    TREE_ADDRESSABLE(decl) = 1;
  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Make a function parameter variable.

Bvariable*
Gcc_backend::parameter_variable(Bfunction* function, const std::string& name,
				Btype* btype, bool is_address_taken,
				source_location location)
{
  tree type_tree = btype->get_tree();
  if (type_tree == error_mark_node)
    return this->error_variable();
  tree decl = build_decl(location, PARM_DECL,
			 get_identifier_from_string(name),
			 type_tree);
  DECL_CONTEXT(decl) = function->get_tree();
  DECL_ARG_TYPE(decl) = type_tree;
  TREE_USED(decl) = 1;
  if (is_address_taken)
    TREE_ADDRESSABLE(decl) = 1;
  go_preserve_from_gc(decl);
  return new Bvariable(decl);
}

// Make a temporary variable.

Bvariable*
Gcc_backend::temporary_variable(Bfunction* function, Bblock* bblock,
				Btype* btype, Bexpression* binit,
				bool is_address_taken,
				source_location location,
				Bstatement** pstatement)
{
  tree type_tree = btype->get_tree();
  tree init_tree = binit == NULL ? NULL_TREE : binit->get_tree();
  if (type_tree == error_mark_node || init_tree == error_mark_node)
    {
      *pstatement = this->error_statement();
      return this->error_variable();
    }

  tree var;
  // We can only use create_tmp_var if the type is not addressable.
  if (!TREE_ADDRESSABLE(type_tree))
    var = create_tmp_var(type_tree, "GOTMP");
  else
    {
      gcc_assert(bblock != NULL);
      var = build_decl(location, VAR_DECL,
		       create_tmp_var_name("GOTMP"),
		       type_tree);
      DECL_ARTIFICIAL(var) = 1;
      DECL_IGNORED_P(var) = 1;
      TREE_USED(var) = 1;
      // FIXME: Permitting function to be NULL here is a temporary
      // measure until we have a proper representation of the init
      // function.
      if (function != NULL)
	DECL_CONTEXT(var) = function->get_tree();
      else
	{
	  gcc_assert(current_function_decl != NULL_TREE);
	  DECL_CONTEXT(var) = current_function_decl;
	}

      // We have to add this variable to the BLOCK and the BIND_EXPR.
      tree bind_tree = bblock->get_tree();
      gcc_assert(TREE_CODE(bind_tree) == BIND_EXPR);
      tree block_tree = BIND_EXPR_BLOCK(bind_tree);
      gcc_assert(TREE_CODE(block_tree) == BLOCK);
      DECL_CHAIN(var) = BLOCK_VARS(block_tree);
      BLOCK_VARS(block_tree) = var;
      BIND_EXPR_VARS(bind_tree) = BLOCK_VARS(block_tree);
    }

  if (init_tree != NULL_TREE)
    DECL_INITIAL(var) = fold_convert_loc(location, type_tree, init_tree);

  if (is_address_taken)
    TREE_ADDRESSABLE(var) = 1;

  *pstatement = this->make_statement(build1_loc(location, DECL_EXPR,
						void_type_node, var));
  return new Bvariable(var);
}

// Make a label.

Blabel*
Gcc_backend::label(Bfunction* function, const std::string& name,
		   source_location location)
{
  tree decl;
  if (name.empty())
    decl = create_artificial_label(location);
  else
    {
      tree id = get_identifier_from_string(name);
      decl = build_decl(location, LABEL_DECL, id, void_type_node);
      DECL_CONTEXT(decl) = function->get_tree();
    }
  return new Blabel(decl);
}

// Make a statement which defines a label.

Bstatement*
Gcc_backend::label_definition_statement(Blabel* label)
{
  tree lab = label->get_tree();
  tree ret = fold_build1_loc(DECL_SOURCE_LOCATION(lab), LABEL_EXPR,
			     void_type_node, lab);
  return this->make_statement(ret);
}

// Make a goto statement.

Bstatement*
Gcc_backend::goto_statement(Blabel* label, source_location location)
{
  tree lab = label->get_tree();
  tree ret = fold_build1_loc(location, GOTO_EXPR, void_type_node, lab);
  return this->make_statement(ret);
}

// Get the address of a label.

Bexpression*
Gcc_backend::label_address(Blabel* label, source_location location)
{
  tree lab = label->get_tree();
  TREE_USED(lab) = 1;
  TREE_ADDRESSABLE(lab) = 1;
  tree ret = fold_convert_loc(location, ptr_type_node,
			      build_fold_addr_expr_loc(location, lab));
  return this->make_expression(ret);
}

// The single backend.

static Gcc_backend gcc_backend;

// Return the backend generator.

Backend*
go_get_backend()
{
  return &gcc_backend;
}

// FIXME: Temporary functions while converting to the new backend
// interface.

Btype*
tree_to_type(tree t)
{
  return new Btype(t);
}

Bexpression*
tree_to_expr(tree t)
{
  return new Bexpression(t);
}

Bstatement*
tree_to_stat(tree t)
{
  return new Bstatement(t);
}

Bfunction*
tree_to_function(tree t)
{
  return new Bfunction(t);
}

Bblock*
tree_to_block(tree t)
{
  gcc_assert(TREE_CODE(t) == BIND_EXPR);
  return new Bblock(t);
}

tree
type_to_tree(Btype* bt)
{
  return bt->get_tree();
}

tree
expr_to_tree(Bexpression* be)
{
  return be->get_tree();
}

tree
stat_to_tree(Bstatement* bs)
{
  return bs->get_tree();
}

tree
block_to_tree(Bblock* bb)
{
  return bb->get_tree();
}

tree
var_to_tree(Bvariable* bv)
{
  return bv->get_tree();
}
