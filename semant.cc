

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <iostream>
#include <set>

extern int semant_debug;
extern char *curr_filename;
extern int node_lineno;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       isProto,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       _BOTTOM_,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  ::copy      = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  isProto     = idtable.add_string("isProto");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  // _BOTTOM_ is the symbol for the bottom of the lattice of types
  _BOTTOM_    = idtable.add_string("_bottom");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {
  install_basic_classes();
  
  /** iterate through, add each class to our data structure */
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ current = classes->nth(i);
    Symbol name = current->get_name();
    if (classNameMap.find(name) != classNameMap.end()) {      // if class is already declared, throw error
      semant_error(current) << "Class " << name->get_string() << " was previously defined." << endl;        // ask about declaring error here
    }
    classNameMap[name] = current;
  }

  if (!verifyParents(classes)) { return; }       // if we can't verify the parents of each class, return
  if (!checkInheritance(classes)) { return; }    // if there are inheritance cycles, return

  /** start type checking */

}

/** 
  This method checks if the parents that each class inherits from exists. 
 */
bool ClassTable::verifyParents(Classes classes) {
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ current = classes->nth(i);
    if (!(current->get_name()->equal_string("Object", 6)) && classNameMap.find(current->get_parent()) == classNameMap.end()) {
      semant_error(current) << "Class " << current->get_name()->get_string() << " inherits from an undefined class " << current->get_parent()->get_string() << endl; 
      return false;     
    }
  }
  return true;
}

/** 
  This method checks if there is cyclic inheritance for a given class symbol,
  and returns true if the graph is well formed, and false if there are no cycles. 
 */
bool ClassTable::checkInheritance(Classes classes) {
  bool inheritanceGood = true;

  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ current = classes->nth(i);
    Symbol name = current->get_name();
    Symbol parent = current->get_parent();
    bool check = true;
    std::set<Symbol> visitedClasses;
    visitedClasses.insert(name);

    /** iterate through inheritance graph and check for cycles for each class */
    while (check) {       
      if (parent->equal_string("_no_class", 9)) {
        check = false;
      }
      else if (visitedClasses.find(parent) != visitedClasses.end()) {   // if we have already seen the class we are visiting now, we have a cycle!
        semant_error(current) << "Class " << name->get_string() << " or an ancestor of " << name->get_string() << ", is involved in an inheritance cycle." << endl;      // ask Sai Gautham about error statements
        check = false;
        inheritanceGood = false;
      } else {
        visitedClasses.insert(parent);
        parent = classNameMap[parent]->get_parent();
      }
    }
  }
  return inheritanceGood;
}
std::map<symbol, env>
void ClassTable::check_methods() {
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    // Check if it has a parent
    if(there is a parent):
      curr_env = 

    else:
      // Iterate through the features.
      // Check if it is a method:
          // Add to the method table <method_name, *method_class>
      // Check if it is an atrribute:
          // Add to the attribute_table <attr_name, type_decl
      
      class_env_table[class->get_name()] = new Environment(method_table, attribute_table, ...)
  }
}

void ClassTable::install_basic_classes() {
  // The tree package uses these globals to annotate the classes built below.
  node_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  // The following demonstrates how to create dummy parse trees to
  // refer to basic Cool classes.  There's no need for method
  // bodies -- these are already built into the runtime system.

  // IMPORTANT: The results of the following expressions are
  // stored in local variables.  You will want to do something
  // with those variables at the end of this method to make this
  // code meaningful.


  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class name
  //        copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //

  Class_ Object_class =
    class_(Object,
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(::copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename);

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //

  Class_ IO_class =
     class_(IO,
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	    filename);

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //

  Class_ Int_class =
      class_(Int,
	     Object,
	     single_Features(attr(val, prim_slot, no_expr())),
	     filename);

  //
  // Bool also has only the "val" slot.
  //

  Class_ Bool_class =
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //

  Class_ Str_class =
      class_(Str,
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat,
				   single_Formals(formal(arg, Str)),
				   Str,
				   no_expr()))),
	    single_Features(method(substr,
				   append_Formals(single_Formals(formal(arg, Int)),
						  single_Formals(formal(arg2, Int))),
				   Str,
				   no_expr()))),
	     filename);
  
  classNameMap[Object] = Object_class;
  classNameMap[IO] = IO_class;
  classNameMap[Int] = Int_class;
  classNameMap[Bool] = Bool_class;
  classNameMap[Str] = Str_class;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//       (line number is extracted from tree_node)
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
  return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}


/*
 * This is the entry point to the semantic checker.
 *
 * Your checker should do the following two things:
 *
 *   1) Check that the program is semantically correct
 *   2) Decorate the abstract syntax tree with type information
 *      by setting the `type' field in each Expression node.
 *      (see `tree.h')
 *
 *   You are free to first do 1), make sure you catch all semantic
 *   errors. Part 2) can be done in a second stage, when you want
 *   to build mycoolc.
 */
void program_class::semant() {
   initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
   ClassTableP classtable = new ClassTable(classes);

   if (classtable->errors()) {
      cerr << "Compilation halted due to static semantic errors." << endl;
      exit(1);
   }
}
