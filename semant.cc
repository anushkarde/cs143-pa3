

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <iostream>
#include <set>
#include <queue>
#include <vector>

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
    Symbol parent = current->get_parent();
    if (name == SELF_TYPE || name == Object || name == Int || name == Str || name == Bool) {  // cannot redefine basic classes
      semant_error(current) << "Redefinition of basic class " << name->get_string() << "." << endl;
      return;
    }
    if (classNameMap.find(name) != classNameMap.end()) {      // if class is already declared, throw error
      semant_error(current) << "Class " << name->get_string() << " was previously defined." << endl;
      return;
    } 
    if (parent == Int || parent == Str || parent == Bool || parent == SELF_TYPE) {
      semant_error(current) << "Class " << name->get_string() << " cannot inherit class " << parent->get_string() << "." << endl;
      return;
    }
    classNameMap[name] = current;
  }
  if (!verifyParents(classes)) { return; }       // if we can't verify the parents of each class, return
  if (!checkInheritance(classes)) { return; }    // if there are inheritance cycles, return
  /** must have a main class somewhere */
  if (classNameMap.find(Main) == classNameMap.end()) {
    semant_error() << "Class Main is not defined." << endl;
    return;
  }

  /** build the actual inheritance tree */
  topSortedClasses = topSortClasses();

  /** start method checking */
  checkMethods();
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
        semant_error(current) << "Class " << name->get_string() << " or an ancestor of " << name->get_string() << ", is involved in an inheritance cycle." << endl;
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

/** 
  Performs Kahn's toplogical sort on our list of classes. Returns a 
  vector of the topologically sorted classes.
 */
std::vector<Symbol> ClassTable::topSortClasses() {
  std::vector<Symbol> topSorted = {};      // initialize a list to hold our top sorted classes
  /** calculate degrees of each class for Kahn's top sort */
  std::map<Symbol, int> inDegrees = {};
  std::map<Symbol, std::vector<Symbol>> parentToKids = {};

  for (const auto& pair : classNameMap) {
    Symbol parent = pair.second->get_parent();
    if (parentToKids.find(parent) == parentToKids.end()) {
      parentToKids[parent] = {};
      parentToKids[parent].push_back(pair.first);
    } else {
      parentToKids[parent].push_back(pair.first);
    }
    if (parent != No_class) {
      inDegrees[pair.first] = 1;
    } else {
      inDegrees[pair.first] = 0;
    }
  }

  std::queue<Symbol> topOrderQueue = {};
  for (const auto& pair : inDegrees) {
    if (pair.second == 0) {
      topOrderQueue.push(pair.first);
    }
  } 

  while (!topOrderQueue.empty()){
    Symbol curr = topOrderQueue.front();
    topOrderQueue.pop();
    topSorted.push_back(curr);
    for (Symbol child : parentToKids[curr]) {
      inDegrees[child] -= 1;
      if (inDegrees[child] == 0) {
        topOrderQueue.push(child);
      }
    }
  }
  return topSorted;
}

void ClassTable::checkMethods() {
  for (Symbol curClass : topSortedClasses) {
    Environment *curEnv = new Environment(curClass);
    Environment *parentEnv = nullptr;
    /** if the environment has a parent, copy the parent's environment */
    if(classNameMap[curClass]->get_parent() != No_class)
    {
      Symbol parent = classNameMap[curClass]->get_parent();
      parentEnv = classEnvTable[parent]->copyEnvironment();
    }
    Features featureList = classNameMap[curClass]->get_features();
    /** iterate over features */
    curEnv->getMethodTable()->enterscope(); 
    curEnv->getAttribTable()->enterscope();
    for (int i = featureList->first(); featureList->more(i); i = featureList->next(i)) {
      Feature curFeat = featureList->nth(i);
      if (curFeat->is_method()) {   // if method, add to method table
        if (parentEnv != nullptr && parentEnv->getMethodTable()->lookup(curFeat->get_name()) != NULL) {     // if we are overriding a method, check for semantic errors in overriding
          checkInheritedMethods(curFeat, parentEnv->getMethodTable->lookup(curFeat->get_name()));
        } else {
          curEnv->getMethodTable.addid(curFeat->get_name(), curFeat);
        }
      } else {                      // if attribute, add to attr table
        curEnv->getAttribTable.addid(curFeat->get_name(), curFeat->get_type_decl());
        if (parentEnv != nullptr && parentEnv->getMethodTable()->lookup(curFeat->get_name()) != NULL) {
          semant_error(classNameMap[curClass]) << "Attribute " << curFeat->get_name() << " is an attribute of an inherited class." << endl;
        }
      }
    }
    
    classEnvTable[curClass] = curEnv;
  }
}

void checkInheritedMethods(Feature childFeat, Feature parentFeat) {
  Formals childFormals = childFeat->get_formals();
  Formals parentFormals = parentFeat->get_formals();
  if (len(childFormals) != len(parentFormals)) {
    semant_error() << "Incompatible number of formal parameters in redefined method " << parentFeat->get_name() << endl;
    return;
  }
  /** keep track of parent formal types, we want same # and types of arguments */
  for (int i = parentFormals->first(); parentFormals->more(i); i = parentFormals->next(i)) {
    Formal parentForm = parentFormals->nth(i);
    Formal childForm = childFormals->nth(i);
    Symbol origType = parentForm->get_type();
    Symbol childType = childForm->get_type();
    if (childType != origType) {
      semant_error() << "In redefined method " << parentFeat->get_name() << ", parameter type " << childType->get_name() << " is different from original type " << origType->get_name() << endl;
    }
  }
  /** we also want both methods to have the same return type */ 
  if (childFeat->get_return_type() != parentFeat->get_return_type()) {
    semant_error() << "In redefined method " << parentFeat->get_name() << ", return type " << childFeat->get_return_type()->get_name() " is different from original return type " << parentFeat->get_return_type()->get_name() << endl;
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