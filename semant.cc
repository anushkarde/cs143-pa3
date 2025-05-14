

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include "cool-tree.h"
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
  mapEnvironments();

  /** type checking */
  doTypeCheck();
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

void ClassTable::mapEnvironments() {
  for (Symbol curClass : topSortedClasses) {
    Environment *curEnv = nullptr;
    /** if the environment has a parent, copy the parent's environment */
    if(classNameMap[curClass]->get_parent() != No_class)
    {
      Symbol parent = classNameMap[curClass]->get_parent();
      curEnv = classEnvTable[parent]->copyEnvironment();
      curEnv->setCurrentClass(curClass);
    } else {
      curEnv = new Environment(curClass);
    }
    Features featureList = classNameMap[curClass]->get_features();
    /** iterate over features */
    curEnv->getMethodTable().enterscope(); 
    curEnv->getAttribTable().enterscope();
    for (int i = featureList->first(); featureList->more(i); i = featureList->next(i)) {
      Feature curFeat = featureList->nth(i);
      if (curFeat->is_method()) { 
        if (curEnv->getMethodTable().probe(curFeat->get_name()) != NULL) {
          semant_error() << "Method " << curFeat->get_name() << " is multiply defined." << endl;
        } else if (curEnv->getMethodTable().lookup(curFeat->get_name()) != NULL) {
          if (curFeat->checkInheritedMethods(this, curEnv->getMethodTable().lookup(curFeat->get_name()))) {
            curFeat->addToTable(curEnv);
          }
        } else {
          curFeat->addToTable(curEnv);
        }
      }
      else {  
        if (curFeat->get_name() == self) {
          semant_error() << "\'self\' cannot be the name of an attribute." << endl;
        } else if (curEnv->getMethodTable().probe(curFeat->get_name()) != NULL) {
          semant_error() << "Attribute " << curFeat->get_name() << " is multiply defined in class." << endl;
        } else if (curEnv->getMethodTable().lookup(curFeat->get_name()) != NULL) {
          semant_error() << "Attribute " << curFeat->get_name() << " is an attribute of an inherited class." << endl;
        } else {
          curFeat->addToTable(curEnv);
        }               
      }
    }
    classEnvTable[curClass] = curEnv;
  }
}

/**
  Returns the least common ancestor of two classes.
 */
Symbol ClassTable::leastCommonAncestor(Symbol type1, Symbol type2) {
  if (type1 == type2) { return type1; }
  if (type1 == _BOTTOM_) { return type2; }
  if (type2 == _BOTTOM_) { return type2; }

  std::set<Symbol> ancestors;
  Class_ curr = classNameMap[type1];
  Symbol parent = nullptr;
  while (curr != nullptr && curr->get_name() != No_class) {
    ancestors.insert(curr->get_name());
    parent = curr->get_parent();
    curr = classNameMap[parent];
  }

  curr = classNameMap[type2];
  while (curr != nullptr && curr->get_name() != No_class) {
    if (ancestors.count(curr->get_name())) {
      return curr->get_name();
    } 
    parent = curr->get_parent();
    curr = classNameMap[parent];
  }

  return Object;
}

/**
  This method checks whether the current method is inherited properly from
  the passed in parent method. 
 */
bool method_class::checkInheritedMethods(ClassTable *classtable, method_class *parentFeat) {
  Formals childFormals = this->get_formals();
  Formals parentFormals = parentFeat->get_formals();
  bool validOverride = true;
  if (childFormals->len() != parentFormals->len()) {
    classtable->semant_error() << "Incompatible number of formal parameters in redefined method " << parentFeat->get_name() << endl;
    return false;
  }
  /** keep track of parent formal types, we want same # and types of arguments */
  int i = parentFormals->first();
  int k = childFormals->first();
  for ( ; parentFormals->more(i), childFormals->more(k); i = parentFormals->next(i), k = childFormals->next(k)) {
    Formal parentForm = parentFormals->nth(i);
    Formal childForm = childFormals->nth(k);
    Symbol origType = parentForm->get_type();
    Symbol childType = childForm->get_type();
    if (childType != origType) {
      classtable->semant_error() << "In redefined method " << parentFeat->get_name() << ", parameter type " << childType->get_string() << " is different from original type " << origType->get_string() << endl;
      validOverride = false;
    }
  }
  /** we also want both methods to have the same return type */ 
  if (this->get_return_type() != parentFeat->get_return_type()) {
    classtable->semant_error() << "In redefined method " << parentFeat->get_name() << ", return type " << this->get_return_type()->get_string() << " is different from original return type " << parentFeat->get_return_type()->get_string() << endl;
    validOverride = false;
  }
  return validOverride;
}

void ClassTable::doTypeCheck() {
  /** go through all classes, perform type checking using their environments */
  for (Symbol curClass : topSortedClasses) {
    Features featureList = classNameMap[curClass]->get_features();
    for (int i = featureList->first(); featureList->more(i); i = featureList->next(i)) {
      Feature f = featureList->nth(i);
      f->checkFeatureType(this, classEnvTable[curClass]);
    }
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
/** declarations for type checking */

/** adding to method table for the given environment */
void method_class::addToTable(Environment *env) {
  env->getMethodTable().addid(name, this);
}

/** adding to the attribution table for the given environemnt */
void attr_class::addToTable(Environment *env) {
  env->getAttribTable().addid(name, &type_decl);
}

/** type check an attribute */
void attr_class::checkFeatureType(ClassTable *classtable, Environment *env) {
  Symbol declared_type = type_decl;
  if (classtable->classEnvTable.find(declared_type) == classtable->classEnvTable.end()) {
    classtable->semant_error() << "Class " << declared_type->get_string() << "of attribute " << name->get_string() << " is undefined." << endl;
  }
  SymbolTable<Symbol, Symbol>& attribTable = env->getAttribTable();
  attribTable.enterscope();
  attribTable.addid(self, &SELF_TYPE);
  Symbol expr_type = init->checkType(classtable, env);
  if (expr_type == No_type) {
    return;
  }
  if (classtable->leastCommonAncestor(expr_type, declared_type) != declared_type) {
    classtable->semant_error() << "Inferred type of " << expr_type->get_string() << " of initialization of attribute " << name->get_string() << " does not conform to declared type " << declared_type->get_string() << "." << endl;
  }
  attribTable.exitscope();
}

/** type check a method */
void method_class::checkFeatureType(ClassTable *classtable, Environment *env) { 
  SymbolTable<Symbol, Symbol>& attribTable = env->getAttribTable();
  attribTable.enterscope();
  attribTable.addid(self, &SELF_TYPE);
  std::set<Symbol> paramNames = {};
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    Formal f = formals->nth(i);
    if (paramNames.find(f->get_name()) != paramNames.end()) {
      classtable->semant_error() << "Formal parameter " << f->get_name()->get_string() << " is multiply defined." << endl;
    } else {
      paramNames.insert(f->get_name());
      Symbol formType = f->get_type();
      attribTable.addid(f->get_name(), &formType);
    }
    if (classtable->classEnvTable.find(f->get_type()) == classtable->classEnvTable.end()) {
      classtable->semant_error() << "Class " << f->get_type()->get_string() << " of formal parameter" << f->get_name()->get_string() << "is undefined." << endl;
    } 
  }
  Symbol inferType = expr->checkType(classtable, env);
  Symbol decl_ret_type = return_type;
  if (return_type == SELF_TYPE) { decl_ret_type = env->getCurrentClass(); }
  if (classtable->classEnvTable.find(decl_ret_type) == classtable->classEnvTable.end()) { 
    classtable->semant_error() << "Undefined return type " << return_type->get_string() << " in method " << name->get_string() << "." << endl;
  } else if (classtable->leastCommonAncestor(inferType, decl_ret_type) != decl_ret_type) {
    classtable->semant_error() << "Inferred return type " << inferType->get_string() << " of method " << name->get_string() << " does not conform to declared return type " << return_type->get_string() << endl;
  }
  attribTable.exitscope();
}

/** type checking for assignment expression */
Symbol assign_class::checkType(ClassTable *classtable, Environment *env) {
  SymbolTable<Symbol, Symbol>& curAttribTable = env->getAttribTable();
  Symbol inferType = expr->checkType(classtable, env);
  if (curAttribTable.lookup(name) == NULL) {
    classtable->semant_error() << "Assignment to undeclared variable " << name->get_string() << "." << endl;
    type = _BOTTOM_;
  } else {
    if (classtable->leastCommonAncestor(inferType, *(curAttribTable.lookup(name))) == *(curAttribTable.lookup(name))) {
      type = inferType;
    } else {
      classtable->semant_error() << "Type " << inferType->get_string() << " of assigned expression does not conform to declared type " << (*(curAttribTable.lookup(name)))->get_string() << " of identifier " << name->get_string() << "." << endl;
      type = _BOTTOM_;
    }
  }
  return type;
}

/** type checking for static dispatch */
Symbol static_dispatch_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol origCallerType = expr->checkType(classtable, env);
  Symbol callerType = origCallerType;
  std::vector<Symbol> paramTypes = {};
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression curExpr = actual->nth(i);
    paramTypes.push_back(curExpr->checkType(classtable, env));
  }
  if (callerType == SELF_TYPE) {
    callerType = env->getCurrentClass();
  }

  /** if the caller type doesn't exist throw an error */
  if (classtable->classEnvTable.find(callerType) == classtable->classEnvTable.end()) {
    classtable->semant_error() << "Static dispatch on type " << callerType->get_string() << "not allowed." << endl;
    type = _BOTTOM_;
    return type;
  }
  
  /** if the method we are trying to call is not one we inherit, throw error */
  Symbol parentType = type_name;
  if (classtable->leastCommonAncestor(parentType, callerType) !=  parentType) {
    classtable->semant_error() << "Expression type " << callerType->get_string() << "does not conform to declared static dispatch type " << parentType->get_string() << "." << endl;
    type = _BOTTOM_;
    return type;
  }

  /**  get method from parent table */
  Environment *parentEnv = classtable->classEnvTable[parentType];
  SymbolTable<Symbol, method_class>& parentMethTable = parentEnv->getMethodTable();
  if (parentMethTable.lookup(name) == NULL) {  
    classtable->semant_error() << "Static dispatch to undefined method " << name->get_string() << "." << endl;
    type = _BOTTOM_;
  } else {
    method_class parentMethod = *(parentMethTable.lookup(name));
    Formals forms = parentMethod.get_formals();
    if (size_t(forms->len()) != paramTypes.size()) {
      classtable->semant_error() << "Method " << name->get_string() << " called with wrong number of arguments." << endl;
      type = _BOTTOM_;
    } else {
      /** iterate through formals and check types */
      bool error_found = false;
      for (int i = forms->first(); forms->more(i); i = forms->next(i)) {
        Formal form = forms->nth(i);
        Symbol form_type = forms->nth(i)->get_type();
        if (classtable->leastCommonAncestor(form_type, paramTypes[i]) != form_type) {
          classtable->semant_error() << "In call of method " << name->get_string() << ", type " << paramTypes[i]->get_string() << " of parameter " << form->get_name()->get_string() << " does not conform to declared type " << form_type->get_string() << endl;
          type = _BOTTOM_;
          error_found = true;
        }
      }
      if (!error_found) {
        if (parentMethod.get_return_type() == SELF_TYPE) {
          type = origCallerType;
        } else {
          type = parentMethod.get_return_type();
        }
      }
    }
  }
  return type; 

}

/** type checking for dispatch */
Symbol dispatch_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol origCallerType = expr->checkType(classtable, env);
  Symbol callerType = origCallerType;
  /** collect all the passed in parameter types */
  std::vector<Symbol> paramTypes = {};
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    Expression curExpr = actual->nth(i);
    paramTypes.push_back(curExpr->checkType(classtable, env));
  }
  if (callerType == SELF_TYPE) {
    callerType = env->getCurrentClass();
  }
  /** if the caller type doesn't exist throw an error */
  if (classtable->classEnvTable.find(callerType) == classtable->classEnvTable.end()) {
    if (callerType == _BOTTOM_) {
      classtable->semant_error() << "Dispatch on type " << callerType->get_string() << "not allowed. The type _bottom is the type of throw expressions." << endl;
    } else {
      classtable->semant_error() << "Dispatch on undefined class " << callerType->get_string() << "." << endl;
    }
    type = _BOTTOM_;
    return type;
  }
  Environment *callerEnv = classtable->classEnvTable[callerType];
  SymbolTable<Symbol, method_class>& callerMethTable = callerEnv->getMethodTable();
  if (callerMethTable.lookup(name) == NULL) {  
    classtable->semant_error() << "Dispatch to undefined method " << name->get_string() << "." << endl;
    type = _BOTTOM_;
  } else {
    method_class callerMethod = *(callerMethTable.lookup(name));
    Formals forms = callerMethod.get_formals();
    if (size_t(forms->len()) != paramTypes.size()) {
      classtable->semant_error() << "Method " << name->get_string() << " called with wrong number of arguments." << endl;
      type = _BOTTOM_;
    } else {
      /** iterate through formals and check types */
      bool error_found = false;
      for (int i = forms->first(); forms->more(i); i = forms->next(i)) {
        Formal form = forms->nth(i);
        Symbol form_type = forms->nth(i)->get_type();
        if (classtable->leastCommonAncestor(form_type, paramTypes[i]) != form_type) {
          classtable->semant_error() << "In call of method " << name->get_string() << ", type " << paramTypes[i]->get_string() << " of parameter " << form->get_name()->get_string() << " does not conform to declared type " << form_type->get_string() << endl;
          type = _BOTTOM_;
          error_found = true;
        }
      }
      if (!error_found) {
        if (callerMethod.get_return_type() == SELF_TYPE) {
          type = origCallerType;
        } else {
          type = callerMethod.get_return_type();
        }
      }
    }
  }
  return type; 
}

/**  type checking for conditional statements */
Symbol cond_class::checkType(ClassTable *classtable, Environment *env) {
  if (pred->checkType(classtable, env) != Bool) {
    classtable->semant_error() << "If statements must have a boolean predicate." << endl;
  } else {
    Symbol type_e1 = then_exp->checkType(classtable, env);
    Symbol type_e2 = else_exp->checkType(classtable, env);
    if (type_e2 == No_type) {
      type = type_e1;
    } else {
      type = classtable->leastCommonAncestor(type_e1, type_e2);
    }
  }
  return type;
}

/** type checking for loops */
Symbol loop_class::checkType(ClassTable *classtable, Environment *env) {
  if (pred->checkType(classtable, env) != Bool) {
    classtable->semant_error() << "Loop condition does not have type Bool." << endl;
  }
  body->checkType(classtable, env);
  type = Object;
  return type;
}

/** type checking for branches of cases */
Symbol branch_class::checkCaseType(ClassTable *classtable, Environment *env) {
  SymbolTable<Symbol, Symbol>& curAttribTable = env->getAttribTable();
  curAttribTable.enterscope();
  curAttribTable.addid(name, &type_decl); 
  Symbol expr_type = expr->checkType(classtable, env);
  curAttribTable.exitscope();
  return expr_type;
}

/** type checking for typcase */
Symbol typcase_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol firstType = expr->checkType(classtable, env);
  std::set<Symbol> branchTypes;
  /** iterate over cases and check types for each case */
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case c = cases->nth(i);
    Symbol curCaseType = c->checkCaseType(classtable, env);
    Symbol decl_type = c->get_type_decl();
    if (branchTypes.find(decl_type) != branchTypes.end()) {
      classtable->semant_error() << "Duplicate branch " << decl_type->get_string() << " in case statement." << endl;
    } else {
      branchTypes.insert(decl_type);
    }
    firstType = classtable->leastCommonAncestor(curCaseType, firstType);
  }
  type = firstType;
  return type;
}

/** type checking for block expressions */
Symbol block_class::checkType(ClassTable *classtable, Environment *env) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    Expression exp = body->nth(i);
    type = exp->checkType(classtable, env);
  }
  return type;
}

/** type checking for let bindings */
Symbol let_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1Type = init->checkType(classtable, env);
  SymbolTable<Symbol, Symbol>& attribTable = env->getAttribTable();     // enter new scope for let bindings
  attribTable.enterscope();
  attribTable.addid(identifier, &type_decl);
  Symbol lcaCheck = type_decl;
  if (e1Type != No_type) {
    if (type_decl == SELF_TYPE) {
      lcaCheck = env->getCurrentClass();
    }
    if (classtable->leastCommonAncestor(e1Type, lcaCheck) != lcaCheck) {
      classtable->semant_error() << "Inferred type " << e1Type->get_string() << " of initialization of " << identifier->get_string() << " does not conform to identifier\'s declared type " << type_decl->get_string() << "." << endl;
    }
  }
  type = body->checkType(classtable, env);
  attribTable.exitscope();
  return type;
}

/** type checking for plus class */
Symbol plus_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1_type = e1->checkType(classtable, env);
  Symbol e2_type = e2->checkType(classtable, env);
  if (e1_type == Int && e2_type == Int) {
    type = Int;
  } else {
    type = _BOTTOM_;
    classtable->semant_error() << "non-Int arguments: " << e1_type->get_string() << "+" << e2_type->get_string() << endl;
  }
  return type;
}

/** type checking for sub class */
Symbol sub_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1_type = e1->checkType(classtable, env);
  Symbol e2_type = e2->checkType(classtable, env);
  if (e1_type == Int && e2_type == Int) {
    type = Int;
  } else {
    type = _BOTTOM_;
    classtable->semant_error() << "non-Int arguments: " << e1_type->get_string() << "-" << e2_type->get_string() << endl;
  }
  return type;
}

/** type checking for mult class */
Symbol mul_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1_type = e1->checkType(classtable, env);
  Symbol e2_type = e2->checkType(classtable, env);
  if (e1_type == Int && e2_type == Int) {
    type = Int;
  } else {
    type = _BOTTOM_;
    classtable->semant_error() << "non-Int arguments: " << e1_type->get_string() << "*" << e2_type->get_string() << endl;
  }
  return type;
}

/** type checking for divide class */
Symbol divide_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1_type = e1->checkType(classtable, env);
  Symbol e2_type = e2->checkType(classtable, env);
  if (e1_type == Int && e2_type == Int) {
    type = Int;
  } else {
    type = _BOTTOM_;
    classtable->semant_error() << "non-Int arguments: " << e1_type->get_string() << "/" << e2_type->get_string() << endl;
  }
  return type;
}

/** type checking for neg class */
Symbol neg_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1_type = e1->checkType(classtable, env);
  if (e1_type == Int) {
    type = Int;
  } else {
    type = _BOTTOM_;
    classtable->semant_error() << "Argument of \'~\' has type " << e1_type->get_string() << " instead of Int." << endl;
  }
  return type;
}

/** type checking for lt class */
Symbol lt_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1_type = e1->checkType(classtable, env);
  Symbol e2_type = e2->checkType(classtable, env);
  if (e1_type != Int || e2_type != Int) {
    type = _BOTTOM_;
    classtable->semant_error() << "non-Int arguments: " << e1_type->get_string() << "<" << e2_type->get_string() << endl;
  } else {
    type = Bool;
  }
  return type;
}

/**  type checking for eq class */
Symbol eq_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1_type = e1->checkType(classtable, env);
  Symbol e2_type = e2->checkType(classtable, env);
  if (e1_type == Int || e2_type == Int || e1_type == Str || e2_type == Str || e1_type == Bool || e2_type == Bool) {
    if (e1_type == e2_type) {
      type = Bool;
    } else {
      classtable->semant_error() << "Illegal comparison with a basic type." << endl;
      type = _BOTTOM_;
    }
  } else {
    type = Bool;
  }
  return type;
}


/** type checking for leq class */ 
Symbol leq_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol e1_type = e1->checkType(classtable, env);
  Symbol e2_type = e2->checkType(classtable, env);
  if (e1_type != Int || e2_type != Int) {
    type = _BOTTOM_;
    classtable->semant_error() << "non-Int arguments: " << e1_type->get_string() << "<=" << e2_type->get_string() << endl;
  } else {
    type = Bool;
  }
  return type;
}

/** type checking for NOT expressions */
Symbol comp_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol expr_type = e1->checkType(classtable, env);
  if (expr_type == Bool) {
    type = Bool;
  } else {
    type = _BOTTOM_;
    classtable->semant_error() << "Argument of \'not\' has type " << expr_type->get_string() << " instead of Bool." << endl;
  }
  return type;
}

/** type checking for integer constants */
Symbol int_const_class::checkType(ClassTable *classtable, Environment *env) {
  type = Int;
  return type;
}

/** type checking for bool constants */
Symbol bool_const_class::checkType(ClassTable *classtable, Environment *env) {
  type = Bool;
  return type;
}

/** type checking string constants */
Symbol string_const_class::checkType(ClassTable *classtable, Environment *env) {
  type = Str;
  return type;
}

/** type checking for new_class */
Symbol new__class::checkType(ClassTable *classtable, Environment *env) {
  if (type_name != SELF_TYPE && classtable->classEnvTable.find(type_name) == classtable->classEnvTable.end()) {
    classtable->semant_error() << "\'new\' used with undefined class " << type_name->get_string() << "." << endl;
  }
  type = type_name;
  return type;
}

/** type checking for isVoid */
Symbol isvoid_class::checkType(ClassTable *classtable, Environment *env) {
  Symbol sym = e1->checkType(classtable, env);
  type = Bool;
  return type;
}

/** no_expr has no type */ 
Symbol no_expr_class::checkType(ClassTable *classtable, Environment *env) {
  type = _BOTTOM_;
  return type;
}

/** type checking for object identifiers */
Symbol object_class::checkType(ClassTable *classtable, Environment *env) {
  type = _BOTTOM_;
  if (name == self) {
    type = SELF_TYPE;
    return type;
  }

  /** if not self type, lookup in table */
  if (env->getAttribTable().lookup(name) != NULL) {
    type = *(env->getAttribTable().lookup(name));
  } else {
    classtable->semant_error() << "Undeclared identifier " << name->get_string() << ".";
  }
  return type;
}