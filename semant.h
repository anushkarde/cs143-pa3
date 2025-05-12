#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include <list>
#include <vector>
#include <map>

#define TRUE 1
#define FALSE 0

class InheritanceNode;
typedef InheritanceNode *InheritanceNodeP;
class ClassTable;
typedef ClassTable *ClassTableP;
class Environment;


// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.
class ClassTable : public SymbolTable<Symbol, InheritanceNode> {
private:
  int semant_errors;           // counts the number of semantic errors
  void install_basic_classes();
  std::map<Symbol, Class_> classNameMap {};    // maps each class to its name
  std::ostream& error_stream;
  bool checkInheritance(Classes classes);    // checks inheritance for a given class
  bool verifyParents(Classes classes);
  void checkMethods();
  void checkInheritedMethods(Feature childFeat, Feature parentFeat);
  std::vector<Symbol> topSortClasses();
  std::vector<Symbol> topSortedClasses {};
  std::map<Symbol, Environment*> classEnvTable {};

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  std::ostream& semant_error();
  std::ostream& semant_error(Class_ c);
  std::ostream& semant_error(Symbol filename, tree_node *t);
};

// This class is a type that hosts the environment for a given class
// by holding symbol tables for methods and attributes.
class Environment {
  private: 
    SymbolTable<Symbol, method_class> methodTable = {};
    SymbolTable<Symbol, Symbol> attribTable = {};
    Symbol currentClass;
  public:
    Environment(Symbol sym) : currentClass(sym) {}
    Environment(SymbolTable<Symbol, method_class> mT, SymbolTable<Symbol, Symbol> aT, Symbol cur) : methodTable(mT), attribTable(aT), currentClass(cur) {}
    Environment *copyEnvironment() {  return new Environment(methodTable, attribTable, currentClass); }
    void setCurrentClass(Symbol curClass) { currentClass = curClass; }
    SymbolTable<Symbol, method_class> getMethodTable () { return methodTable; }
    SymbolTable<Symbol, Symbol> getAttribTable () { return attribTable; }
};


#endif
