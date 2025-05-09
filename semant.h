#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include <list>
#include <map>

#define TRUE 1
#define FALSE 0

class InheritanceNode;
typedef InheritanceNode *InheritanceNodeP;
class ClassTable;
typedef ClassTable *ClassTableP;


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

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  std::ostream& semant_error();
  std::ostream& semant_error(Class_ c);
  std::ostream& semant_error(Symbol filename, tree_node *t);
  bool checkInheritance(Classes classes);    // checks inheritance for a given class
  bool verifyParents(Classes classes);
};



class Environment {
  private: 
    SymbolTable<Symbol, method_class> methodTable;
    SymbolTable<Symbol, Symbol> varToType;
    Symbol currentClass;
  public: 
   def copy_environment():
    return new Environment(methodTable, varToType, currentClass);
}



Start:
  // If the current class doesn't have any parent, 
  // 



#endif
