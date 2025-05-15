-- 1. Attributes cannot be multiply defined in the same class
class A {
    x : Int <- 3;
    x : String <- "hello"; 
};

-- 2. Inherited attributes cannot be redefined
class B {
    y : Int <- 42;
};

class C inherits B {
    y : String <- "oops";
};

-- 3. Declared type of attribute must match inferred type of init expression
class D {
    z : Bool <- 123;
};

-- 4. Cannot have attributes named 'self'
class E {
    self : Int <- 5;
};

-- 5. Declared type of attribute must be defined
class F {
    a: NotDefined;
};

class Main inherits IO {
    main(): Object {
        out_string("Running attribute error tests...\n")
    };
};