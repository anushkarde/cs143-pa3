-- 1. Methods cannot be multiply defined in the same class
class A {
    foo(): Int { 1 };
    foo(): String { "oops" };
};

-- 2. Identifiers used in formal parameter list must be distinct
class B {
    bar(x: Int, x: Bool): Object { x }; 
};

-- 3. Method body must conform to declared return type
class C {
    baz(): Bool { 42 };
};

-- 4. Inherited methods must match arg count/types and return type
class D {
    ping(x: Int): Bool { x };
};

-- incompatible parameter type
class E inherits D {
    ping(x: String): Bool { true };
};

-- incompatible number of parameters 
class G inherits D {
    ping(x: Int, s: String): Bool { true };
};

-- incompatible formal type 
class I inherits D {
    ping(s: String): Bool { true };
};

-- incompatible return type
class H inherits D {
    ping(x: Int): Int { 6 };
};

-- 5. Cannot use 'self' as a formal parameter name
class F {
    bad(self: Int): Int { self }; 
};

class Main inherits IO {
    main(): Object {
        out_string("Running method error tests...\n")
    };
};