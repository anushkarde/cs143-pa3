-- STATIC DISPATCH ERROR TEST SUITE

-- 1. Static type to the left of @ does not conform to type on the right (A does not conform to B)
class A {
    foo(): Int { 1 };
};

class B {
    test(): Int {
        (new A)@B.foo()
    };
};

-- 2. Static dispatch using SELF_TYPE (invalid)
class C {
    clone(): SELF_TYPE {
        self
    };

    test(): Object {
        (new C)@SELF_TYPE.clone()
    };
};

-- 3. Valid-looking inheritance but incorrect static type
class D {
    speak(): Int { 1 };
};

class E inherits D {};

class F {
    test(): Int {
        (new D)@E.speak()
    };
};

-- 4. Dynamic type is valid but static type not
class G {
    shout(): Int { 1 };
};

class H inherits G {};

class I {
    test(): Int {
        let x: G <- new H in x@H.shout() 
    };
};

-- Static dispatch to 

-- Main entry point to make the file valid
class Main inherits IO {
    main(): Object {
        out_string("Running static dispatch error tests...\n")
    };
};