-- 1. assign to self
class O inherits IO {
    main(): Object {
        self <- 5
    };
};

-- 2. self bound in a let
class Y {
    foo(): Object {
        let self: Int <- 5 in self -- ❌ illegal use of `self` in let binding
    };
};

-- 3. assignment mismatch
class X {
    a: Bool <- true;
    bar(s: String): Int {
        a <- 5
    };
};

-- Dispatch tests

-- 1. Calling a method that doesnt exist
class A {
    foo(): Int { 5 };
};

class B inherits A {
    bar(): Int {
        (new A).not_in_A()
    };
};

-- 2. Call method with wrong number of arguments
class C {
    greet(name: String): String { name };
};

class D {
    call_wrong_arity(): String {
        (new C).greet("hi", "extra")
    };
};

-- 3. Wrong type for argument
class E {
    double(x: Int): Int { x + x };
};

class F {
    call_wrong_type(): Int {
        (new E).double("hello")
    };
};

-- 4. SELF_TYPE return misused (expected static type is not caller)
class G {
    clone(): SELF_TYPE {
        self
    };
};

class H {
    test(): G {
        let x: G <- (new G).clone() in x
    };
};

-- 5. Dispatching on object whose static type doesnt have the method
class I {};
class J {
    use(): Int {
        let x: I <- new I in x.cool() -- ❌ 'cool' not defined in class I
    };
};

-- 6. Dispatch where method exists in subclass, but static type is parent class
class K {
    say(): Int { 1 };
};

class L inherits K {
    say(y: Int): Int { y };
};

class M {
    test(): Int {
        let x: K <- new L in x.say(5)
    };
};

-- Main class to make the file compile
class Main inherits IO {
    main(): Object {
        out_string("Running dispatch error tests...\n")
    };
};