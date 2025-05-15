-- 10. main method must be in class Main (and not inherit from a parent class)
class A {
    main(): Object {
        out_string("Wrong place\n")
    };
};

class Main inherits A {
    nothing(): Object { 0 };
};