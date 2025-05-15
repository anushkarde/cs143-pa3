-- 3. Cannot redefine attributes from parent class
class A {
    a: Int <- 4;
    foo(): Int {
        3
    };
};

class B inherits A {
    a: String <- "hello";
};

class Main inherits IO {
    main(): Object {
        {
            out_string("Hello from Main!\n");
            0;
        }
    };
};