class A {
    a: A;
    b: String;
    func(x: Int): Int {
        {
            self <- 3;
            a <- (new C);
            a <- (new SELF_TYPE);
            y <- true;
            self <- true;
            b <- 3;
        }
    };
};

class B inherits A {

};

class C inherits B {

};

class Main {
    main(): Int {1};
};