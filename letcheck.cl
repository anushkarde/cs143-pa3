class A {
    a: B;
    func(x: Int): Bool {
        {
            a <- (new B);
            a.fuck();
        }
    };
};

class Main inherits A {
    main(): Int {1};
};