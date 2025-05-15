class B {

};

class A {
    x: String;
    y: Int;
    z: Bool;
    w: B;
    p: SELF_TYPE;
    fun(): Bool {
        {
            p + y;      --cannot compare non ints 
            y + y;      --comparing ints is fine
            w = p;      --this is fine
            w < p;      --this is bad
            w <= p;     --this is bad
            z = x;      --this is bad
            z = true;   --this is okay
            y = 3;      --this is okay
            x = "poop"; --this is okay
            y = w;
        }
    };
};

class Main {
    main(): Int {1};
};