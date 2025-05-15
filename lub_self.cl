class A {
	a : Int;
	b : Bool;
};

class B inherits A {
    foo(): Int {
        3 + 4
    };
};

class C inherits B {
    bar(): String {
        "hi riya"
    };
};

class D inherits A {
    shawty(): Bool {
        true
    };
};

class Main inherits IO {
    main(x: Int): Object {
        -- LCA(C, D) = A, so z: A
        let x: SELF_TYPE, y: D <- new D, z: A in z
    };
};