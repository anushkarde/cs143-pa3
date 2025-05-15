class A {
	a : Int;
	b : Bool;
    matcha: SELF_TYPE;
    hello: A <- (new SELF_TYPE);
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
    shawty(self: Int): Bool {
        {  
            matcha <- (new A); 
            true;
        }
    };
};

class Main inherits IO {
    main(x: Int): Object {
        -- LCA(C, D) = A, so z: A
        let x: SELF_TYPE, y: SELF_TYPE, z: A in z
    };
};