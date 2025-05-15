class A {
	a : Int <- 1;
	b : Bool;
	c : String <-"hello";

	foo(str: D): Bool {
		true
	};
};

class B inherits A {
	x : Bool;
    otherfunc(x: String, x: Int): Int {
		{
			x.otherfunc();
			(new P).foo((new A));
		}
    };
};

class BB inherits B {
	x : Bool;
    otherfunc(x: Int): Yare {
		{
			x.otherfunc();
			(new P).foo((new A));
		}
    };
};

class C inherits A {
	x : Bool <- 6;
	aa : A;
    otherfunc(): Int {
        {
			(self)@A.fooll(true);
			aa <- (new C);
		}
    };
};