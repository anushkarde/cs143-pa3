class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

class A {
	a : Int <- 1;
	b : Bool;
	c : String <-"hello";

	foo(): Bool {
		true
	};
};

class B inherits A {
	x: String;
	p: A;

	fatty(): Bool {
		{
			case x = "cat" of luchia: Int => 4; anushka: SELF_TYPE => 7; esac;
			true;
		}
	};
};

class D inherits B {
	riya: Int;
};

Class Main {
	main():C {
		{
			(new C).init(1,true);
		}
	};
};