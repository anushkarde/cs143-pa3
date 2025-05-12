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

class E {
	a: Int;
	quick(x: Int, y: Int) : Bool {
		true
	};
};

class D inherits E {
	a: Int;
	b: Int;
	c : D;
	c: Bool <- 3;
	fat (): Int {
		c
	};
	fat(): Bool {
		true
	};
};

class F inherits D {
	quick(x: Int, y: Int) : String {
		false
	};
};

Class Main {
	main():C {
		{
			(new C).init(1,true);
			(new D).fat();
		}
	};
};