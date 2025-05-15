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
		while "ehllo" loop true pool
	};
};

class D inherits E {
	a: Int;
	b: Int;
	c: Bool <- "str";
	fat(): Bool {
		c
	};
	fat (): Int {
		c
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