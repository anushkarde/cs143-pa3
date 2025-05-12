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
	b: self.quick(3, false);
	quick(x: Int, x: Bool) : Bool {
		false
	};
};

class F inherits D {
	quick(x: Int, y: Bool) : Bool {
		false
	};
};

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};