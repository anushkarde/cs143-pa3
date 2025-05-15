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
    fat () : C {
        (new D).init(3, false)
    };
};

Class Main {
	main():C {
		{
			(new C).init(1,true);
		}
	};
};