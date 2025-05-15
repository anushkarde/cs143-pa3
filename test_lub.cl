class A {
    poo: Int <- self;
}; 

class C {
    poo:Bool;
    fun(): Bool {
        self.fun()
    };
};

class B inherits C {
    self(x: Bool, y: String): D {
       case 1 of x:T=>x+1; x:IO=>4; x:IO=>5; esac
    };
};

class Main inherits A {
    main(): Int { (new B)@C.fun() };
};