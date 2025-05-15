class A{
    a:Int;
}; 

class C inherits A {
    a:Bool;
};

class B inherits D {

};

class D inherits F {

};

class F inherits B {

};

class Main{
    main():Bool{false};
};