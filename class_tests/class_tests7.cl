-- 6. Classes cannot inherit from basic classes (except Object and IO)
class C inherits String { 
    a: Int <- 4;
    foo(): Int {
        3
    };
};

class B inherits Bool { 
    a: Int <- 4;
    foo(): Int {
        3
    };
};

class A inherits Int { 
    a: Int <- 4;
    foo(): Int {
        3
    };
};