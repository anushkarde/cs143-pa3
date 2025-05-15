-- 6. Classes cannot inherit from SELF_TYPE

class A inherits SELF_TYPE { 
    a: Int <- 4;
    foo(): Int {
        3
    };
};