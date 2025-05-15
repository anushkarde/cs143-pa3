-- 4. Inheritance graph may not contain cycles
-- creates a cycle: A → D → B → A

class A inherits D { 
    a: Int <- 4;
    foo(): Int {
        3
    };
};

class B inherits A {
    a: String <- "hello";
};

class C inherits A {};

class D inherits B {};