-- 1. SELF_TYPE as a method parameter type
class D {
    bad1(x: SELF_TYPE): Object { x }; -- not allowed
};

-- 2. SELF_TYPE as return value of a let expression
class E {
    oops(): SELF_TYPE {
        let x: SELF_TYPE <- 5 in x
    };
};

-- 3. SELF_TYPE in case branch
class F {
    bad_case(): Object {
        case self of
            x: SELF_TYPE => x; -- not allowed
        esac
    };
};

class Main inherits IO {
    main(): Object {
        out_string("Running SELF_TYPE error tests\n")
    };
};