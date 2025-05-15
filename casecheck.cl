class A {
    func(x: Int): Bool {
        {
            case 3 of x: B => x+8; y: Int => y+x; esac;
            case 3 of x:SELF_TYPE=>self; esac;
            case 3 of self:Bool => true; x:Bool => false; esac;        --can't have duplicate branches in case statement
        }
    };
};

class Main inherits A {
    main(self: A): Int {1};
};