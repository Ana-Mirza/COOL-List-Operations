class List {
    add(o : Object) : List {{
        (new Cons).init(o, self);
    }};

    hd() : Object { abort() };

    tl() : List {{ abort(); self; }};

    toString():String {
        ""
    };

    toStringInner():String {
        ""
    };

    isNil() : Bool { true };
};

class Cons inherits List {

    (* TODO: store data *)
    hd : Object;
    tl : List;

    car : Int;
    cdr : List;

    hd() : Object { hd };

    tl() : List { tl };

    init(h : Object, t : List) : List {
        {
            hd <- h;
            tl <- t;
            self;
        }
    };

    isNil() : Bool { false };

    toStringInner():String {
        let str : String <- "" in 
        {
            case hd of
            int : Int => str <- "Int(".concat("int").concat(")");
            str : String => str <- "String(".concat(str).concat(")");
            product : Product => str <- product.toString();
            rank : Rank => str <- rank.toString();
            esac;

            if tl.isNil() then str else str.concat(", ").concat(tl.toStringInner()) fi;
        }
    };

    toString():String {{
        -- "[TODO: implement me]"
        let str : String <- "" in {
        -- while hd.type_name() = "Cons" loop { 
            case hd of
            l : Cons => {str.concat("[ ".concat(l.toStringInner()).concat(" ]\n")).concat(tl.toString()); };
            esac;
        -- } pool; 
        -- str;
        };
    }};

    merge(other : List):SELF_TYPE {
        self (* TODO *)
    };

    filterBy():SELF_TYPE {
        self (* TODO *)
    };

    sortBy():SELF_TYPE {
        self (* TODO *)
    };
};
