class List {
    len : Int <- 0;

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

    append(list : List) : List {
        list
    };

    reverse() : List { { self; } };

    getLen() : Int { len };

    setLen(num : Int) : SELF_TYPE {{
        len <- num;
        self;
    }};

    merge(str : String) : List {
        self
    };
};

class Cons inherits List {

    hd : Object;
    tl : List;

    hd() : Object { hd };

    tl() : List { tl };

    init(h : Object, t : List) : List {
        {
            hd <- h;
            tl <- t;
            self;
        }
    };

    append(list : List) : List {{
        tl.append(list).add(hd);
    }};

    reverse() : List {
        tl.reverse().append(new List.add(hd))
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
            str.concat("[ ".concat(self.toStringInner()).concat(" ]\n"));
        };
    }};

    merge(str : String): List {{
        -- self (* TODO *)
        let stringTokenizer : StringTokenizer <- new StringTokenizer.init(str),
            cmd : String <- stringTokenizer.nextElem(),
            index1 : String <- stringTokenizer.nextElem(),
            index2 : String <- stringTokenizer.nextElem() in {
            self;
        };
    }};

    filterBy():SELF_TYPE {
        self (* TODO *)
    };

    sortBy():SELF_TYPE {
        self (* TODO *)
    };
};
