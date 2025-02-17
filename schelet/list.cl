class List {

    add(o : Object) : List {{
        (new Cons).init(o, self);
    }};

    hd() : Object { abort() };

    tl() : List {{ abort(); self; }};

    toString():String {
        ""
    };

    isNil() : Bool { true };

    append(list : List) : List {
        list
    };

    reverse() : List { { self; } };

    get(index : Int) : Object {{
        abort();
        self;
    }};

    getIndex(comparator: Comparator, method : String) : Int {{
        abort();
        0;
    }};

    replace(index: Int, obj : Object) : List {{
        if index = 1 then self.add(obj) else { abort(); self; } fi;
    }};

    remove(index: Int) : List {{
        abort();
        self;
    }};

    merge(str : String, lists : List) : List {{
        abort();
        self;
    }};

    filterBy(f : Filter): List {{
        self;
    }};

    sortBy(comparator : Comparator, method : String) : List { self };
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

    replace(index: Int, obj : Object) : List {{
        if index = 1 then tl.add(obj) else tl.replace(index - 1, obj).add(hd) fi;
    }};

    append(list : List) : List {{
        tl.append(list).add(hd);
    }};

    reverse() : List {
        tl.reverse().append(new List.add(hd))
    };

    isNil() : Bool { false };

    toString():String {
        let str : String <- "" in 
        {
            case hd of
            int : Int => str <- "Int(".concat(new A2I.i2a(int)).concat(")");
            string : String => str <- "String(".concat(string).concat(")");
            bool : Bool => { str <- "Bool("; if bool then str <- str.concat("true") else str <- str.concat("false") fi; str <- str.concat(")"); };
            io : IO => str <- "IO()";
            product : Product => str <- product.toString();
            rank : Rank => str <- rank.toString();
            esac;

            if tl.isNil() then str else str.concat(", ").concat(tl.toString()) fi;
        }
    };

    get(index: Int) : Object {{
        if index = 1 then hd else { tl.get(index - 1); } fi;
    }};

    remove(index : Int) : List {{
        if index = 1 then self.tl() else new List.add(hd).append(tl.remove(index - 1)) fi;
    }};

    merge(str : String, lists : List): List {{
        let stringTokenizer : StringTokenizer <- new StringTokenizer.init(str),
            cmd : String <- stringTokenizer.nextElem(),
            index1 : Int,
            index2 : Int,
            list1 : Object,
            list2 : Object,
            ls : List <- lists,
            newList : List in {

            -- get the lists
            index1 <- new A2I.a2i((stringTokenizer.nextElem()));
            index2 <- new A2I.a2i((stringTokenizer.nextElem()));
            list1 <- self.get(index1);
            list2 <- self.get(index2);

            -- merge the 2 lists
            case list1 of
            l : List => {
                newList <- l;
                case list2 of
                l2 : List => newList <- newList.append(l2); 
                esac;
            };
            esac;

            -- remove the two lists and add them at the end 
            ls <- lists;
            ls <- ls.remove(new Util.max(index1, index2)).remove(new Util.min(index1, index2)).append(new List.add(newList));
            ls;
        };
    }};

    filterBy(f : Filter):List {
        if f.filter(hd) then tl.filterBy(f).add(hd) else tl.filterBy(f) fi
    };

    getIndex(comparator : Comparator, method: String) : Int {{
        let index : Int <- 1,
            obj : Object <- self.hd(),
            list : List <- self,
            currIndex : Int <- 1 in {
            
            list <- list.tl();
            while not list.isNil() loop {
                currIndex <- currIndex + 1;

                if method = "max" then { -- get max element index
                    if comparator.compareTo(obj, list.hd()) = 1 then obj <- obj else {
                        obj <- list.hd();
                        index <- currIndex;
                    } fi; } else { -- get min element index
                    if comparator.compareTo(obj, list.hd()) = 2 then obj <- obj else {
                    obj <- list.hd();
                    index <- currIndex;
                    } fi;
                } fi; 
                list <- list.tl();
            } pool;
            index;
        };
    }};

    sortBy(comparator : Comparator, method: String):List {{
        let sortedList : List <- new List,
        oldList : List <- self,
        index : Int in {
            while not oldList.isNil() loop {
                if method = "ascendent" then index <- oldList.getIndex(comparator, "min") else
                                            index <- oldList.getIndex(comparator, "max") fi;
                sortedList <- sortedList.add(oldList.get(index));
                oldList <- oldList.remove(index);
            } pool;
            sortedList;
        };
    }};
};
