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

    get(index : Int) : Object {{
        abort();
        self;
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
            int : Int => str <- "Int(".concat("int").concat(")");
            str : String => str <- "String(".concat(str).concat(")");
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

    sortBy():SELF_TYPE {
        self (* TODO *)
    };
};
class Main inherits IO{
    lists : List <- new List;
    looping : Bool <- true;
    somestr : String;
    stringTokenizer : StringTokenizer <- new StringTokenizer;

    load(): List {{
        let list : List <- new List,
            stringTokenizer : StringTokenizer,
            readLine : Bool <- true,
            objectFactory : ObjectFactory in {

            while readLine loop {
                somestr <- in_string();

                -- build list with input objects
                if (somestr = "END") then readLine <- false else {
                    stringTokenizer <- new StringTokenizer.init(somestr);
                    objectFactory <- new ObjectFactory.build(stringTokenizer);
                    list <- list.add(objectFactory.getObject());
                }
                fi;
            } pool;
            new List.add(list.reverse());
        };
    }};

    print(str : String): String {{
        let stringTokenizer : StringTokenizer <- new StringTokenizer.init(str),
            cmd : String <- stringTokenizer.nextElem(),
            index : Int,
            str : String <- "",
            i : Int <- 0,
            ls : List in {
            if stringTokenizer.hasMoreElem() then {
                -- print list at given index
                index <- new A2I.a2i(stringTokenizer.nextElem()) - 1;
                
                ls <- lists;
                while 0 < index loop {
                    index <- index - 1;
                    ls <- ls.tl();
                } pool;

                case ls.hd() of
                l : List => {str <- str.concat("[ ").concat(l.toString()).concat(" ]\n"); };
                esac;
                str;
            } else { -- print all lists
                ls <- lists;
                while not ls.isNil() loop {
                    i <- i + 1;
                    case ls.hd() of
                    l : List => str <- str.concat(new A2I.i2a(i)).concat(". [ ").concat(l.toString().concat(" ]\n"));
                    esac;
                    ls <- ls.tl();
                } pool;
                str;
            } fi;
        };
    }};

    filter(str: String) : List {{
        let newList : Object,
        stringTokenizer : StringTokenizer <- new StringTokenizer.init(str),
        cmd : String <- stringTokenizer.nextElem(),
        index : Int <- new A2I.a2i(stringTokenizer.nextElem()),
        tmp : Int <- index,
        f : Filter <- new FilterFactory.build(stringTokenizer.nextElem()).getObject() in {
            -- get list at given index
            newList <- lists.get(index);

            -- filter list
            case newList of
            list : Cons => {
                newList <- list.filterBy(f); }; 
            esac;

            lists <- lists.replace(index, newList);
        };
    }};

    main():Object {
        while looping loop {
            out_string("Enter command: ");
            somestr <- in_string();
            if (somestr = "help") then { out_string("load print merge filterBy sortBy".concat("\n")); } else 
            if (somestr = "load") then { lists <- lists.append(self.load()); lists.setLen(lists.getLen() + 1); } else
            if (new StringTokenizer.init(somestr).nextElem() = "print") then out_string(self.print(somestr)) else
            if (new StringTokenizer.init(somestr).nextElem() = "merge") then { lists <- lists.merge(somestr, lists); } else
            if (new StringTokenizer.init(somestr).nextElem() = "filterBy") then { lists <- self.filter(somestr); } else
            if (somestr = "sortBy") then { out_string("sortBy\n"); } else
            abort()
            fi fi fi fi fi fi;
        } pool
    };
};
(*******************************
 *** Classes Product-related ***
 *******************************)
class Product {
    name : String;
    model : String;
    price : Int;

    init(n : String, m: String, p : Int):SELF_TYPE {{
        name <- n;
        model <- m;
        price <- p;
        self;
    }};

    getprice():Int{ price * 119 / 100 };

    toString():String {
        self.type_name().concat("(").concat(name).concat(",").concat(model).concat(")")
    };
};

class Edible inherits Product {
    -- VAT tax is lower for foods
    getprice():Int { price * 109 / 100 };
};

class Soda inherits Edible {
    -- sugar tax is 20 bani
    getprice():Int {price * 109 / 100 + 20};
};

class Coffee inherits Edible {
    -- this is technically poison for ants
    getprice():Int {price * 119 / 100};
};

class Laptop inherits Product {
    -- operating system cost included
    getprice():Int {price * 119 / 100 + 499};
};

class Router inherits Product {};

(****************************
 *** Classes Rank-related ***
 ****************************)
class Rank {
    name : String;

    init(n : String):SELF_TYPE {{
        name <- n;
        self;
    }};

    toString():String {{
        self.type_name().concat("(").concat(name).concat(")");
    }};
};

class Private inherits Rank {};

class Corporal inherits Private {};

class Sergent inherits Corporal {};

class Officer inherits Sergent {};
(* Think of these as abstract classes *)
class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class Filter {
    filter(o : Object):Bool {true};
};

class ProductFilter inherits Filter {
    filter(o : Object):Bool {{
        case o of
        product : Product => true;
        obj : Object => false;
        esac;
    }};
};

class RankFilter inherits Filter {
    filter(o : Object):Bool {{
        case o of
        rank : Rank => true;
        obj : Object => false;
        esac;
    }};
};

class SamePriceFilter inherits Filter {
    filter(o : Object):Bool {{
        true;
    }};
};

(* TODO: implement specified comparators and filters*)

(*******************************
 *** Classes Utils ***
 *******************************)
class StringTokenizer {
    str : String;
    hasMoreElem : Bool;

    init(string: String): StringTokenizer {{
        str <- string;
        hasMoreElem <- true;
        self;
    }};

    nextElem(): String {{
        let len : Int <- str.length(),
            i : Int <- 0,
            elem : String <- "",
            char : String <- "",
            looping : Bool <- true in
            {
            while looping loop {
                char <- str.substr(i, 1);

                -- check if reached end of a word
                if char = " " then {
                    elem <- str.substr(0, i);
                    str <- str.substr(i + 1, len - i - 1);
                    looping <- false;
                } else i <- i + 1 fi;

                -- check if reached end of the string
                if i = len then {
                    looping <- false;
                    hasMoreElem <- false;
                } else hasMoreElem <- true fi;
            } pool;

            -- return next word
            if hasMoreElem then elem else str fi;
        };
    }};

    hasMoreElem(): Bool {
        hasMoreElem
    };
};

class ObjectFactory {
    obj: Object;

    build(stringTokenizer : StringTokenizer): SELF_TYPE {
        -- create object
        let type : String in {
            type <- stringTokenizer.nextElem();
            if type = "Soda" then obj <- new Soda.init(stringTokenizer.nextElem(), stringTokenizer.nextElem(), new A2I.a2i(stringTokenizer.nextElem())) else
            if type = "Coffee" then obj <- new Coffee.init(stringTokenizer.nextElem(), stringTokenizer.nextElem(), new A2I.a2i(stringTokenizer.nextElem())) else
            if type = "Laptop" then obj <- new Laptop.init(stringTokenizer.nextElem(), stringTokenizer.nextElem(), new A2I.a2i(stringTokenizer.nextElem())) else
            if type = "Router" then obj <- new Router.init(stringTokenizer.nextElem(), stringTokenizer.nextElem(), new A2I.a2i(stringTokenizer.nextElem())) else
            if type = "Private" then obj <- new Private.init(stringTokenizer.nextElem()) else
            if type = "Corporal" then obj <- new Corporal.init(stringTokenizer.nextElem()) else
            if type = "Sergent" then obj <- new Sergent.init(stringTokenizer.nextElem()) else
            if type = "Officer" then obj <- new Officer.init(stringTokenizer.nextElem()) else
            abort()
            fi fi fi fi fi fi fi fi;
            self;
        }
    };

    getObject(): Object { obj };
};

class FilterFactory {
    obj: Filter;

    build(type : String): SELF_TYPE {{
        -- create filter
        let type : String <- type in {
            if type = "ProductFilter" then obj <- new ProductFilter else
            if type = "RankFilter" then obj <- new RankFilter else
            if type = "SamePriceFilter" then obj <- new SamePriceFilter else
            abort()
            fi fi fi;
            self;
        };
    }};

    getObject(): Filter { obj };
};

class A2I {

     c2i(char : String) : Int {
        if char = "0" then 0 else
        if char = "1" then 1 else
        if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
	if i = 0 then "0" else
	if i = 1 then "1" else
	if i = 2 then "2" else
	if i = 3 then "3" else
	if i = 4 then "4" else
	if i = 5 then "5" else
	if i = 6 then "6" else
	if i = 7 then "7" else
	if i = 8 then "8" else
	if i = 9 then "9" else
	{ abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
    a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.
*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
	if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(*
  a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
*)
     a2i_aux(s : String) : Int {
	(let int : Int <- 0 in	
           {	
               (let j : Int <- s.length() in
	          (let i : Int <- 0 in
		    while i < j loop
			{
			    int <- int * 10 + c2i(s.substr(i,1));
			    i <- i + 1;
			}
		    pool
		  )
	       );
              int;
	    }
        )
     };

(*
    i2a converts an integer to a string.  Positive and negative 
numbers are handled correctly.  
*)
    i2a(i : Int) : String {
	if i = 0 then "0" else 
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1)) 
        fi fi
    };
	
(*
    i2a_aux is an example using recursion.
*)		
    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
	    (let next : Int <- i / 10 in
		i2a_aux(next).concat(i2c(i - next * 10))
	    )
        fi
    };

};

class Util {
    min(a : Int, b : Int) : Int {{
        if a < b then a else b fi;
    }};

    max(a : Int, b : Int) : Int {{
        if a < b then b else a fi;
    }};
};
