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

    getMinIndex(comparator: Comparator) : Int {{
        abort();
        0;
    }};

    getMaxIndex(comparator: Comparator) : Int {{
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

    getMinIndex(comparator : Comparator) : Int {{
        let index : Int <- 1,
            obj : Object <- self.hd(),
            list : List <- self,
            currIndex : Int <- 1 in {
            
            list <- list.tl();
            while not list.isNil() loop {
                currIndex <- currIndex + 1;
                if comparator.compareTo(obj, list.hd()) = 2 then obj <- obj else {
                    obj <- list.hd();
                    index <- currIndex;
                } fi;
                list <- list.tl();
            } pool;
            index;
        };
    }};

    getMaxIndex(comparator : Comparator) : Int {{
        let index : Int <- 1,
            obj : Object <- self.hd(),
            list : List <- self,
            currIndex : Int <- 1 in {
            
            list <- list.tl();
            while not list.isNil() loop {
                currIndex <- currIndex + 1;
                if comparator.compareTo(obj, list.hd()) = 1 then obj <- obj else {
                    obj <- list.hd();
                    index <- currIndex;
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
                -- index <- oldList.getMinIndex(comparator);
                if method = "ascendent" then index <- oldList.getMinIndex(comparator) else
                                            index <- oldList.getMaxIndex(comparator) fi;
                sortedList <- sortedList.add(oldList.get(index));
                oldList <- oldList.remove(index);
            } pool;
            sortedList;
        };
    }};
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
            objectFactory : ObjectFactory,
            obj : Object in {

            while readLine loop {
                somestr <- in_string();

                -- build list with input objects
                if (somestr = "END") then readLine <- false else {
                    stringTokenizer <- new StringTokenizer.init(somestr);
                    objectFactory <- new ObjectFactory.build(stringTokenizer);
                    obj <- objectFactory.getObject();
                    list <- list.add(obj);
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
            } else { 
                -- print all lists
                ls <- lists;
                while not ls.isNil() loop {
                    i <- i + 1;
                    case ls.hd() of
                    l : List => str <- str.concat(new A2I.i2a(i)).concat(": [ ").concat(l.toString().concat(" ]\n"));
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

            -- replace with filtered list
            lists.replace(index, newList);
        };
    }};

    sort(str: String) : List {{
        let newList : Object,
        stringTokenizer : StringTokenizer <- new StringTokenizer.init(str),
        cmd : String <- stringTokenizer.nextElem(),
        index : Int <- new A2I.a2i(stringTokenizer.nextElem()),
        comparator : Comparator <- new ComparatorFactory.build(stringTokenizer.nextElem()).getObject(),
        method : String <- stringTokenizer.nextElem() in {
            -- get list at given index
            newList <- lists.get(index);

            -- sort list
            case newList of
            list : Cons => { newList <- list.sortBy(comparator, method); };
            esac;

            -- replace with sorted list
            lists.replace(index, newList);
        };
    }};

    main():Object {{
        lists <- lists.append(self.load());
        while looping loop {
            somestr <- in_string();
            if (somestr = "help") then out_string("load print merge filterBy sortBy".concat("\n")) else 
            if (somestr = "load") then lists <- lists.append(self.load()) else
            if (new StringTokenizer.init(somestr).nextElem() = "print") then out_string(self.print(somestr)) else
            if (new StringTokenizer.init(somestr).nextElem() = "merge") then { lists <- lists.merge(somestr, lists); } else
            if (new StringTokenizer.init(somestr).nextElem() = "filterBy") then { lists <- self.filter(somestr); } else
            if (new StringTokenizer.init(somestr).nextElem() = "sortBy") then { lists <- self.sort(somestr); } else
            abort()
            fi fi fi fi fi fi;
        } pool;
    }};
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

    getRank() : Int {
        0
    };

    toString():String {{
        self.type_name().concat("(").concat(name).concat(")");
    }};
};

class Private inherits Rank {
    getRank() : Int {
        1
    };
};

class Corporal inherits Private {
    getRank() : Int {
        2
    };
};

class Sergent inherits Corporal {
    getRank() : Int {
        3
    };
};

class Officer inherits Sergent {
    getRank() : Int {
        4
    };
};
(* Think of these as abstract classes *)
class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class Filter {
    filter(o : Object):Bool {true};
};

(* specific comparators and filters *)
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
        let specific_price : Int,
            generic_price : Int in {
            -- get prices
            case o of
            product : Product => { generic_price <- product@Product.getprice();
                                    specific_price <- product.getprice(); };
            esac;

            specific_price = generic_price;
        };
    }};
};

class PriceComparator inherits Comparator {
    compareTo(o1 : Object, o2 : Object):Int {{
        case o1 of
        n1 : Product => {
            case o2 of
            n2 : Product => if n1.getprice() = n2.getprice() then 0 else {
                if n1.getprice() < n2.getprice() then 1 else 2 fi; } fi;
            esac; };
        n2 : Object => {abort(); 0;};
        esac;
    }};
};

class RankComparator inherits Comparator {
    compareTo(o1 : Object, o2 : Object):Int {{
        case o1 of
        n1 : Rank => {
            case o2 of
            n2 : Rank => if n1.getRank() = n2.getRank() then 0 else { 
                if n1.getRank() < n2.getRank() then 1 else 2 fi; } fi;
            esac; };
        n2 : Object => {abort(); 0;}; 
        esac;
    }};
};

class AlphabeticComparator inherits Comparator {
    compareTo(o1 : Object, o2 : Object):Int {{
        case o1 of
        str1 : String => {
            case o2 of
            str2 : String => { 
                if str1 = str2 then 0 else {
                    if str1 < str2 then 1 else 2 fi;
                } fi;
            };
            esac;
        };
        esac;
    }};  
};

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
        let type : String,
            str : String in {
            type <- stringTokenizer.nextElem();
            if type = "Soda" then obj <- new Soda.init(stringTokenizer.nextElem(), stringTokenizer.nextElem(), new A2I.a2i(stringTokenizer.nextElem())) else
            if type = "Coffee" then obj <- new Coffee.init(stringTokenizer.nextElem(), stringTokenizer.nextElem(), new A2I.a2i(stringTokenizer.nextElem())) else
            if type = "Laptop" then obj <- new Laptop.init(stringTokenizer.nextElem(), stringTokenizer.nextElem(), new A2I.a2i(stringTokenizer.nextElem())) else
            if type = "Router" then obj <- new Router.init(stringTokenizer.nextElem(), stringTokenizer.nextElem(), new A2I.a2i(stringTokenizer.nextElem())) else
            if type = "Private" then obj <- new Private.init(stringTokenizer.nextElem()) else
            if type = "Corporal" then obj <- new Corporal.init(stringTokenizer.nextElem()) else
            if type = "Sergent" then obj <- new Sergent.init(stringTokenizer.nextElem()) else
            if type = "Officer" then obj <- new Officer.init(stringTokenizer.nextElem()) else
            if type = "String" then obj <- stringTokenizer.nextElem() else
            if type = "Int" then obj <- new A2I.a2i(stringTokenizer.nextElem()) else
            if type = "Bool" then { str <- stringTokenizer.nextElem(); if str = "true" then obj <- true else { if str = "false" then obj <- false else abort() fi; } fi; } else
            if type = "IO" then obj <- new IO else
            abort()
            fi fi fi fi fi fi fi fi fi fi fi fi;
            self;
        }
    };

    getObject(): Object { obj };
};

class FilterFactory {
    obj: Filter;

    build(type : String): SELF_TYPE {{
        -- create filter object
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

class ComparatorFactory {
    obj: Comparator;

    build(type : String): SELF_TYPE {{
        -- create comparator object
        let type : String <- type in {
            if type = "PriceComparator" then obj <- new PriceComparator else
            if type = "RankComparator" then obj <- new RankComparator else
            if type = "AlphabeticComparator" then obj <- new AlphabeticComparator else
            abort()
            fi fi fi;
            self;
        };
    }};

    getObject(): Comparator { obj };
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
