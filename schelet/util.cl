(* Think of these as abstract classes *)
class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class Filter {
    filter(o : Object):Bool {true};
};

(* TODO: implement specified comparators and filters*)
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
            product : Product => { generic_price <- product@Product.getprice(); specific_price <- product.getprice(); };
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
            n2 : Product => if n1.getprice() <= n2.getprice() then 0 else 1 fi;
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
            n2 : Rank => if n1.getRank() <= n2.getRank() then 0 else 1 fi;
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
            str2 : String => { if str1.substr(0, 1) < str2.substr(0, 1) then 0 else {
                if str2.substr(0, 1) < str1.substr(0, 1) then 1 else {
                    if str2.substr(0, 1) = str1.substr(0, 1) then {
                        if str1.length() = 1 then { if str2.length() = 1 then 0 else 0 fi; } else { if str2.length() = 1 then 1 else { self.compareTo(str1.substr(1, str1.length() - 1), str2.substr(1, str2.length() - 1)); } fi; } fi;
                    } else 1 fi;
                } fi; } fi;
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
            -- if type = "Bool" then { str <- stringTokenizer.nextElem(); if str = "true" then obj <- true else {if str = "false" then obj <- false else abort() fi; }; } else
            abort()
            fi fi fi fi fi fi fi fi fi fi fi;
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

class ComparatorFactory {
    obj: Comparator;

    build(type : String): SELF_TYPE {{
        -- create filter
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
