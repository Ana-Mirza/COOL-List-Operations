class Main inherits IO{
    lists : List <- new List;
    looping : Bool <- true;
    somestr : String;

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
            command : String <- stringTokenizer.nextElem(),
            index : Int,
            str : String <- "",
            i : Int <- 0,
            ls : List in {
            if stringTokenizer.hasMoreElem() then {
                -- print list at given index
                index <- new A2I.a2i(stringTokenizer.nextElem()) - 1;
                out_string("index = ".concat(new A2I.i2a(index)).concat(" length = ".concat(new A2I.i2a(lists.getLen()).concat("\n"))));

                -- TO DO: check index
                ls <- lists;
                while 0 < index loop {
                    index <- index - 1;
                    ls <- ls.tl();
                } pool;

                case ls.hd() of
                l : Cons => {str <- str.concat(l.toString()); };
                esac;
                str;
            } else { -- print all lists
                ls <- lists;
                while not ls.isNil() loop {
                    i <- i + 1;
                    case ls.hd() of
                    l : Cons => {str <- str.concat(new A2I.i2a(i)).concat(". ").concat(l.toString()); };
                    esac;
                    ls <- ls.tl();
                } pool;
                str;
            } fi;
        };
    }};

    main():Object {
        while looping loop {
            out_string("Enter command: ");
            somestr <- in_string();
            if (somestr = "help") then { out_string("load print merge filterBy sortBy".concat("\n")); } else 
            if (somestr = "load") then { lists <- lists.append(self.load()); lists.setLen(lists.getLen() + 1); } else
            if (new StringTokenizer.init(somestr).nextElem() = "print") then out_string(self.print(somestr)) else
            if (new StringTokenizer.init(somestr).nextElem() = "merge") then { lists <- lists.merge(somestr); } else
            if (somestr = "filterBy") then { out_string("filterBy\n"); } else
            if (somestr = "sortBy") then { out_string("sortBy\n"); } else
            abort()
            fi fi fi fi fi fi;
        } pool
    };
};
