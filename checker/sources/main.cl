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
            } else { -- print all lists
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

            lists <- lists.replace(index, newList);
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
            list : Cons => {
                if method = "descendent" then newList <- list.sortBy(comparator, method).sortBy(comparator, method) else newList <- list.sortBy(comparator, method) fi;
                };
            esac;

            lists <- lists.replace(index, newList);
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
