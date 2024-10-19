class Main inherits IO{
    lists : List <- new List;
    looping : Bool <- true;
    somestr : String;

    load(): Object {{
        let list : List <- new List,
            stringTokenizer : StringTokenizer,
            readLine : Bool <- true,
            objectFactory : ObjectFactory in {

            while readLine loop {
                somestr <- in_string();

                -- build list
                if (somestr = "END") then readLine <- false else {
                    stringTokenizer <- new StringTokenizer.init(somestr);
                    objectFactory <- new ObjectFactory.build(stringTokenizer);
                    list <- list.add(objectFactory.getObject());
                }
                fi;
            } pool;
            list;
        };
    }};

    main():Object {
        while looping loop {
            out_string("Enter command: ");
            somestr <- in_string();
            if (somestr = "help") then { out_string("load print merge filterBy sortBy".concat("\n")); } else 
            if (somestr = "load") then { lists <- lists.add(self.load()); } else
            if (somestr = "print") then out_string(lists.toString()) else
            if (somestr = "merge") then { out_string("merge\n"); } else
            if (somestr = "filterBy") then { out_string("filterBy\n"); } else
            if (somestr = "sortBy") then { out_string("sortBy\n"); } else
            abort()
            fi fi fi fi fi fi;
        } pool
    };
};