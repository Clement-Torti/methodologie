-- Tests of the main methods

package body tests_main is
    procedure run_tests_main is
        t: T_Tree;
        trees: Ptr_Cellule;
        p: T_Person;
    begin
        -- 1. Create tree
        New_Line;
        Put_Line("Create tree testing");
        Put_Line("insert: '1', 'n', 'bobo', 'baba', 'd', '0'");

        user_program(trees);

        assert(not est_vide(trees), "Failed: error create_tree -> trees still empty after element creation");

        -- grab the created element
        t := root_liste(trees);
        p := root_tree(t);
        assert(p.gender = true, "Failed: error create_tree -> wrong element creation");
        assert(To_String(p.name) = "bobo", "Failed: error create_tree -> wrong element creation");
        assert(To_String(p.surname) = "baba", "Failed: error create_tree -> wrong element creation");

        New_Line;
        Put_Line("-> create tree: success");

        -- 2.Add parent
        New_Line;
        Put_Line("Add parent testing");
        Put_Line("insert: '2', '" & Integer'Image(p.id) & "', 'n', 'bibi', 'baba', 'd', '0'");

        user_program(trees);

        assert(tree_depth(t) = 2, "Failed: error add_parent -> parent not added properly");
        
        New_Line;
        Put_Line("-> add parent: success");

       

        -- 3. Get number of ancestor
        New_Line;
        Put_Line("Get number of ancestor");
        Put_Line("insert: '3', '" & Integer'Image(p.id) & "', 'd', '0'");
        Put_Line("should be displayed 2");

        user_program(trees);

        New_Line;
        Put_Line("-> get number of ancestor: You're in charge of saying if it was successfull");

        -- 4. Get ancestor at a certain level"
        New_Line;
        Put_Line("Get ancestor at level");
        Put_Line("insert: '4', '" & Integer'Image(p.id) & "', '1', 'd', '0'");
        Put_Line("should be displayed: bibi baba");

        user_program(trees);

        New_Line;
        Put_Line("-> get ancestor at level: You're in charge of saying if it was successfull");

        -- 5. Show person tree
        New_Line;
        Put_Line("Show person tree");
        Put_Line("insert: '5', '" & Integer'Image(p.id) & " 'd', '0'");
        Put_Line("should be displayed the genealogic tree of bobo");

        user_program(trees);

        New_Line;
        Put_Line("-> show person tree: You're in charge of saying if it was successfull");


        -- 7. One parent list
        New_Line;
        Put_Line("One parent list");
        Put_Line("insert: '7', 'd', '0'");
        Put_Line("should be displayed bobo");

        user_program(trees);

        New_Line;
        Put_Line("-> One parent list: You're in charge of saying if it was successfull");

        -- 8. 2 parents list
        New_Line;
        Put_Line("2 parent list");
        Put_Line("insert: '8', 'd', '0'");
        Put_Line("should be displayed nothing");

        user_program(trees);

        New_Line;
        Put_Line("-> 2 parent list: You're in charge of saying if it was successfull");


        -- 9. 0 parent list
        New_Line;
        Put_Line("0 parent list");
        Put_Line("insert: '9', 'd', '0'");
        Put_Line("should be displayed bibi");

        user_program(trees);

        New_Line;
        Put_Line("-> 0 parent list: You're in charge of saying if it was successfull");

        -- 10. Get all ancestor of someone
        New_Line;
        Put_Line("Get all ancestor of someone");
        Put_Line("insert: '10', '" & Integer'Image(p.id) & " 'd', '0'");
        Put_Line("should be displayed bobo et bibi");

        user_program(trees);

        New_Line;
        Put_Line("-> Get all ancestor of someone: You're in charge of saying if it was successfull");


        -- 11. Get descendant at a certain level

        New_Line;
        Put_Line("Get descendant at level");
        Put_Line("insert: '11', '<id bibi>', '1', 'd', '0'");
        Put_Line("should be displayed: bobo");

        user_program(trees);

        New_Line;
        Put_Line("-> get descendant at level: You're in charge of saying if it was successfull");


        -- 12. Get all descendants of someone
        New_Line;
        Put_Line("Get all descendant at level");
        Put_Line("insert: '12', '<id bibi>', '1', 'd', '0'");
        Put_Line("should be displayed: bobo");

        user_program(trees);

        New_Line;
        Put_Line("-> get all descendant: You're in charge of saying if it was successfull");


        -- 6. Delete person (and his ancestor)
        New_Line;
        Put_Line("Delete person");
        Put_Line("insert: '6', '<id bibi>', 'd', '0'");

        user_program(trees);

        assert(tree_length(t) = 1, " Fail -> error in delete person -> wrong tree length after deletion");

        New_Line;
        Put_Line("-> delete person success");

        
        -- 14. Save in file
        New_Line;
        Put_Line("Save in file");
        Put_Line("insert: '14', 'gen', 'd', '0'");
        Put_Line("A file called gen.txt should be created, we will see later if it works properly");

        user_program(trees);

        New_Line;

        -- 13. Clear all info
        New_Line;
        Put_Line("Clear all info");
        Put_Line("insert: '13', 'y', 'd', '0");

        user_program(trees);

        assert(est_vide(trees), " Fail -> error in clear all info -> tree wasn't clear");

        New_Line;
        Put_Line("-> clear info success");

        -- 15. Load from file
        New_Line;
        Put_Line("Load from file");
        Put_Line("insert: '15', 'gen', 'd', '0'");

        user_program(trees);
        t := root_liste(trees);

        assert(tree_length(t) = 1, "Failed: load file or save file-> trees hasn't been loaded properly");

        New_Line;
        Put_Line("-> save and load success");
        

        -- 16. Show persons list
        New_Line;
        Put_Line("Show persons list");
        Put_Line("insert: '16', 'd', '0'");
        Put_Line("should be displayed: bobo");

        user_program(trees);

        New_Line;
        Put_Line("-> Show person list: You're in charge of saying if it was successfull");


        New_Line;
        Put_Line("tests_main: success");
    end run_tests_main;
end;    
    


    
