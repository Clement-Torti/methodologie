-- Tests of the utils_gen methods

package body tests_utils_gen is
    procedure run_tests_utils_gen is
        t, t2: T_Tree;
        trees, trees2: Ptr_Cellule;
        id: Integer;
        p: T_Person;
        c: Character;
        str: Unbounded_String;
    begin
        t := stub_tree_1;
        trees := creer_liste_vide;
        inserer_en_tete(t, trees);

        --  find_el_trees
        id := 2;
        t2 := find_el_trees(id, trees);
        p := root_tree(t2);
        assert(p.id = id, "FAIL: error in find_el_tree -> invalid tree returned");

        t2 := find_el_trees(id, trees, true);
        p := root_tree(t2);
        assert(p.id = 1, "FAIL: error in find_el_tree -> invalid tree returned");

        New_Line;
        Put_Line("-> find_el_tree: success");


        -- filter_trees
        New_Line;
        Put_Line("testing filter_trees:");

        Put_Line("with 0 parents (2 elements should appear)");
        filter_trees(trees, 0);
        Get(c);

        Put_Line("with 1 parents (0 element should appear)");
        filter_trees(trees, 1);
        Get(c);

        Put_Line("with 2 parents (1 element should appear)");
        filter_trees(trees, 2);
        Get(c);

        New_Line;
        Put_Line("-> filter_trees: you tell if it was a success");


        -- save_trees / load_trees
        str := To_Unbounded_String("gen_test");
        assert(save_trees(trees, str), "Fail -> error while saving trees");

        trees2 := load_trees(str);
        t2 := root_liste(trees2);
        assert(equal_tree(t, t2), "Fail -> error while loading trees");
        New_Line;
        Put_Line("-> save/load_trees: success");

        New_Line;
        Put_Line("tests_utils_gen: success");
    end run_tests_utils_gen;
end;