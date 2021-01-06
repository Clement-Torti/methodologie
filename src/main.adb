--
-- Main program launch at app's start
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with person; use person;
with tree_Bin;
with console; use console;
with p_list_gen;


PROCEDURE main IS 
    -- Tree_Bin configuration
    package Tree_Genealog is new Tree_Bin(T_Element=> T_Person, show=> show_person, id_el=>id_person);
    use Tree_Genealog;

    -- Provide a basic tree for tests
    function stub_tree_1 return T_Tree is
        tree: T_Tree;
        pers1: T_Person;
        pers2: T_Person;
        pers3: T_Person;
    begin
        pers1.id := 1;
        pers1.name := To_Unbounded_String("clement");
        pers1.surname := To_Unbounded_String("torti");
        pers1.gender := true;

        pers2.id := 2;
        pers2.name := To_Unbounded_String("sylvie");
        pers2.surname := To_Unbounded_String("torti");
        pers2.gender := true;

        pers3.id := 3;
        pers3.name := To_Unbounded_String("serge");
        pers3.surname := To_Unbounded_String("torti");
        pers3.gender := false;

        tree := init_tree(pers1);

        add_el_tree(pers2, tree);
        add_el_tree(pers3, tree);

        return tree;
    end stub_tree_1;

    -- Chained list configuration (to keep track of multiple tree)
    function equal_tree(g,d: in T_Tree) return Boolean is
        rootLeft: T_Person;
        rootRight: T_Person;
    begin
        rootLeft := root_tree(g);
        rootRight := root_tree(d);

        return id_person(rootLeft) = id_person(rootRight); 
    end equal_tree;

    procedure show_tree(el: T_Tree) is
    begin
        show_tree(el, 0);
    end show_tree;

    package List_Tree is new p_list_gen(T_Element=> T_Tree, equal=>equal_tree, afficher_el=> show_tree);
    use List_Tree;

    --------------------------------------------------------
    -- procedure find_el_trees
    -- sémantique: Find an tree element inside a list of trees
    -- parameters: eid: id of the element to find
    --              trees: the list of trees to search in
    -- return type: T_Tree
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function find_el_trees(id: in Integer; trees: in Ptr_Cellule; root: in Boolean := false) return T_Tree is
        cur_el: Ptr_Cellule;
        subTree: T_Tree;
        list: Ptr_Cellule;
        index: Integer := 0;
    begin
        -- Go trought each tree
        cur_el := element_liste(index, trees);
        while (not est_vide(cur_el)) loop
            index := index + 1;

            -- Try to find the element inside it
            subTree := find_el_tree(id, root_liste(cur_el));
            if (not empty_tree(subTree)) then
                -- return the tree containing the element
                if (root) then
                    return root_liste(cur_el);
                -- return the element itself
                else
                    return subTree;
                end if;
                
                
            end if;

            -- Next tree
            cur_el := element_liste(index, trees);
        end loop;

        -- No subtree found
        return subTree;
    end find_el_trees;

    procedure filter_trees(trees: in Ptr_Cellule; nb_parent: in Integer) is
        index: Integer := 0;
        cur_el: Ptr_Cellule;
    begin
        cur_el := element_liste(index, trees);
        while (not est_vide(cur_el)) loop
            filter_tree(root_liste(cur_el), nb_parent);

            index := index + 1;
            cur_el := element_liste(index, trees);
        end loop;

    end filter_trees;

    trees: Ptr_Cellule;
    tree: T_Tree;
    subTree: T_Tree;
    pers: T_Person;
    choice: Integer := 0;
    temp: Character;
    id: Integer;
    length: Integer;
    gen: Integer;
BEGIN
    trees := creer_liste_vide;

    -- Stub data for testing 
    tree := stub_tree_1;
    inserer_en_tete(tree, trees);

    -- General process of the application
    while (choice /= -1) loop
        -- Show options
        show_options_console;

        -- Let the user choose
        choice := choose_option_console;

        -- Deal with the choice
        case choice is
            when 1 => 
                pers := create_person;
                tree := init_tree(pers);
                inserer_en_tete(tree, trees);
                
                Put_Line("Tree created.");
            when 2 =>
                id := choose_id_console;

                -- This person exist ?
                subTree := find_el_trees(id, trees);
                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    -- Create a person
                    pers := create_person;
                    -- Add it to the tree
                    add_el_tree(pers, subTree);
                end if;
            
            when 3 =>
                id := choose_id_console;
                subTree := find_el_trees(id, trees);

                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    length := tree_length(subTree);
                    Put_Line(Integer'Image(id) & " has " & Integer'Image(length) & " ancestor(s).");
                end if;
            
            when 4 =>
                id := choose_id_console;
                subTree := find_el_trees(id, trees);

                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    gen := choose_gen_console;
                    show_ancestors(subTree, 0, gen);
                end if;

            when 5 =>
                id := choose_id_console;
                subTree := find_el_trees(id, trees);

                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    show_tree(subTree, 0);
                end if;
            when 6 =>
                id := choose_id_console;

                -- This person exist ?
                subTree := find_el_trees(id, trees, true);
                tree := find_el_trees(id, trees, false);
                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    -- is the deleted element to root element of a tree ?
                    if (equal_tree(subTree, tree)) then
                        enlever(subTree, trees);
                    else
                        delete_tree(id, subTree);
                    end if;
                    
                end if;
            when 7 => filter_trees(trees, 1);
            when 8 => filter_trees(trees, 2);
            when 9 => filter_trees(trees, 0);

            when 10 =>
                id := choose_id_console;
                subTree := find_el_trees(id, trees);
                
                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    show_ancestors(subTree, 0, -1);
                end if;
            
            when 11 =>
                id := choose_id_console;
                subTree := find_el_trees(id, trees);

                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    gen := choose_gen_console;
                    show_descendant(subTree, 0, gen);
                end if;

            when 12 =>
                id := choose_id_console;
                subTree := find_el_trees(id, trees);
                
                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    show_descendant(subTree, 0, -1);
                end if;
            
            when 13 =>
                Put("Are you sure you want to erase everything (y, n)?");
                Get(temp);

                if (temp = 'y') then
                    vider_liste(trees);
                    Put_Line("Erased successfull");
                else
                    Put_Line("Erased canceled");
                end if;

            when 16 => afficher_liste(trees);
            when others => Put_Line("Unknown option");
        end case;

        -- Wait for the user to show the options again
        New_Line;
        Put("Enter any character and press enter to continue: ");
        Get(temp);
        New_Line;
    end loop;

    Put_Line("End of program... Good bye");
END main;
