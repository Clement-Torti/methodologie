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
with gen_saver; use gen_saver;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with gen_loader; use gen_loader;

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

        Put_Line(To_String(save_person(pers1, -1)));
        Put_Line(To_String(save_person(pers1, 10)));

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

    procedure show_t(el: T_Tree) is
    begin
        show_tree(el);
    end show_t;

    package List_Tree is new p_list_gen(T_Element=> T_Tree, equal=>equal_tree, afficher_el=> show_t);
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


    -- Saver configuration
    function formatted_el_save(el: in T_Person; ancestor: in Integer) return Unbounded_String is
    begin
        return save_person(el, ancestor);
    end formatted_el_save;

    function traverse_tree is new Tree_Genealog.traverse_tree(
formatted_el=> formatted_el_save);

    function save_trees(trees: in Ptr_Cellule; file_name: in Unbounded_String) return Boolean is
        index: Integer := 0;
        cur_el: Ptr_Cellule;
        trees_str: Unbounded_String;
    begin
        cur_el := element_liste(index, trees);
        -- go throught each tree
        while (not est_vide(cur_el)) loop
            trees_str := To_Unbounded_String(
                To_String(trees_str) & 
                "arbres" &
                 LF & 
                 To_String(traverse_tree(root_liste(cur_el)))
                 );


            index := index + 1;
            cur_el := element_liste(index, trees);
        end loop;

        return save_tree(trees_str, file_name);
    end save_trees;




    function load_trees(file_name: in Unbounded_String) return Ptr_Cellule is
        My_File  : FILE_TYPE;
        line: Unbounded_String;
        trees: Ptr_Cellule;
        person: T_Person;
        tree: T_Tree;
        subTree: T_Tree;
        id: Integer;
    begin
        -- create empty tree
        trees := creer_liste_vide;
        
        -- open the file
        open(My_File, In_File, To_String(file_name) & ".txt");
        
        -- loop over it
        loop
            exit when End_Of_File(My_File);
            line := To_Unbounded_String(Get_Line(My_File));

            -- decrypt the line
            if (To_String(line) = "arbres") then
                -- This is the first element of the tree
                line := To_Unbounded_String(Get_Line(My_File));
                person := person_from_line(line, id, true);
                tree := init_tree(person);
                inserer_en_tete(tree, trees);
            else
                person := person_from_line(line, id, false);
                subTree := find_el_trees(id, trees);
                add_el_tree(person, subTree);
            end if;
        end loop;

        -- close the file
        close(My_File);
        
        return trees;
    end;

    trees: Ptr_Cellule;
    tree: T_Tree;
    subTree: T_Tree;
    pers: T_Person;
    choice: Integer := 0;
    temp: Character;
    id: Integer;
    length: Integer;
    gen: Integer;
    file_name: Unbounded_String;
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
            when 1 => -- Create tree
                pers := create_person;
                tree := init_tree(pers);
                inserer_en_tete(tree, trees);
                
                Put_Line("Tree created.");

            when 2 => -- Add parent
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
            
            when 3 => -- Get number of ancestor
                id := choose_id_console;
                subTree := find_el_trees(id, trees);

                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    length := tree_length(subTree);
                    Put_Line(Integer'Image(id) & " has " & Integer'Image(length) & " ancestor(s).");
                end if;
            
            when 4 => -- Get ancestor at a certain level
                id := choose_id_console;
                subTree := find_el_trees(id, trees);

                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    gen := choose_gen_console;
                    show_ancestors(subTree, gen);
                end if;

            when 5 => -- Show person tree
                id := choose_id_console;
                subTree := find_el_trees(id, trees);

                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    show_tree(subTree);
                end if;

            when 6 => -- Delete person (and his ancestor)
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

            when 7 => -- One parent list
                filter_trees(trees, 1);
            when 8 => -- 2 parent list
                filter_trees(trees, 2);
            when 9 =>  -- 0 parent list
                filter_trees(trees, 0);

            when 10 => -- Get all ancestor of someone
                id := choose_id_console;
                subTree := find_el_trees(id, trees);
                
                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    show_ancestors(subTree, -1);
                end if;
            
            when 11 => -- Get descendant at a certain level
                id := choose_id_console;
                subTree := find_el_trees(id, trees);

                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    gen := choose_gen_console;
                    show_descendant(subTree, gen);
                end if;

            when 12 => -- Get all descendants of someone
                id := choose_id_console;
                subTree := find_el_trees(id, trees);
                
                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    show_descendant(subTree, -1);
                end if;
        
            when 13 => -- Clear all info
                Put("Are you sure you want to erase everything (y, n)?");
                Get(temp);

                if (temp = 'y') then
                    vider_liste(trees);
                    Put_Line("Erased successfull");
                else
                    Put_Line("Erased canceled");
                end if;

            when 14 => -- Save in file
                file_name := choose_file_name;
                
                if (save_trees(trees, file_name)) then
                    Put_Line("file saved");
                else
                    Put_Line("impossible to save file");
                end if;

            when 15 => -- Load from file
                file_name := choose_file_name;
                trees := load_trees(file_name);
                afficher_liste(trees);

            when 16 => -- Show persons list
                afficher_liste(trees);

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
