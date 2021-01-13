--
-- Main program launch at app's start
--

package body main is
    --------------------------------------------------------
    --STUB DATA FOR TREE
    --
    function stub_tree_1 return T_Tree is
        tree: T_Tree;
        pers1: T_Person;
        pers2: T_Person;
        pers3: T_Person;
    begin
        -- create persons
        pers1.id := 1;
        pers1.name := To_Unbounded_String("henri");
        pers1.surname := To_Unbounded_String("dupont");
        pers1.gender := true;

        pers2.id := 2;
        pers2.name := To_Unbounded_String("sandrine");
        pers2.surname := To_Unbounded_String("marchant");
        pers2.gender := true;

        pers3.id := 3;
        pers3.name := To_Unbounded_String("herve");
        pers3.surname := To_Unbounded_String("dupont");
        pers3.gender := false;

        -- create the tree
        tree := init_tree(pers1);

        -- add the persons to the tree
        add_el_tree(pers2, tree);
        add_el_tree(pers3, tree);

        return tree;
    end stub_tree_1;

------
    procedure user_program(trees: in out Ptr_Cellule) is
        tree: T_Tree;
        subTree: T_Tree;
        pers: T_Person;
        choice: Integer := -1;
        temp: Character;
        id: Integer;
        length: Integer;
        gen: Integer;
        file_name: Unbounded_String;
    begin
        --trees := creer_liste_vide;

        -- General process of the application
        while (true) loop
            -- Show options
            show_options_console;

            -- Let the user choose
            choice := choose_option_console;

            -- Deal with the choice
            case choice is     
                when 0 => Put_Line("End of program... Good bye"); return;        
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
    end user_program;

------
    procedure user_program is 
        trees: Ptr_Cellule;
    begin
        user_program(trees);
    end user_program;

end main;