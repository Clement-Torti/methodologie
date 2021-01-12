--
-- Module defining util functions to manipulate list of trees
--

package body utils_gen is
function find_el_trees(id: in Integer; trees: in Ptr_Cellule; root: in Boolean := false) return T_Tree is
        cur_el: Ptr_Cellule;
        subTree: T_Tree;
        index: Integer := 0;
    begin
        -- go trought each tree
        cur_el := element_liste(index, trees);
        while (not est_vide(cur_el)) loop
            index := index + 1;

            -- try to find the element inside it
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

            -- next tree of the list
            cur_el := element_liste(index, trees);
        end loop;

        -- no subtree found
        return subTree;
    end find_el_trees;

------
    procedure filter_trees(trees: in Ptr_Cellule; nb_parent: in Integer) is
        index: Integer := 0;
        cur_el: Ptr_Cellule;
    begin
        -- go throught each tree
        cur_el := element_liste(index, trees);
        while (not est_vide(cur_el)) loop

            -- call the filter tree on that tree
            filter_tree(root_liste(cur_el), nb_parent);

            -- get next tree
            index := index + 1;
            cur_el := element_liste(index, trees);
        end loop;

    end filter_trees;
    
------
function save_trees(trees: in Ptr_Cellule; file_name: in Unbounded_String) return Boolean is
        index: Integer := 0;
        cur_el: Ptr_Cellule;
        trees_str: Unbounded_String;
        F : File_Type;
    begin
        -- go throught each tree
        cur_el := element_liste(index, trees);
        while (not est_vide(cur_el)) loop

            -- get a string out of a tree
            trees_str := To_Unbounded_String(
                To_String(trees_str) & 
                "arbres" &
                LF &
                To_String(stringify_tree(root_liste(cur_el)))
                );

            index := index + 1;
            cur_el := element_liste(index, trees);
        end loop;

        -- save the file
        -- Open a stream into the file
        Create (F, Out_File, To_String(file_name) & ".txt");

        -- Write the string in it
        Put_Line (F, To_String(trees_str));

        -- Close the file
        Close (F);

        return true;
    end save_trees;


------
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
            -- get the next line
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

    exception
        when NAME_ERROR => Put_Line(To_String(file_name) & ".txt doesn't exist"); return trees;
        when BAD_PERSON_FORMAT => Put_Line("file corrupted"); return trees;
    end;

end utils_gen;