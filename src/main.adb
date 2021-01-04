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


PROCEDURE main IS 
    -- Tree_Bin configuration
    package Tree_Genealog is new Tree_Bin(T_Element=> T_Person, show=> show_person, id_el=>id);
    use Tree_Genealog;

    -- Provide a basic tree for tests
    function stub_tree_1 return T_Tree is
        tree: T_Tree;
        pers1: T_Person;
        pers2: T_Person;
        pers3: T_Person;
        pers4: T_Person;
    begin
        pers1.id := 1;
        pers1.name := To_Unbounded_String("clement");
        pers1.surname := To_Unbounded_String("torti");
        pers1.gender := true;

        pers2.id := 2;
        pers2.name := To_Unbounded_String("sylvie");
        pers2.surname := To_Unbounded_String("torti");
        pers2.gender := false;

        pers3.id := 3;
        pers3.name := To_Unbounded_String("serge");
        pers3.surname := To_Unbounded_String("torti");
        pers3.gender := true;

        tree := init_tree(pers1);

        add_el_tree(pers2, tree);
        add_el_tree(pers3, tree);

        return tree;
    end stub_tree_1;

    tree: T_Tree;
    subTree: T_Tree;
    pers: T_Person;
    choice: Integer := 0;
    temp: Character;
    id: Integer;
BEGIN
    tree := stub_tree_1;
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
                Put_Line("Tree created.");
            
            when 2 =>
                -- add a parent to which person ?
                Put("what's the child id ?: ");
                Get(id);

                -- This person exist ?
                subTree := find_el_tree(id, tree);
                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    -- Create a person
                    pers := create_person;
                    -- Add it to the tree
                    add_el_tree(pers, subTree);
                end if;
            
            when 6 =>
                -- add a parent to which person ?
                Put("what's the child id ?: ");
                Get(id);

                -- This person exist ?
                subTree := find_el_tree(id, tree);
                if (empty_tree(subTree)) then
                    Put("Abort (Unknown id)");
                else
                    delete_tree(subTree);
                end if;
            when 7 => filter_tree(tree, 1);
            when 8 => filter_tree(tree, 2);
            when 9 => filter_tree(tree, 0);
            when 16 => show_tree(tree, 0);
            when others => Put_Line("Unknown option");
        end case;

        -- Wait for the user to show the options again
        New_Line;
        Put("Enter any character and press enter to continue: ");
        Get(temp);
        New_Line;
    end loop;

    Put("End of program... Good bye");
END main;
