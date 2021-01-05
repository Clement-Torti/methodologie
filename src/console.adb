--
-- Module containing the user interface functions/procedures
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body console is
    procedure show_options_console is
    begin
        Put_Line("-----------------------------------------");
        Put_Line("**** Welcome to the Genealogic tree ****");
        New_Line;
        Put_Line("Choose an option:");
        
        -- must have options
        Put_Line("  1. Create tree");
        Put_Line("  2. Add parent");
        Put_Line("  3. Get number of ancestor");
        Put_Line("  4. Get ancestor at a certain level");
        Put_Line("  5. Show person tree");
        Put_Line("  6. Delete person (and his ancestor)");
        Put_Line("  7. One parent list");
        Put_Line("  8. 2 parents list");
        Put_Line("  9. 0 parent list");
        Put_Line("  10. Get all ancestor of someone");
        Put_Line("  11. Get descendant at a certain level");
        Put_Line("  12. Get all descendants of someone");


        -- additional options
        Put_Line("  13. Clear all info");
        Put_Line("  14. Save in file");
        Put_Line("  15. Load from file");
        Put_Line("  16. Show persons list");
    end show_options_console;

------
    function choose_option_console return Integer is
        choice: Integer;
    begin
        New_Line;
        Put("What's your choice ?: ");
        Get(choice);

        if (choice <= 0 or choice > NBOPTION) then
            raise BAD_CHOICE;
        end if;

        Put_Line("-----------------------------------------");
        return choice;
    exception
        when BAD_CHOICE => skip_line; return choose_option_console;
        when DATA_ERROR => skip_line; return choose_option_console;
    end choose_option_console;

------
    function choose_id_console return Integer is
        id: Integer;
    begin
        New_Line;
        Put("what's the child id ?: ");
        Get(id);

        return id;
    exception
        when DATA_ERROR => skip_line; return choose_id_console;
    end choose_id_console;

------
    function choose_gen_console return Integer is
        gen: Integer;
    begin
        New_Line;
        Put("what generation do you want ?: ");
        Get(gen);

        if (gen < 0) then
            raise BAD_CHOICE;
        end if;

        return gen;
    exception
        when BAD_CHOICE => skip_line; return choose_gen_console;
        when DATA_ERROR => skip_line; return choose_gen_console;
    end choose_gen_console;

end console;