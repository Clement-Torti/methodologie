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
        Put_Line("  3. Get number of ancester");
        Put_Line("  4. Get ancester at a certain level");
        Put_Line("  5. Show person tree");
        Put_Line("  6. Delete person (and his ancester)");
        Put_Line("  7. One parent list");
        Put_Line("  8. 2 parents list");
        Put_Line("  9. 0 parent list");
        Put_Line("  10. Get all ancester of someone");
        Put_Line("  11. Get descendant at a certain level");
        Put_Line("  12. Get all ancester of someone");


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
            skip_line;
            return choose_option_console;
        end if;

        Put_Line("-----------------------------------------");
        return choice;
    exception
        when DATA_ERROR => skip_line; return choose_option_console;
    end choose_option_console;

end console;