    -- Tests of the utils methods
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with main; use main;


procedure tests_main is
    t: T_Tree;
begin
    -- stub_tree_1
    t := stub_tree_1;

    --assert(id > 0, "FAIL: error in generate_id -> invalid id returned");
    Put_Line("-> stub_tree_1: success");

    New_Line;
    Put_Line("tests_main: success");
end tests_main;
    
    


    
