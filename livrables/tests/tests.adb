--
-- Procedure running all tests
--
with Ada.Text_IO; use Ada.Text_IO;
with tests_utils; use tests_utils;
with tests_tree_bin; use tests_tree_bin;
with tests_person; use tests_person;
with tests_console; use tests_console;
with tests_utils_gen; use tests_utils_gen;
with tests_main; use tests_main;


procedure tests is
    c: Character;
begin
    Put_Line("----------------------------------------");
    Put_Line("Running all tests");
    New_Line;
    Put_Line("tests will be paused at some point, you have to write the information provided as an entry for the tests to run properly");

    New_Line;
    Put_Line("----------------------------------------");
    Put_Line("Utils testing, enter something to start: ");
    Get(c);
    run_tests_utils;

    New_Line;
    Put_Line("----------------------------------------");
    Put_Line("Tree_bin testing, enter something to start: ");
    Get(c);
    run_tests_tree_bin;

    New_Line;
    Put_Line("----------------------------------------");
    Put_Line("Person testing, enter something to start: ");
    Get(c);
    run_tests_person;

    New_Line;
    Put_Line("----------------------------------------");
    Put_Line("Console testing, enter something to start: ");
    Get(c);
    run_tests_console;

    New_Line;
    Put_Line("----------------------------------------");
    Put_Line("Utils_gen testing, enter something to start: ");
    Get(c);
    run_tests_utils_gen;

    New_Line;
    Put_Line("----------------------------------------");
    Put_Line("Main testing, enter something to start: ");
    Get(c);
    run_tests_main;


    New_Line;
    Put_Line("All tests successfull");
    New_Line;
end;