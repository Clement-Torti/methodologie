-- Tests of the utils methods
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with console; use console;

procedure tests_utils is
    option, id, gen: Integer;
    file_name: Unbounded_String;
begin
    -- choose_option_console
    Put_Line("insert: 'd', then: '-2', then '1");
    option := choose_option_console;
    assert(option >= 0 and option < 17, "FAIL: error in choose_option_console -> invalid option returned");
    Put_Line("-> choose_option_console: success");

    -- choose_id_console
    New_Line;
    Put("insert: 'd', then: '1");
    id := choose_id_console;
    Put_Line("-> choose_id_console: success");

    -- choose_gen_console
    New_Line;
    Put_Line("insert: 'd', then: '-2', then: '1'");
    gen := choose_gen_console;
    assert(gen > 0, "FAIL: error in choose_gen_console -> invalid gen returned");
    --Put_Line("-> choose_gen_console: success");

    -- choose_file_name
    New_Line;
    Put_Line("insert: '', then: 'gen'");
    file_name := choose_file_name;
    assert(To_String(file_name) = "gen", "FAIL: error in choose_file_name -> invalid file_name returned");
    --Put_Line("-> choose_file_name: success");

    New_Line;
    New_Line;
    Put_Line("tests_console: success");
end tests_utils;