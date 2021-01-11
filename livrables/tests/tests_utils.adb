-- Tests of the utils methods
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with utils; use utils;


procedure tests_utils is
    id, a, b, c: Integer;
begin
    -- generate_id
    id := generate_id;
    assert(id > 0, "FAIL: error in generate_id -> invalid id returned");
    Put_Line("-> generate_id: success");

    -- max
    a := 5;
    b := 1;
    c := max(a,b);
    assert(c = a, "FAIL: error in max -> invalid result");
    Put_Line("-> max: success");

    New_Line;
    Put_Line("tests_utils: success");
end tests_utils;