-- Tests of the utils methods
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with person; use person;


procedure tests_person is
    p, p2: T_Person;
    id, ancestor_id: Integer;
    str: Unbounded_String;
begin
    -- create_person
    Put_Line("insert: 'y', then: 'toto', then: 'tata'");
    p := create_person;

    assert(p.gender = false, "FAIL: error in create_person -> invalid person gender");
    assert(p.name = "toto", "FAIL: error in create_person -> invalid person name");
    assert(p.surname = "tata", "FAIL: error in create_person -> invalid person surname");
    
    Put_Line("-> create_person: success");

    -- id_person
    id := id_person(p);
    assert(p.id = id, "FAIL: error in id_person -> invalid person id returned");
    Put_Line("-> id_person: success");

    -- stringify_person
    str := stringify_person(p, 1);
    assert(To_String(str) = " 1" & Integer'Image(p.id) & " 0 toto tata", "FAIL: error in stringify_person -> invalid conversion of person");
    Put_Line("-> stringify_person: success");

    -- person_from_line
    p2 := person_from_line(str, ancestor_id, false);
    assert(ancestor_id = 1, "FAIL: error in person_from_line -> invalid ancestor_id returned");
    assert(p2.gender = false, "FAIL: error in person_from_line -> invalid person gender");
    assert(p2.name = "toto ", "FAIL: error in person_from_line -> invalid person name");
    assert(p2.surname = "tata", "FAIL: error in person_from_line -> invalid person surname");
    
    Put_Line("-> person_from_line: success");

    New_Line;
    Put_Line("tests_persons: success");
end tests_person;