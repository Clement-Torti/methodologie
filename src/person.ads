--
-- Module representing a person
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Person is
    type T_Person is record
       id: Integer; -- Unique identifier
       gender: Boolean; -- 0: Man  1: Woman
       name: Unbounded_String;
       surname: Unbounded_String;
       -- birthdate: Date (for futur implementation)
    end record;

    --------------------------------------------------------
    -- procedure show_person
    -- sémantique: show person info
    -- parameters: el: the person to show
    --             parent: Boolean indicating if we want to place "mother/father" before
    -- return type: none
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    procedure show_person(el: in T_Person; parent: in Boolean);

    --------------------------------------------------------
    -- procedure create_person
    -- sémantique: create a new person
    -- parameters: none
    -- return type: T_Person
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function create_person return T_Person;

    --------------------------------------------------------
    -- procedure id
    -- sémantique: get the if of a person
    -- parameters: el: the person
    -- return type: Integer
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function id(el: in T_Person) return Integer;
end Person;