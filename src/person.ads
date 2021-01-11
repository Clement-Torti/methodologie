--
-- Module representing a person
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Person is
    BAD_PERSON_FORMAT: Exception;

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
    function id_person(el: in T_Person) return Integer;

    --------------------------------------------------------
    -- procedure stringify_person
    -- sémantique: convert a person into a string
    -- parameters: el: the  person to convert
    --              ancestor_id: the ancestor id of the person
    -- return type: Unbounded_String
    -- pre-condition: none
    -- post-condition: none
    function stringify_person(el: in T_Person; ancestor_id: in Integer := -1) return Unbounded_String;
    
    --------------------------------------------------------
    -- procedure person_from_line
    -- sémantique: convert a string into a person
    -- parameters: line: the string to convert
    --              ancestor_id: provide the ancestor id
    --              root: indicating if it's the root element
    -- return type: T_Person
    -- pre-condition: none
    -- post-condition: none
    -- exception: BAD_PERSON_FORMAT
    function person_from_line(line: in Unbounded_String; ancestor_id: in out Integer; root: in Boolean) return T_Person;

end Person;