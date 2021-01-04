--
-- Main program launch at app's start
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tree_Bin;


PROCEDURE main IS 
    -- Defining the content of the Tree
    type Person is record
       id: Integer; -- Unique identifier
       gender: Boolean; -- 0: Man  1: Woman
       name: Unbounded_String;
       surname: Unbounded_String;
       -- birthdate: Date (for futur implementation)
    end record;

    procedure show_person(el: in Person) is
    begin
        Put(el.id);
        Put("   " & To_String(el.name));
        Put("   " & To_String(el.surname));
        Put("   gender: ");
        if (el.gender) then
            Put("woman");
        else
            Put("man");
        end if;

        -- Put(el.birthdate);
    end show_person;

    package Tree_Genealog is new Tree_Bin(T_Element=> Person, show=> show_person);
    use Tree_Genealog;

    arbre: T_Tree;
    pers: Person;
BEGIN
    pers.id := 1;
    pers.name := To_Unbounded_String("Clément");
    pers.surname := To_Unbounded_String("Torti");
    pers.gender := false;

    show_person(pers);

END main;
