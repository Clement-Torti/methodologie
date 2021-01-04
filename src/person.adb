--
-- Module representing a person
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with utils; use utils;

package body Person is
    procedure show_person(el: in T_Person; parent: in Boolean) is
        begin
            if (parent) then
                Put("-- ");
                if (el.gender) then
                    Put("mother: ");
                else
                    Put("father: ");
                end if;
            end if;

            Put("" & Integer'Image(el.id));
            Put("   " & To_String(el.name));
            Put("   " & To_String(el.surname));

            -- Put(el.birthdate);
            New_Line;
    end show_person;

------
    function create_person return T_Person is
        pers: T_Person;
        choice: Character;
    begin
        -- id
        pers.id := generate_id;

        -- gender
        Put_Line("You must create a person: ");
        Put("is the person a man (y, n) ?: ");
        Get(choice);
        if (choice = 'y') then
            pers.gender := false;
        else 
            pers.gender := true;
        end if;

        -- name
        New_Line;
        skip_line;
        Put("Enter a name: ");
        pers.name := To_Unbounded_String(Get_Line);

        -- surname
        New_Line;
        Put("Enter a surname: ");
        pers.surname := To_Unbounded_String(Get_Line);
        New_Line;

        return pers;
    end create_person;

------
    function id(el: in T_Person) return Integer is
    begin
        return el.id;
    end id;
end Person;