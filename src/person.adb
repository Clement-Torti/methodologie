--
-- Module representing a person
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
    function id_person(el: in T_Person) return Integer is
    begin
        return el.id;
    end id_person;

------
    function stringify_person(el: in T_Person; ancestor_id: in Integer := -1) return Unbounded_String is
        str: Unbounded_String := To_Unbounded_String("");
    begin
        -- ancestor id
        if (ancestor_id /= -1) then
            str := To_Unbounded_String(Integer'Image(ancestor_id));
        end if;

        -- id
        str := To_Unbounded_String(To_String(str) & Integer'Image(el.id));

        -- gender
        if (el.gender) then
            str := To_Unbounded_String(To_String(str) & " 1");
        else 
            str := To_Unbounded_String(To_String(str) & " 0");
        end if;
        
        -- name
        str := To_Unbounded_String(To_String(str) & " " & To_String(el.name));

        -- surname
        str := To_Unbounded_String(To_String(str) & " " & To_String(el.surname));

        return str;
    end stringify_person;

------
    function person_from_line(line: in Unbounded_String; ancestor_id: in out Integer; root: in Boolean) return T_Person is
        pers: T_Person;
        last_index: Integer := 1;
        cur_index: integer := 2; -- Not 1 because of a strange ' ' appearing when saving
        c: Character;
        split_line: Unbounded_String;
        el: Integer := 0;
        gender: Integer;
    begin
        if (not root) then
            ancestor_id := -1;
            el := -1;
        end if;

        -- go throught every character
        loop
            exit when cur_index = length(line);
            -- get the next character
            c := Element(line, cur_index);
            
            -- if it's a space ...
            if (c = ' ') then
                -- ... we get the word
                unbounded_slice(line, split_line, last_index, cur_index);

                -- we deal with the word based on on it position
                case el is
                when -1 => -- ancestor id
                    ancestor_id := Integer'value(To_String(split_line));

                when 0 => -- personal id
                    pers.id := Integer'value(To_String(split_line));

                when 1 => -- gender
                    gender := Integer'value(To_String(split_line));

                    if (gender = 0) then
                        pers.gender := false;
                    else
                        pers.gender := true;
                    end if;

                when 2 => -- name
                    pers.name := split_line;

                when others => raise BAD_PERSON_FORMAT;
                end case;

                el := el + 1;
                last_index := cur_index + 1;
            end if;

            cur_index := cur_index + 1;
        end loop;

        -- Grab the surname finally
        unbounded_slice(line, split_line, last_index, cur_index);
        pers.surname := split_line;
        
        return pers;
    end person_from_line;

end Person;