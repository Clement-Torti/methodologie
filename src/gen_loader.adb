--
--  Module allowing to load genealogic' trees
--

package body gen_loader is
    function person_from_line(line: in Unbounded_String; ancestor_id: in out Integer; root: in Boolean) return T_Person is
        pers: T_Person;
        last_index: Integer := 1;
        cur_index: integer := 1;
        c: Character;
        split_line: Unbounded_String;
        el: Integer := 0;
        gender: Integer;
    begin
        if (not root) then
            ancestor_id := -1;
            el := -1;
        end if;

        loop
            exit when cur_index = length(line);
            -- get the next character
            c := Element(line, cur_index);
            

            if (c = ' ') then
                unbounded_slice(line, split_line, last_index, cur_index);

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

                when 3 => -- surname
                    pers.surname := split_line;

                when others => raise BAD_FORMAT;
                end case;

                el := el + 1;
                last_index := cur_index + 1;
            end if;

            cur_index := cur_index + 1;
        end loop;

        return pers;
    end person_from_line;


end gen_loader;