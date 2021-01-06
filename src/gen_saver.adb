--
--  Module allowing to save genealogic' trees
--

package body gen_saver is
    function save_person(el: in T_Person; ancestor_id: in Integer := -1) return Unbounded_String is
        str: Unbounded_String := To_Unbounded_String("");
    begin
        -- ancestor id
        if (ancestor_id /= -1) then
            str := To_Unbounded_String("" & Integer'Image(ancestor_id));
        end if;

        -- id
        str := To_Unbounded_String(To_String(str) & " " & Integer'Image(el.id));

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
    end save_person;

    function save_tree(tree: in Unbounded_String; file_name: in Unbounded_String) return Boolean is
        F : File_Type;
    begin
        Create (F, Out_File, To_String(file_name) & ".txt");
        Put_Line (F, To_String(tree));
        Close (F);

        return true;
    end save_tree;
end;