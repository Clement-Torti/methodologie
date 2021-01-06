--
--  Module allowing to save genealogic' trees
--
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with person; use person;


package gen_saver is
    function save_person(el: in T_Person; ancestor_id: in Integer := -1) return Unbounded_String;
    function save_tree(tree: in Unbounded_String; file_name: in Unbounded_String) return Boolean;
end gen_saver;