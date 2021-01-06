--
--  Module allowing to load genealogic' trees
--
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with person; use person;


package gen_loader is
    BAD_FORMAT: Exception;
    function person_from_line(line: in Unbounded_String; ancestor_id: in out Integer; root: in Boolean) return T_Person;
end gen_loader;