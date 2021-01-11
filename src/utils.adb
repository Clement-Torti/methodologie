--
-- Utils function and procedure used throughout the programm
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with alea; use alea;

package body Utils is

    function generate_id return Integer is
    begin
        return Alea_1_1000;
    end;

------
    function max(a,b: in Integer) return Integer is
    begin
        if (a > b) then
            return a;
        else
            return b;
        end if;
    end;

end Utils;