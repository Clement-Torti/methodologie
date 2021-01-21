--
-- Utils function and procedure used throughout the programm
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Utils is
    --------------------------------------------------------
    -- fonction generate_id
    -- sémantique: return large integer
    -- parameters: none
    -- return type: Integer
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function generate_id return Integer;

    --------------------------------------------------------
    -- fonction max
    -- sémantique: the maximum between 2 values
    -- parameters: a, b: the numbers to compare
    -- return type: Integer
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function max(a,b: in Integer) return Integer;
end Utils;