--
-- Generic module implementation of a binary Tree
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body Tree_Bin is
    function init_tree return T_Tree is
        tree: T_Tree;
    begin
        return tree;
    end init_tree;
    
end Tree_Bin;