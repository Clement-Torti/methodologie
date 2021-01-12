--
-- Main program launch at app's start
--
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tree_Gen; use Tree_Gen;
with person; use person;
with console; use console;
with list_tree_gen; use list_tree_gen;
with utils_gen; use utils_gen;



PACKAGE main IS
    --STUB DATA FOR TREE
    function stub_tree_1 return T_Tree;
    
    --------------------------------------------------------
    -- procedure user_program
    -- sémantique: program interacting with the user to let him manipulate gen trees
    -- parameters:  none
    -- return type: none
    -- pre-condition: none
    -- post-condition: none
    procedure user_program;

END main;