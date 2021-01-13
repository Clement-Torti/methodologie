-- Tests of the tree_bin methods
with Ada.Assertions; use Ada.Assertions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with tree_bin;

package tests_tree_bin is
    procedure run_tests_tree_bin;
end;