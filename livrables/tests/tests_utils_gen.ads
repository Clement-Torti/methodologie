-- Tests of the utils_gen methods
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with utils_gen; use utils_gen;
with main; use main;
with tree_gen; use tree_gen;
with list_tree_gen; use list_tree_gen;
with person; use person;

package tests_utils_gen is
    procedure run_tests_utils_gen;
end;