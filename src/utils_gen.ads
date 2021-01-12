--
-- Module defining util function to manipulate list of trees
--
with Ada.Text_IO; use Ada.Text_IO;
with person; use Person;
with Tree_Gen; use Tree_Gen;
with List_Tree_Gen; use List_Tree_Gen;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package utils_gen is
    -- A method to go throught every element of one tree
    function stringify_tree is new Tree_Gen.stringify_tree(
stringify_el=> stringify_person);

    --------------------------------------------------------
    -- procedure find_el_trees
    -- sémantique: Find an tree element inside a list of trees
    -- parameters: id: id of the element to find
    --              root: if tree, return the root of the tree containing the element. If false return the subtree.
    --              trees: the list of trees to search in
    -- return type: T_Tree
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function find_el_trees(id: in Integer; trees: in Ptr_Cellule; root: in Boolean := false) return T_Tree;


    --------------------------------------------------------
    -- procedure filter_trees
    -- sémantique: apply the filter_tree methods (see tree_bin.adb) to a list of trees
    -- parameters: trees: the list of trees
    --              nb_parent: the number of parent an element should have to be displayed
    -- return type: none
    -- pre-condition: none
    -- post-condition: none
    procedure filter_trees(trees: in Ptr_Cellule; nb_parent: in Integer);

    --------------------------------------------------------
    -- procedure save_trees
    -- sémantique: save trees inside a file 
    -- parameters: trees: the list of trees
    --              file_name: the name of the file containing trees
    -- return type: Boolean indicating success
    -- pre-condition: none
    -- post-condition: none
    function save_trees(trees: in Ptr_Cellule; file_name: in Unbounded_String) return Boolean;

    --------------------------------------------------------
    -- procedure load_trees
    -- sémantique: load trees from a .txt file
    -- parameters:  file_name: the name of the file containing trees
    -- return type: Ptr_Cellule (null if unsuccessfull)
    -- pre-condition: none
    -- post-condition: none
    function load_trees(file_name: in Unbounded_String) return Ptr_Cellule;

end utils_gen;