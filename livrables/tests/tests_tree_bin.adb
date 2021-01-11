-- Tests of the utils methods
with Ada.Assertions; use Ada.Assertions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with tree_bin;


procedure tests_tree_bin is
    CPT: Integer := 0;

    procedure show_int(el: in Integer; parent: Boolean := false) is
    begin
        Put(el);
        CPT := CPT + 1;
    end show_int;

    function stringify_int(el: in Integer; ancestor: in Integer) return Unbounded_String is
    begin
        return To_Unbounded_String(Integer'Image(el));
    end stringify_int;

    function id_int(el: in Integer) return Integer is
    begin
        return el;
    end id_int;

    package Tree_Int is new Tree_Bin(T_Element=> Integer, show=> show_int, id_el=>id_int);
    use Tree_Int;

    function stringify_tree is new Tree_Int.stringify_tree(
stringify_el=> stringify_int);

    t: T_Tree;
    t2: T_Tree;
    sub_t: T_Tree;
    b: Boolean;
    depth, length, num: Integer;
    str: Unbounded_String;
begin
    -- init tree
    t := init_tree(1); 
    assert(not empty_tree(t), "FAIL: error in init_tree -> invalid null tree returned");
    Put_Line("-> init_tree: success");

    -- empty tree
    b := empty_tree(t);
    assert(b = false, "Fail -> error in empty_tree -> tree not empty");
    b := empty_tree(t2);
    assert(b = true, "Fail -> error in empty_tree -> tree empty");
    Put_Line("-> empty_tree: success");

    -- tree_depth
    depth := tree_depth(t);
    assert(depth = 1, "Fail -> error in tree_depth -> wrong depth");
    depth := tree_depth(t2);
    assert(depth = 0, "Fail -> error in tree_depth -> wrong depth");
    Put_Line("-> tree_depth: success");

    
    -- tree_length
    length := tree_length(t);
    assert(length = 1, "Fail -> error in tree_length -> wrong length");
    length := tree_length(t2);
    assert(length = 0, "Fail -> error in tree_length -> wrong length");
    Put_Line("-> tree_length: success");


    -- exist_el_tree
    b := exist_el_tree(1, t);
    assert(b = true, "Fail -> error in exist_el_tree -> 1 is in fact in the tree");
    b := exist_el_tree(1, t2);
    assert(b = false, "Fail -> error in exist_el_tree -> 1 isn't in the tree");
    Put_Line("-> exist_el_tree: success");
    
    
    -- find_el_tree
    sub_t := find_el_tree(-1, t);
    assert(empty_tree(sub_t), "Fail -> error in find_el_tree -> find an unexisting tree");
    sub_t := find_el_tree(1, t);
    assert(not empty_tree(sub_t), "Fail -> error in find_el_tree -> didn't find an element of the tree");
    
    Put_Line("-> find_el_tree: success");
    

    -- add_el_tree
    add_el_tree(2, t);
    sub_t := find_el_tree(2, t);
    assert(not empty_tree(sub_t), "Fail -> error in add_el_tree -> element don't added");
    
    Put_Line("-> add_el_tree: success");
    
    -- show tree
    CPT := 0;
    show_tree(t);
    assert(CPT = 2, "Fail -> error in show_tree -> didn't show the right number of element");
    CPT := 0;
    show_tree(t2);
    assert(CPT = 0, "Fail -> error in show_tree -> didn't show the right number of element");
   
    New_Line;
    Put_Line("-> show_tree: success");

    
    -- show_ancestors
    CPT := 0;
    show_ancestors(t, 1);
    assert(CPT = 1, "Fail -> error in show_ancestor -> didn't show the right number of element");
    CPT := 0;
    show_ancestors(t, 2);
    assert(CPT = 0, "Fail -> error in show_ancestor -> didn't show the right number of element");
   
    New_Line;
    Put_Line("-> show_ancestor: success");
    
    
    -- show_descendant
    CPT := 0;
    sub_t := find_el_tree(2, t);
    show_descendant(sub_t, 1);
    assert(CPT = 1, "Fail -> error in show_descendant -> didn't show the right number of element");
    CPT := 0;
    show_ancestors(t2, 1);
    assert(CPT = 0, "Fail -> error in show_descendant -> didn't show the right number of element");
   
    New_Line;
    Put_Line("-> show_descendant: success");
    

    -- filter_tree
    add_el_tree(3, t);
    sub_t := find_el_tree(2, t);
    add_el_tree(4, sub_t);

    CPT := 0;
    filter_tree(t, 1);
    assert(CPT = 1, "Fail -> error in filter_tree -> didn't show the right number of element");
    
    CPT := 0;
    filter_tree(t, 2);
    assert(CPT = 1, "Fail -> error in filter_tree -> didn't show the right number of element");
   
    CPT := 0;
    filter_tree(t, 0);
    assert(CPT = 2, "Fail -> error in filter_tree -> didn't show the right number of element");
   
    New_Line;
    Put_Line("-> filter_tree: success");
    
    -- delete_tree
    delete_tree(2, t);
    length := tree_length(t);
    assert(length = 2, "Fail -> error in delete_tree -> didn't delete the element properly");

    New_Line;
    Put_Line("-> delete_tree: success");


    -- root tree
    num := root_tree(t);
    assert(num = 1, "Fail -> error in root_tree -> wrong root returned");

    New_Line;
    Put_Line("-> root_tree: success");


    -- stringify_tree
    str := stringify_tree(t);
    assert(" 1" & LF & " 3" & LF = To_String(str), "Fail -> error in stringify_tree -> wrong string");

    New_Line;
    Put_Line("-> root_tree: success");

    -- clear tree
    clear_tree(t);
    assert(empty_tree(t), "Fail -> error in clear_tree -> tree not cleared");

    New_Line;
    Put_Line("-> clear_tree: success"); 

    New_Line;
    Put_Line("tests_tree_bin: success");
end tests_tree_bin;