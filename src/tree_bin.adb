--
-- Generic module implementation of a binary Tree
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with utils; use utils;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;


package body Tree_Bin is
------
    function init_tree(el: T_Element) return T_Tree is
        tree: T_Tree;
    begin
        -- initialize the tree
        tree := new T_Node;
        -- insert the root element
        tree.All.el := el;
        -- return it
        return tree;
    end init_tree;

------
    function empty_tree(tree: in T_Tree) return Boolean is
    begin
        return tree = null;
    end empty_tree;

------
    function tree_depth(tree: in T_Tree) return Integer is
    begin
        -- stop criteria
        if (tree = null) then
            return 0;
        end if;

        return 1 + max(tree_depth(tree.All.l), tree_depth(tree.All.r));
    end tree_depth;

------
    function tree_length(tree: in T_Tree) return Integer is
    begin
        -- stop criteria
        if (tree = null) then
            return 0;
        end if;

        return 1 + tree_length(tree.All.l) + tree_length(tree.All.r);
    end tree_length;

------
    function exist_el_tree(id: in Integer; tree: in T_Tree) return Boolean is
    begin
        return find_el_tree(id, tree) /= null;
    end;

------
    function find_el_tree(id: in Integer; tree: in T_Tree) return T_Tree is
        t: T_Tree;
    begin
        -- Stop criteria
        if (tree = null) then
            return null;
        end if;

        -- This is the element we want
        if (id_el(tree.All.el) = id) then
            return tree;
        end if;

        -- Search the element on the left branch
        t := find_el_tree(id, tree.All.l);

        if (t /= null) then
            return t;
        else 
            -- Search the element on the right branch
            return find_el_tree(id, tree.All.r);
        end if;

    end find_el_tree;

------
    procedure add_el_tree(el: in T_Element; tree: in out T_Tree) is
        t: T_Tree;
    begin
        t := new T_Node;
        t.All.el := el;
        t.All.up := tree;

        -- Add the node to the left side
        if (tree.All.l = null) then
            tree.All.l := t;
        -- Add the node to the right side
        elsif (tree.All.r = null) then
            tree.All.r := t;
        -- No places left, raise exception
        else
            raise FULL_NODE;
        end if;

    exception
        when FULL_NODE => Put_Line("Abort (no places left)");
    end add_el_tree;

------
    procedure show_tree(tree: in T_Tree; cur_gen: in Integer) is
        depth: Integer;
    begin
        -- Stop criteria
        if (tree = null) then
            return;
        end if;

        -- Show the first row
        if (cur_gen = 0) then
            -- calculate the depth of the tree
            depth := tree_depth(tree);
            for i in 1..depth loop
                Put(Integer'Image(i-1) & "   ");
            end loop;

            Put_Line("generation");
            Put_Line("-------------------------------");
        end if;

        -- Indent the content
        for i in 0..cur_gen-1 loop
            Put("      ");  
        end loop;

        show(tree.All.el, cur_gen /= 0);

        show_tree(tree.All.l, cur_gen + 1);
        show_tree(tree.All.r, cur_gen + 1);
    end show_tree;

------
    procedure show_tree(tree: in T_Tree) is
    begin
        show_tree(tree, 0);
    end show_tree;

------
    procedure show_ancestors(tree: in T_Tree; generation: in Integer; cur_gen: in Integer := 0) is
    begin
        -- Stop criteria
        if (tree = null) then
            return;
        end if;

        if (cur_gen = generation or generation = -1) then
            show(tree.All.el, false);
        end if;
        
        show_ancestors(tree.All.l, generation, cur_gen + 1);
        show_ancestors(tree.All.r, generation, cur_gen + 1);

    end show_ancestors;

------
    procedure show_descendant(tree: in T_Tree; descendant: in Integer; cur_desc: in Integer := 0) is
    begin
        -- Stop criteria
        if (tree = null) then
            return;
        end if;

        if (cur_desc = descendant or descendant = -1) then
            show(tree.All.el, false);
        end if;
        
        show_descendant(tree.All.up, descendant, cur_desc + 1);
    end show_descendant;

------
    procedure filter_tree(tree: in T_Tree; nb_parent: in Integer) is
        nb: Integer := 0;
    begin
        -- stop criteria
        if (tree = null) then
            return;
        end if;

        -- calculate the number of parent
        if (tree.All.l /= null) then
            nb := nb + 1;
        end if;

        if (tree.All.r /= null) then
            nb := nb + 1;
        end if;

        -- if valid, show it
        if (nb = nb_parent) then
            show(tree.All.el, false);
        end if;

        -- recursive call
        filter_tree(tree.All.l, nb_parent);
        filter_tree(tree.All.r, nb_parent);
    end filter_tree;

------
    procedure delete_tree(id: in Integer; tree: in out T_Tree) is
    begin
        -- Stop criteria
        if (tree = null) then
            return;
        end if;

        -- If it's the root element
        if (id_el(tree.All.el) = id) then
            tree := null;
            return;
        end if;

        delete_tree(id, tree.All.l);
        delete_tree(id, tree.All.r);
    end delete_tree;

------
    procedure clear_tree(tree: in out T_Tree) is
    begin
        tree := null;
    end;

------
    function root_tree(tree: in T_Tree) return T_Element is
    begin
        return tree.All.el;
    end root_tree;

------
    function  stringify_tree(tree: in T_Tree; ancestor_id: in Integer := -1) return Unbounded_String is
    begin
        if (tree = null) then
            return To_Unbounded_String("");
        end if;


        return To_Unbounded_String(
            To_String(stringify_el(tree.All.el, ancestor_id)) & 
            LF &
            To_String(stringify_tree(tree.All.l, id_el(tree.All.el))) & 
            To_String(stringify_tree(tree.All.r, id_el(tree.All.el)))
            );
    end stringify_tree;

------
    function equal_tree(g,d: in T_Tree) return Boolean is
        rootLeft: T_Element;
        rootRight: T_Element;
    begin
        if (empty_tree(g) /= empty_tree(d)) then
            return false;
        end if;

        if (empty_tree(g)) then
            return true;
        end if;

        rootLeft := root_tree(g);
        rootRight := root_tree(d);

        return id_el(rootLeft) = id_el(rootRight); 
    end equal_tree;
end Tree_Bin;