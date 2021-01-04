--
-- Generic module specification implementing a binary Tree
--

generic
    type T_Element is private;
    with procedure show(el: T_Element);
package Tree_Bin is
    type T_Tree is private;
    ABSENT_KEY: Exception;

    --------------------------------------------------------
    -- fonction init_tree
    -- sémantique: return an empty tree
    -- parameters: el: The root of the tree
    -- return type: T_Tree
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function init_tree(el: T_Element) return T_Tree;

    --------------------------------------------------------
    -- procedure add_el
    -- sémantique: add an element to a tree
    -- parameters: el: The element to add
    --             tree: The tree receiving the element
    --             left: left child or right child
    -- return type: X
    -- pre-condition: 
    -- post-condition: none
    -- exception: none
    procedure add_el(el: in T_Element, tree: in out T_Tree, left: in Boolean);

private
    type T_Node;

    type T_Tree is access T_Node;

    type T_Node is record
        el: T_Element; -- The content of a node
        l: T_Tree; -- The left descendant
        r: T_Tree; -- the right descendant
        up: T_Tree; -- the upper element
    end record;

end Tree_Bin;