--
-- Generic module specification implementing a binary Tree
--

generic
    type T_Element is private;
    with procedure show(el: in T_Element; parent: in Boolean);
    with function id_el(el: in T_Element) return Integer; -- return the id of an element
package Tree_Bin is
    type T_Tree is private;
    ABSENT_KEY: Exception; -- Access a none existing node
    FULL_NODE: Exception; -- A node already has 2 connections
    
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
    -- function empty_tree 
    -- sémantique: is the tree empty
    -- parameters: tree: The tree to check
    -- return type: Boolean
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function empty_tree(tree: in T_Tree) return Boolean;

    --------------------------------------------------------
    -- function tree_depth (recursive)
    -- sémantique: give the depth of a tree
    -- parameters: tree: The tree to check
    -- return type: Integer
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function tree_depth(tree: in T_Tree) return Integer;

    --------------------------------------------------------
    -- function tree_length (recursive)
    -- sémantique: give the length of a tree
    -- parameters: tree: The tree to check
    -- return type: Integer
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function tree_length(tree: in T_Tree) return Integer;

    --------------------------------------------------------
    -- function exist_el_tree 
    -- sémantique: indicate wether an element exist within a tree
    -- parameters: tree: The tree to check
    --             id: the searching id
    -- return type: Boolean
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function exist_el_tree(id: in Integer; tree: in T_Tree) return Boolean;

    --------------------------------------------------------
    -- function find_el_tree (recursive)
    -- sémantique: return the tree with the node being the element of a certain id
    -- parameters: tree: The tree to check
    --              id: The id of the willing element
    -- return type: T_Tree (potentially null)
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    function find_el_tree(id: in Integer; tree: in T_Tree) return T_Tree;

    --------------------------------------------------------
    -- procedure add_el-tree
    -- sémantique: add an element to a tree
    -- parameters: el: The element to add
    --             tree: The tree receiving the element
    -- return type: X
    -- pre-condition: not an empty tree
    -- post-condition: none
    -- exception: FULL_NODE
    procedure add_el_tree(el: in T_Element; tree: in out T_Tree) with Pre=> not empty_tree(tree);

    --------------------------------------------------------
    -- procedure show_tree (recursive)
    -- sémantique: show all the element of a tree
    -- parameters: tree: The tree to display
    --              generation: always set to 0 at first use (used to indent content properly)
    -- return type: X
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    procedure show_tree(tree: in T_Tree; generation: in Integer);

    --------------------------------------------------------
    -- procedure show_ancestors (recursive)
    -- sémantique: show all the ancestors at a given level
    -- parameters: tree: The tree to display
    --              cur_gen: Used to keep track of the current generation
    --              generation: to gen level to show
    -- return type: X
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    procedure show_ancestors(tree: in T_Tree; cur_gen: in Integer; generation: in Integer);

    --------------------------------------------------------
    -- procedure show_descendant
    -- sémantique: show all the ancestors at a given level
    -- parameters: tree: The tree to display
    --              cur_desc: Used to keep track of the current generation
    --              descendant: to gen level to show
    -- return type: X
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    procedure show_descendant(tree: in T_Tree; cur_desc: in Integer; descendant: in Integer);

    --------------------------------------------------------
    -- procedure filter_tree (recursive)
    -- sémantique: show all the element of a tree with the right number of parent
    -- parameters: tree: The tree containing all the elements
    --              nb_parents: The filter crieria
    -- return type: X
    -- pre-condition: a valid number of parent
    -- post-condition: none
    -- exception: none
    procedure filter_tree(tree: in T_Tree; nb_parent: in Integer) with Pre=> nb_parent >= 0 and nb_parent <=2;

    --------------------------------------------------------
    -- procedure delete_tree
    -- sémantique: delete the tree element
    -- parameters: tree: The root of the tree
    --              id : the element id to delete
    -- return type: X
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    procedure delete_tree(id: in Integer; tree: in out T_Tree);

    --------------------------------------------------------
    -- procedure clear_tree
    -- sémantique: clear the all tree
    -- parameters: tree: The root of the tree
    -- return type: none
    -- pre-condition: none
    -- post-condition: none
    -- exception: none
    procedure clear_tree(tree: in out T_Tree);
                                                                
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