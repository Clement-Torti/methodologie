with p_list_gen;
with Tree_Gen; use Tree_Gen;


package List_Tree_Gen is new p_list_gen(T_Element=> T_Tree, equal=>equal_tree, afficher_el=> show_tree);