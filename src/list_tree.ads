with p_list_gen;
with Tree_Genealog; use Tree_Genealog;


package List_Tree is new p_list_gen(T_Element=> T_Tree, equal=>equal_tree, afficher_el=> show_tree);