--
-- Specification of the chained list (from TP13)
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

PACKAGE BODY p_list_gen IS
    function est_vide(l: in Ptr_Cellule) return Boolean is
    begin
        return l = null;
    end est_vide;

------
    function rechercher(e: in T_Element; l: in Ptr_Cellule) return Ptr_Cellule is
        current: Ptr_Cellule := l;
    begin
        if (current = null) then
            return current;
        elsif (equal(current.All.el,e)) then
            return current;
        else
            return rechercher(e, current.All.suiv); 
        end if;
    end;

------
    function exist(e: in T_Element; l: in Ptr_Cellule) return Boolean is 
    begin
        return rechercher(e, l) /= null;
    end;

------
    function creer_liste_vide return Ptr_Cellule is
        l: Ptr_Cellule;
    begin
        return l;
    end creer_liste_vide;

------
    procedure inserer_en_tete(x: in T_Element; l: in out Ptr_Cellule) is
        cell: Ptr_Cellule;
    begin
        cell := new T_Cellule;
        cell.All.el := x;
        cell.All.suiv := l;
        l := cell;
    end;

------
    procedure afficher_liste(l: in Ptr_Cellule) is
    begin
        if (l = null) then
            New_Line;
            return;
        end if;

        afficher_el(l.All.el);
        New_Line;
        
        afficher_liste(l.All.suiv);
    end afficher_liste;

------
    procedure enlever(e: in T_Element; l: in out Ptr_Cellule) is
        current: Ptr_Cellule := l;
    begin
        -- 1er element à supprimer
        if (equal(current.All.el, e)) then
            l := l.All.suiv;
            return;
        end if;

        while (current.All.suiv /= null) loop
            if (equal(current.All.suiv.All.el, e)) then
                current.All.suiv := current.All.suiv.All.suiv;
                return; 
            end if;
        end loop;
    end;

------
    function element_liste(index: in Integer; l: in Ptr_Cellule) return Ptr_Cellule is
        cur_index: Integer := 0;
        l2: Ptr_Cellule;
    begin
        if (l = null) then
            return null;
        end if;

        l2 := l;
        for i in 0..index-1 loop
            if (l2.All.suiv = null) then
                return null;
            end if;

            l2 := l2.All.suiv;
        end loop;

        return l2;
    end element_liste;

------
    function root_liste(l: in Ptr_Cellule) return T_Element is
    begin
        return l.All.el;
    end root_liste;

------
    procedure vider_liste(l: in out Ptr_Cellule) is
    begin
        l := null;
    end vider_liste;
END p_list_gen;

