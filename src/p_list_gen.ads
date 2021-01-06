--
-- Specification of the chained list (from TP13)
--

GENERIC
    type T_Element is private; 
    with function equal(g,d: in T_Element) return Boolean;
    with procedure afficher_el(e: in T_Element);
PACKAGE p_list_gen IS
    ELEMENT_ABSENT: Exception;
    LISTE_VIDE: Exception;

    type Ptr_Cellule is private;

    --------------------------------------------------------
    -- fonction est_vide
    -- sémantique: teste si une liste l est vide
    -- paramètres: l donnée type liste
    -- type retour: booléen
    -- pré-condition: aucune
    -- post-condition: aucune
    -- exception: aucune
    function est_vide(l: in Ptr_Cellule) return Boolean;

    -----------------------------------------------------------
    -- fonction rechercher
    -- sémantique: recherche si e est présent dans la liste l,
    -- retourne son adresse ou null si la liste est vide ou
    -- si e n’appartient pas à la liste
    -- paramètres: l donnée type liste
    -- e donnée type entier
    -- type-retour: liste
    -- pré-condition: aucune
    -- post-condition: aucune
    function rechercher(e: in T_Element; l: in Ptr_Cellule) return Ptr_Cellule;

    -----------------------------------------------------------
    -- fonction exist
    -- sémantique: recherche si e est présent dans la liste l,
    -- retourne vrai ou faux
    -- si e n’appartient pas à la liste
    -- paramètres: l donnée type liste
    -- e donnée type entier
    -- type-retour: liste
    -- pré-condition: aucune
    -- post-condition: aucune
    function exist(e: in T_Element; l: in Ptr_Cellule) return Boolean;

    --------------------------------------------------------
    -- fonction créer_liste_vide
    -- sémantique: créer une liste vide
    -- paramètres: aucun
    -- type-retour: liste
    -- pré-condition: aucune
    -- post-condition: est_vide (l) vaut vrai
    -- exception: aucune
    function creer_liste_vide return Ptr_Cellule with POST=> est_vide(creer_liste_vide'RESULT);

    --------------------------------------------------------
    -- procedure inserer_en_tete
    -- sémantique: insère l’élément nouveau en tete de la liste l (l vide ou non vide)
    -- paramètres: l donnée/résultat type liste
    -- x: element à ajouter
    -- pré-condition: aucune
    -- post-condition: nouveau appartient à la liste
    -- exception: aucune
    procedure inserer_en_tete(x: in T_Element; l: in out Ptr_Cellule) with POST=> exist(x, l);

    -----------------------------------------------------
    -- procedure afficher_liste
    -- sémantique: afficher les éléments de la liste l
    -- paramètres: l donnée type liste
    -- pré-condition: aucune
    -- post-condition: aucune
    -- exception: aucune
    procedure afficher_liste(l: in Ptr_Cellule);


    -------------------------------------------------------------------------
    -- procedure enlever
    -- sémantique: enlever un élément e de la liste l (liste vide ou non vide)
    -- paramètres: l liste à parcourir
    -- e: element à supprimer
    -- pré-condition: aucune
    -- post-condition: e n’appartient pas à la liste
    -- exception: aucune
    procedure enlever(e: in T_Element; l: in out Ptr_Cellule) with Post => not exist(e, l);

    -------------------------------------------------------------------------
    -- procedure element_liste
    -- sémantique: retourne l'élement à index de la liste (potentiellement null)
    -- paramètres: l liste à parcourir
    -- index: index de l'élement à retourner
    -- pré-condition: aucune
    -- post-condition: aucune
    -- exception: aucune
    function element_liste(index: in Integer; l: in Ptr_Cellule) return Ptr_Cellule;

    -------------------------------------------------------------------------
    -- procedure root_liste
    -- sémantique: retourne le premier element de la liste
    -- paramètres: l liste
    -- pré-condition: liste non vide
    -- post-condition: aucune
    -- exception: aucune
    function root_liste(l: in Ptr_Cellule) return T_Element with Pre=> not est_vide(l);

    -------------------------------------------------------------------------
    -- procedure vider_liste
    -- sémantique: vide la liste
    -- paramètres: l liste à vider
    -- pré-condition: aucune
    -- post-condition: aucune
    -- exception: aucune
    procedure vider_liste(l: in out Ptr_Cellule);

PRIVATE 
    type T_Cellule;
    type Ptr_Cellule is access T_Cellule;
    type T_Cellule is record
       el: T_Element;
       suiv: Ptr_Cellule;
    end record;
END p_list_gen;