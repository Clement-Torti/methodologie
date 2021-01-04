
with Ada.Numerics.Discrete_Random;

package body Alea is
  -- Cette fonction fournit un nombre aléatoire entre 1 et 100.
  -- Pas de parametre.
  -- Resultat : Entier.
  -- Pre : Rien
  -- Post :  1 <= resultat <=1000
 
   function Alea_1_1000 return Integer is
      type Intervalle is range 1 .. 1000; -- un entier entre 1 et 100.
      package Gene is new Ada.Numerics.Discrete_Random (Intervalle);
      use Gene;
      n : Intervalle;
      G : Generator;
   begin
      Reset (G);          -- Initialise le générateur (fait une seule fois)
      n := Random (G);    -- Tire un nombre au hasard entre Min et Max
      return Integer'Value(Intervalle'Image(N)) ;
   end Alea_1_1000;
end Alea ;
