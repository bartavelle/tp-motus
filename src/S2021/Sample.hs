module S2021.Sample where

import Data.Set (Set)
import qualified Data.Set as S
import Motus (Solution (Solution), Status (..))

-- | Ne pas oublier de saisir ce champ!
student :: String
student = "Nom Prénom"

-- Partie 1, 5 points
--
-- La fonction motus fait ce qu'il se passe dans le jeu Motus (ou Wordle pour les plus jeunes)
--
-- Elle indique, pour chaque lettre du mot soumis par l'utilisateur son status:
-- * NotUsed -> la lettre n'est pas utilisée dans le mot secret
-- * Misplaced -> la lettre est utilisée dans le mot secret, mais n'est pas au bon endroit
-- * Correct -> la lettre est bien à cet endroit dans le mot secret

-- tous les mots sont composés des 26 lettres non accentuées minuscules
-- dans les tests, tous les mots font 6 caractères, mais ils pourront faire jusqu'à 20
-- caractères pour l'évaluation

check ::
  -- | secret word
  String ->
  -- | user word
  String ->
  [Status]
check = undefined

-- Partie 2, 15 points
--
-- vous devez maintenant écrire une fonction capable de résoudre le jeu!
-- barême:
--  * 10 points si la fonction trouve toujours la réponse en moins de 18 coups
--  * 5 points en fonction de la performance
--
-- ATTENTION : les tests qui conditionnent la note seront fait avec un dictionnaire
-- plus gros que celui qui vous est proposé ici, en particulier avec des mots plus longs!
--
-- Pour utiliser le "Set", reportez vous à la documentation suivante:
-- https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Set.html
solve ::
  -- | dictionnary of possible secret words
  Set String ->
  -- | previous attempts
  [(String, [Status])] ->
  -- | current attempt
  String
solve = undefined

-- ********* Ne pas modifier après cette marque! ***********

sol :: Solution
sol = Solution student check solve
