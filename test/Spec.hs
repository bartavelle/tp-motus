{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Bifunctor (bimap)
import Data.Either (isRight, partitionEithers)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import Motus
import qualified S2021.Sample
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)

main :: IO ()
main = do
  possible <- S.fromList . lines <$> readFile "test/wordle-allowed-guesses.txt"
  hspec $ do
    checkSolution possible S2021.Sample.sol

checkSolution :: S.Set String -> Solution -> Spec
checkSolution possible sol = do
  describe "check" $ do
    let alltests =
          [ ("tegus", "rejig", [NotUsed, Correct, NotUsed, NotUsed, Misplaced]),
            ("nadas", "snaws", [Misplaced, Misplaced, Misplaced, NotUsed, Correct]),
            ("nabob", "doven", [NotUsed, Misplaced, NotUsed, NotUsed, Misplaced]),
            ("avine", "fagot", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("eughs", "jokey", [NotUsed, NotUsed, NotUsed, Misplaced, NotUsed]),
            ("turns", "viler", [NotUsed, NotUsed, NotUsed, NotUsed, Misplaced]),
            ("globy", "pluff", [NotUsed, Correct, NotUsed, NotUsed, NotUsed]),
            ("guqin", "quats", [Misplaced, Correct, NotUsed, NotUsed, NotUsed]),
            ("koaps", "sighs", [Misplaced, NotUsed, NotUsed, NotUsed, Correct]),
            ("azoic", "baurs", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("foxie", "sloot", [NotUsed, NotUsed, Misplaced, Misplaced, NotUsed]),
            ("bacca", "slurb", [NotUsed, NotUsed, NotUsed, NotUsed, Misplaced]),
            ("hayer", "ouped", [NotUsed, NotUsed, NotUsed, Correct, NotUsed]),
            ("topee", "gleis", [NotUsed, NotUsed, Misplaced, NotUsed, NotUsed]),
            ("tofus", "porny", [NotUsed, Correct, NotUsed, NotUsed, NotUsed]),
            ("toils", "roguy", [NotUsed, Correct, NotUsed, NotUsed, NotUsed]),
            ("kibei", "gynny", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("unbed", "bield", [Misplaced, NotUsed, Misplaced, NotUsed, Correct]),
            ("reams", "whamo", [NotUsed, NotUsed, Correct, Correct, NotUsed]),
            ("refix", "zests", [NotUsed, Correct, NotUsed, NotUsed, NotUsed]),
            ("nosed", "thrid", [NotUsed, NotUsed, NotUsed, NotUsed, Correct]),
            ("coons", "icier", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("lurer", "withy", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("femmy", "rajas", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("hoper", "bhais", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("sired", "boxen", [NotUsed, NotUsed, NotUsed, Correct, NotUsed]),
            ("soger", "sings", [Correct, NotUsed, NotUsed, Misplaced, Misplaced]),
            ("rores", "klett", [NotUsed, NotUsed, Misplaced, NotUsed, NotUsed]),
            ("pipit", "warre", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("hades", "molys", [NotUsed, NotUsed, NotUsed, NotUsed, Correct]),
            ("kited", "peppy", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("mater", "bothy", [NotUsed, NotUsed, Correct, NotUsed, NotUsed]),
            ("dolls", "speir", [Misplaced, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("leear", "soldo", [NotUsed, NotUsed, Misplaced, NotUsed, NotUsed]),
            ("ouped", "monte", [NotUsed, Misplaced, NotUsed, NotUsed, Misplaced]),
            ("tolus", "sprit", [Misplaced, NotUsed, NotUsed, NotUsed, Misplaced]),
            ("villi", "moods", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("crapy", "spica", [NotUsed, Misplaced, NotUsed, Misplaced, Misplaced]),
            ("skeos", "skids", [Correct, Correct, NotUsed, NotUsed, Correct]),
            ("pored", "penie", [Correct, Misplaced, NotUsed, NotUsed, Misplaced]),
            ("murra", "slots", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("autos", "roups", [NotUsed, Misplaced, Misplaced, NotUsed, Correct]),
            ("tronk", "annas", [NotUsed, Misplaced, Misplaced, NotUsed, NotUsed]),
            ("punji", "treif", [NotUsed, NotUsed, NotUsed, Misplaced, NotUsed]),
            ("durum", "dirks", [Correct, NotUsed, Correct, NotUsed, NotUsed]),
            ("uvula", "forts", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("copal", "gated", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("talea", "codon", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("pooja", "terfs", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("bajra", "kojis", [NotUsed, NotUsed, Correct, NotUsed, NotUsed]),
            ("troak", "wauff", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("smoko", "nubby", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("laved", "sites", [NotUsed, NotUsed, NotUsed, Correct, NotUsed]),
            ("vimen", "whaps", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("haiks", "finds", [NotUsed, Misplaced, NotUsed, NotUsed, Correct]),
            ("slier", "vulgo", [NotUsed, NotUsed, Misplaced, NotUsed, NotUsed]),
            ("crues", "naris", [NotUsed, NotUsed, Misplaced, NotUsed, Correct]),
            ("brool", "domed", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("gobos", "kerma", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("frass", "tetra", [NotUsed, NotUsed, NotUsed, Misplaced, Misplaced]),
            ("lurgy", "looms", [Correct, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("kandy", "azoic", [Misplaced, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("ohing", "calve", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("gists", "gript", [Correct, NotUsed, Misplaced, NotUsed, Misplaced]),
            ("adder", "lawer", [NotUsed, Misplaced, NotUsed, Correct, Correct]),
            ("barny", "hours", [NotUsed, NotUsed, NotUsed, Misplaced, NotUsed]),
            ("hayed", "panni", [NotUsed, Correct, NotUsed, NotUsed, NotUsed]),
            ("newel", "cycas", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("skeen", "fixed", [NotUsed, NotUsed, NotUsed, Correct, NotUsed]),
            ("drome", "commy", [NotUsed, Misplaced, Misplaced, Correct, NotUsed]),
            ("courb", "poulp", [NotUsed, Correct, Correct, NotUsed, NotUsed]),
            ("paced", "koaps", [NotUsed, NotUsed, Misplaced, Misplaced, NotUsed]),
            ("bobas", "rifty", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("retax", "druxy", [NotUsed, Misplaced, NotUsed, Misplaced, NotUsed]),
            ("palla", "bimah", [NotUsed, NotUsed, NotUsed, Misplaced, NotUsed]),
            ("puces", "ronte", [NotUsed, NotUsed, NotUsed, NotUsed, Misplaced]),
            ("voxel", "sabot", [NotUsed, NotUsed, NotUsed, Misplaced, NotUsed]),
            ("bulgy", "kasme", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("plims", "fugly", [NotUsed, NotUsed, NotUsed, Misplaced, NotUsed]),
            ("stent", "toffy", [Misplaced, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("spina", "debel", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("files", "loggy", [Misplaced, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("eales", "flexi", [NotUsed, Misplaced, Misplaced, NotUsed, NotUsed]),
            ("huggy", "seine", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("aband", "rifer", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("boric", "bises", [Correct, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("ydrad", "coqui", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("vairs", "oxlip", [NotUsed, NotUsed, NotUsed, Misplaced, NotUsed]),
            ("ylkes", "emmew", [Misplaced, NotUsed, NotUsed, Correct, NotUsed]),
            ("pluff", "vinas", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("riffs", "damar", [NotUsed, NotUsed, NotUsed, NotUsed, Misplaced]),
            ("fains", "abets", [Misplaced, NotUsed, NotUsed, NotUsed, Correct]),
            ("oints", "intil", [Misplaced, Misplaced, Misplaced, Misplaced, NotUsed]),
            ("poule", "wefts", [NotUsed, Misplaced, NotUsed, NotUsed, NotUsed]),
            ("gleys", "kaing", [NotUsed, NotUsed, NotUsed, NotUsed, Misplaced]),
            ("urped", "plugs", [Misplaced, NotUsed, Misplaced, NotUsed, NotUsed]),
            ("minny", "pacha", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("mobey", "laxly", [NotUsed, NotUsed, NotUsed, NotUsed, Correct]),
            ("glogg", "imari", [NotUsed, NotUsed, NotUsed, NotUsed, NotUsed]),
            ("pends", "ewest", [Misplaced, NotUsed, Misplaced, Misplaced, NotUsed]),
            ("pends", "pends", [Correct, Correct, Correct, Correct, Correct])
          ]
    forM_ alltests $ \(secret, user, expected) -> it (secret ++ "/" ++ user) $ _check sol secret user `shouldBe` expected
  describe "solve" $ do
    let soltest mx secret = go (0 :: Int) []
          where
            go tries prevattempts
              | tries > mx = Left "too many attempts"
              | otherwise =
                let attempt = _solve sol possible prevattempts
                 in if attempt == secret
                      then Right tries
                      else go (tries + 1) (prevattempts ++ [(attempt, _check sol secret attempt)])
    it "fains, in 10" $ soltest 10 "fains" `shouldSatisfy` isRight
    it "zarfs, in 10" $ soltest 10 "zarfs" `shouldSatisfy` isRight
    let results = [bimap (w,) (w,) (soltest 18 w) | w <- S.toList possible]
        (failed, succeeded) = partitionEithers results
        (worstword, worstattempt) = maximumBy (comparing snd) succeeded
    it "did not fail" $ failed `shouldBe` []
    describe ("Worst word (" ++ worstword ++ ", " ++ show worstattempt ++ " attempts ) success in") $
      forM_ [18, 17 .. 4] $ \t ->
        it ("< " ++ show t) $ worstattempt `shouldSatisfy` (< t)
