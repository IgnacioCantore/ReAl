module PrettyPrinter
  ( printTable
  , printOperator
  , printTableFile
  , printOperatorFile
  )
where

import           Common
import           Data.List
import qualified Data.Set as S

printTable :: Table -> String
printTable (T as _ rs) =
  let header = map printValue as
      rows = S.toList $ S.map (map printValue) rs
      widths = [let l = maximum $ map length col in if l < 3 then 3 else l | col <- transpose $ header : rows]
      separator = intercalate "═╬═" [replicate width '═' | width <- widths]
      fillCols cols = intercalate " ║ " [center width col | (c, width, col) <- zip3 as widths cols]
  in ("\n" ++ (unlines $ fillCols header : separator : map fillCols rows))
 where
   -- Para llenar con espacios
   center :: Int -> String -> String
   center n s = replicate l ' ' ++ s ++ replicate r ' '
     where x = n - length s
           l = x `div` 2
           r = x - l

printOperator  :: Name -> Args -> Relation -> String
printOperator n args op = let args' = intercalate "," args
                          in (n ++ "(" ++ args' ++ ") = " ++ (printRelation op))

printRelation :: Relation -> String
printRelation (Table n)     = n
printRelation (Sel c r)     = let c' = printCond c
                                  r' = printRelation r
                              in "sel(" ++ c' ++ ")(" ++ r' ++ ")"
printRelation (Proj as r)   = let as' = intercalate "," (map printValue as)
                                  r'  = printRelation r
                              in "proj(" ++ as' ++ ")(" ++ r' ++ ")"
printRelation (Rename n r)  = let r' = printRelation r
                              in "rename(" ++ n ++ ")()" ++ r' ++ ")"
printRelation (Union r1 r2) = let r1' = printRelation r1
                                  r2' = printRelation r2
                              in "union(" ++ r1' ++ "," ++ r2' ++ ")"
printRelation (Diff r1 r2)  = let r1' = printRelation r1
                                  r2' = printRelation r2
                              in "diff(" ++ r1' ++ "," ++ r2' ++ ")"
printRelation (Prod r1 r2)  = let r1' = printRelation r1
                                  r2' = printRelation r2
                              in "prod(" ++ r1' ++ "," ++ r2' ++ ")"

printCond :: Cond -> String
printCond (Eq v1 v2)  = (printValue v1) ++ " == " ++ (printValue v2)
printCond (NEq v1 v2) = (printValue v1) ++ " /= " ++ (printValue v2)
printCond (Lt v1 v2)  = (printValue v1) ++ " < " ++ (printValue v2)
printCond (Gt v1 v2)  = (printValue v1) ++ " > " ++ (printValue v2)
printCond (LtE v1 v2) = (printValue v1) ++ " <= " ++ (printValue v2)
printCond (GtE v1 v2) = (printValue v1) ++ " >= " ++ (printValue v2)
printCond (And c1 c2) = "(" ++ (printCond c1) ++ ") && (" ++ (printCond c2) ++ ")"
printCond (Or c1 c2)  = "(" ++ (printCond c1) ++ ") || (" ++ (printCond c2) ++ ")"
printCond (Not c)     = "! (" ++ (printCond c) ++ ")"

printValue :: Value -> String
printValue (StringVal s) = "\"" ++ s ++ "\""
printValue (IntVal n)    = show n
printValue (DoubleVal d) = show d
printValue (AttrVal r a) = if null r then a else r ++ "." ++ a

printTableFile :: Name -> Table -> String
printTableFile n (T as _ rs) =
  let as' = intercalate "," $ map (\(AttrVal r a) -> r ++ a) as
      rs' = intercalate "\n" $ S.toList (S.map (\xs -> intercalate "," $ map printValue xs) rs)
  in ("\n" ++ n ++ " = {\n" ++ as' ++ "\n" ++ rs' ++ "\n}\n")

printOperatorFile :: Name -> Args -> Relation -> String
printOperatorFile n args op = let op' = printOperator n args op
                              in ("\ndef " ++ op' ++ "\n")
