module MyLib where

import Control.Exception  -- for SomeException

import Language.C

import Data.Loc (Pos, Loc(Loc, NoLoc), SrcLoc,
                 startPos, noLoc, locOf, locStart, locEnd, posCol, posLine)
import Data.Generics
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Data.String
import Data.Either
import Control.Monad.State
import qualified Data.ByteString as B (ByteString,length,pack)
import Data.ByteString.UTF8 as U8 (fromString, toString)
import Data.Char(ord, isSpace)
import Data.List
import Text.Printf (printf)
-- import Data.List(dropWhileEnd)
import Text.Show.Unicode
import GHC.Read (list)
import Text.Regex.Posix
import Debug.Trace

restoreUTF8 :: String -> String
restoreUTF8 = U8.toString . B.pack . map (fromInteger . fromIntegral . ord)

utf8Const :: Const -> Const
utf8Const (StringConst ss str loc) = StringConst (map restoreUTF8 ss) (restoreUTF8 str) loc
utf8Const c = c

utf8Stm :: Stm -> Stm
utf8Stm (Comment str stm loc) = Comment (restoreUTF8 str) stm loc
utf8Stm s = s

utf8Definition :: Definition -> Definition
utf8Definition = everywhere (mkT utf8Stm) . everywhere (mkT utf8Const)

someFunc :: IO ()
someFunc = putStrLn ("Hello")

start (Loc s _) = s
end (Loc _ e)   = e 

parseProg :: String -> Either SomeException [Definition]
parseProg str = parse [C11] [] parseUnit (U8.fromString str) Nothing

eraseComments :: String -> String -> String
eraseComments [] rs  = reverse rs
eraseComments [a] rs = reverse (a : rs)
eraseComments ('/' : '*' : cs) rs = eraseCommentsIn cs (' ' : ' ' : rs)
  where eraseCommentsIn [] _  = error "EOF encountered in comment"
        eraseCommentsIn [a] _ = error "EOF encountered in comment"
        eraseCommentsIn ('*' : '/' : cs) rs = eraseComments cs (' ' : ' ' : rs)
        eraseCommentsIn ('\n' : cs) rs      = eraseCommentsIn cs ('\n' : rs)
        eraseCommentsIn (c : cs) rs | ord c < 128 = eraseCommentsIn cs (' ': rs)
                                    | otherwise
           = let n = B.length (U8.fromString [c])
               in eraseCommentsIn cs (replicate n ' ' ++ rs)
eraseComments ('"' : cs) rs = eraseCommentsInStr cs ('"' : rs)
  where eraseCommentsInStr [] _        = error "EOF encountered in string literal"
        eraseCommentsInStr ('"' : cs) rs = eraseComments cs ('"' : rs)
        eraseCommentsInStr ('\\' : c : cs) rs
          = eraseCommentsInStr cs (c : '\\' : rs)
        eraseCommentsInStr (c : cs) rs = eraseCommentsInStr cs (c : rs)
eraseComments (c : cs) rs    = eraseComments cs (c : rs)

untabify :: String -> String
untabify line = untabifyAux line "" 0
  where
    untabifyAux [] r _    = reverse r
    untabifyAux ('\t' : cs) r idx
      = let n = 8 - idx `mod` 8
          in untabifyAux cs (replicate n ' ' ++ r) (idx + n)
    untabifyAux (c : cs) r idx = untabifyAux cs (c : r) (idx + 1)

printstrsearch :: Int -> String -> Int
printstrsearch num (x1:x2:xs)
  | (((x1 == '%') && (x2 == 'd')) == True) = printstrsearch (num+1) xs
  | (((x1 == '%') && (x2 == 'f')) == True) = printstrsearch (num+1) xs
  | otherwise = printstrsearch num (x2:xs)
printstrsearch num _ = num

-- let linesOfCode = Data.List.lines code
--         pattern :: String
--         pattern = "^[[:space:]]*\\w+[[:space:]]+\\w+\\([^)]*\\)[[:space:]]*\\{"
--         matches = Data.List.any (\(line, _) -> line =~ pattern) (Data.List.zip linesOfCode [1..])
--       in matches

countpFormatStrings :: String -> Int
countpFormatStrings inputString = length matches
  where
    regexPattern :: String
    regexPattern = "%[0-9]*\\.?[0-9]*[df]"
    matches = inputString =~ regexPattern :: [[String]]

countsFormatStrings :: String -> Int
countsFormatStrings inputString = length matches
  where
    regexPattern :: String
    regexPattern = "%[0-9]*\\.?[0-9]*([d]|lf)"
    matches = inputString =~ regexPattern :: [[String]]

pcountPercent :: String -> Int
pcountPercent [] = 0
pcountPercent ('%':'d':xs) = 1 + pcountPercent xs
pcountPercent ('%':'f':xs) = 1 + pcountPercent xs
pcountPercent (_:xs) = pcountPercent xs

scountPercent :: String -> Int
scountPercent [] = 0
scountPercent ('%':'d':xs) = 1 + scountPercent xs
scountPercent ('%':'l':'f':xs) = 1 + scountPercent xs
scountPercent (_:xs) = scountPercent xs

scanstrsearch :: Int -> String -> Int
scanstrsearch num (x1:x2:x3:xs)
  | (((x1 == '%') && (x2 == 'd')) == True) = scanstrsearch (num+1) (x3:xs)
  -- | (((x1 == '%') && (x2 == 'l') && (x3 == 'f')) == True) = scanstrsearch (num+1) xs
  | otherwise = scanstrsearch num (x2:x3:xs)
scanstrsearch num _ = num

abStm2 :: Stm -> ([Int], [Int])
abStm2 (If (Assign _ _ _ _) _ Nothing _) = ([2], [4])
abStm2 (If (BinOp _ _ _ _) (Exp (Just (FnCall (Var (Id "printf" _) _) ((Const (StringConst _ str _) _):args) _)) _) Nothing _)
  | (printstrsearch 0 str) == (length args) = ([2, 1], [3, 1])
  | otherwise = ([2, 1], [3, 2])
abStm2 stm = ([], [])

abStm :: Stm -> ([Int], [Int])
abStm (If exp stm Nothing loc) = (\(a, b) (c, d) -> (a ++ c, b ++ d)) (abStm stm) (abStm2 (If exp stm Nothing loc))
abStm (If exp stm1 (Just stm2) loc) = abStm stm2
abStm (For (Right (Just (Assign _ _ _ _))) (Just (BinOp _ _ _ _)) (Just (PostInc _ _)) stm loc) = ([4], [7])
abStm (For e1 (Just (BinOp _ _ _ _)) (Just (PostInc _ _)) stm loc) = ([4], [8])
abStm (For (Right (Just (Assign _ _ _ _))) e2 (Just (PostInc _ _)) stm loc) = ([4], [9])
abStm (For (Right (Just (Assign _ _ _ _))) (Just (BinOp _ _ _ _)) e3 stm loc) = ([4], [10])
abStm (For e1 e2 (Just (PostInc _ _)) stm loc) = ([4], [11])
abStm (For e1 (Just (BinOp _ _ _ _)) e3 stm loc) = ([4], [12])
abStm (For (Right (Just (Assign _ _ _ _))) e2 e3 stm loc) = ([4], [13])
abStm (For e1 e2 e3 stm loc) = ([4], [14])
abStm (Exp (Just (FnCall (Var (Id "printf" _) _) ((Const (StringConst _ str _) _):args) _)) _)--override
  | (printstrsearch 0 str) == (length args) = ([1], [1])
  | otherwise = ([1], [2])
abStm (Exp (Just (FnCall (Var (Id "scanf" _) _) ((Const (StringConst _ str _) _):args) _)) _)--override
  | (scanstrsearch 0 str) == (length args) = ([3], [5])
  | otherwise = ([3], [6])
abStm stm = ([], [])

abStm' :: Stm -> [Int]
abStm' (If (Assign _ _ _ _) _ Nothing _) = [4]
abStm' (If (BinOp _ _ _ _) _ Nothing _) = [3]
abStm' (For (Right (Just (Assign _ _ _ _))) (Just (BinOp _ _ _ _)) (Just (PostInc _ _)) stm loc) = [7]
abStm' (For (Left (InitGroup (DeclSpec _ _ _ _) _ [Init _ _ _ _ _ _] _)) (Just (BinOp _ _ _ _)) (Just (PostInc _ _)) stm loc) = [7]
abStm' (For (Right (Just (Assign _ _ _ _))) (Just (BinOp _ _ _ _)) (Just (Assign _ _ _ _)) stm loc) = [7]
abStm' (For (Left (InitGroup (DeclSpec _ _ _ _) _ [Init _ _ _ _ _ _] _)) (Just (BinOp _ _ _ _)) (Just (Assign _ _ _ _)) stm loc) = [7]
abStm' (For (Right Nothing) (Just (BinOp _ _ _ _)) (Just (PostInc _ _)) stm loc) = [7]
abStm' (For (Right Nothing) (Just (BinOp _ _ _ _)) (Just (Assign _ _ _ _)) stm loc) = [7]
abStm' (For e1 (Just (BinOp _ _ _ _)) (Just (PostInc _ _)) stm loc) = [8]
abStm' (For (Right (Just (Assign _ _ _ _))) e2 (Just (PostInc _ _)) stm loc) = [9]
abStm' (For (Right Nothing) e2 (Just (PostInc _ _)) stm loc) = [9]
abStm' (For e1 (Just (BinOp _ _ _ _)) (Just (Assign _ _ _ _)) stm loc) = [8]
abStm' (For (Right (Just (Assign _ _ _ _))) e2 (Just (Assign _ _ _ _)) stm loc) = [9]
abStm' (For (Right Nothing) e2 (Just (Assign _ _ _ _)) stm loc) = [9]
abStm' (For (Left (InitGroup (DeclSpec _ _ _ _) _ [Init _ _ _ _ _ _] _)) e2 (Just (PostInc _ _)) stm loc) = [9]
abStm' (For (Left (InitGroup (DeclSpec _ _ _ _) _ [Init _ _ _ _ _ _] _)) e2 (Just (Assign _ _ _ _)) stm loc) = [9]
abStm' (For (Right (Just (Assign _ _ _ _))) (Just (BinOp _ _ _ _)) e3 stm loc) = [10]
abStm' (For (Right Nothing) (Just (BinOp _ _ _ _)) e3 stm loc) = [10]
abStm' (For (Left (InitGroup (DeclSpec _ _ _ _) _ [Init _ _ _ _ _ _] _)) (Just (BinOp _ _ _ _)) e3 stm loc) = [10]
abStm' (For e1 e2 (Just (PostInc _ _)) stm loc) = [11]
abStm' (For e1 e2 (Just (Assign _ _ _ _)) stm loc) = [11]
abStm' (For e1 (Just (BinOp _ _ _ _)) e3 stm loc) = [12]
abStm' (For (Right (Just (Assign _ _ _ _))) e2 e3 stm loc) = [13]
abStm' (For (Right Nothing) e2 e3 stm loc) = [13]
abStm' (For (Left (InitGroup (DeclSpec _ _ _ _) _ [Init _ _ _ _ _ _] _)) e2 e3 stm loc) = [13]
abStm' (For e1 e2 e3 stm loc) = [14]
abStm' (Exp (Just (FnCall (Var (Id "printf" _) _) ((Const (StringConst _ str _) _):args) _)) _)--override
  | (countpFormatStrings str) == (length args) = [1]
  | otherwise = [2]
abStm' (Exp (Just (FnCall (Var (Id "scanf" _) _) ((Const (StringConst _ str _) _):args) _)) _)--override
  | (countsFormatStrings str) == (length args) = [5]
  | otherwise = [6]
abStm' stm = []

countControlStm :: Stm -> [Int]
countControlStm (If _ _ Nothing _)  = [1]
countControlStm (If _ _ (Just _) _) = [2] 
countControlStm (Switch _ _ _)      = [3]
countControlStm (While _ _ _)       = [4]
countControlStm (For _ _ _ _ _)     = [5]
countControlStm (DoWhile _ _ _)     = [6]
countControlStm _                   = []

nameStm :: InitGroup -> [String]
nameStm (InitGroup declspec attr [Init (Id str _) decl Nothing Nothing attr2 _] _) = [str]

countControl :: [Definition] -> [Int]
countControl = everything (++) ([] `mkQ` abStm')

countControl2 :: [Definition] -> [String]
countControl2 = everything (++) ([] `mkQ` nameStm)

countControlStm2 :: Stm -> [Int]
countControlStm2 (If _ _ Nothing _)  = [1,0,0,0,0,0,0,0,0,0]
countControlStm2 (If _ _ (Just _) _) = [0,1,0,0,0,0,0,0,0,0] 
countControlStm2 (Switch _ _ _)      = [0,0,1,0,0,0,0,0,0,0]
countControlStm2 (While _ _ _)       = [0,0,0,1,0,0,0,0,0,0]
countControlStm2 (For _ _ _ _ _)     = [0,0,0,0,1,0,0,0,0,0]
countControlStm2 (DoWhile _ _ _)     = [0,0,0,0,0,1,0,0,0,0]
countControlStm2 (Exp (Just (FnCall (Var (Id "printf" _) _) _ _)) _) = [0,0,0,0,0,0,1,0,0,0]
-- countControlStm2 (InitGroup declspec attr [Init (Id str _) decl Nothing Nothing attr2 _] _) = [0,0,0,0,0,0,0,1]
countControlStm2 _                   = [0,0..]--[0, 0, 0, 0, 0, 0, 0, 0, 0]

countControlStm3 :: InitGroup -> [Int]
countControlStm3 (InitGroup declspec attr [Init (Id str _) decl Nothing Nothing attr2 _] _) = [0,0,0,0,0,0,0,1,0,0]
countControlStm3 _ = [0,0..]

countControlStm4 :: Func -> [Int]
countControlStm4 (Func (DeclSpec storage typequal typespec loc) id decl params blockitem _)
  | ((typespec == (Tint Nothing loc)) == True) = [0,0,0,0,0,0,0,0,1,0]
  | ((typespec == (Tvoid loc)) == True) = [0,0,0,0,0,0,0,0,0,1]
countControlStm4 (OldFunc (DeclSpec storage typequal typespec loc) id decl _ _ blockItem _)
  | ((typespec == (Tint Nothing loc)) == True) = [0,0,0,0,0,0,0,0,1,0]
  | ((typespec == (Tvoid loc)) == True) = [0,0,0,0,0,0,0,0,0,1]

countStm1 :: [Definition] -> [Int]
countStm1 = everything (zipWith (+)) ([0,0..] `mkQ` countControlStm2)

countStm2 :: [Definition] -> [Int]
countStm2 = everything (zipWith (+)) ([0,0..] `mkQ` countControlStm3)

countStm3 :: [Definition] -> [Int]
countStm3 = everything (zipWith (+)) ([0,0..] `mkQ` countControlStm4)

countStm0 :: [Definition] -> [Int]
countStm0 defs = zipWith (+) (countStm1 defs) (countStm2 defs)

countStm :: [Definition] -> [Int]
countStm defs = zipWith (+) (countStm0 defs) (countStm3 defs)



-- catchStm :: Stm -> String
-- catchStm stm = trace ("Test: " ++ show stm) $
--     case stm of
--       If (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm Nothing _ -> " --> If{"++id++" < "++str++"}<br>If -- True --> True[ifstm]"
--       If (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm Nothing _ -> " --> If{"++id++" > "++str++"}<br>If -- True --> True[ifstm]"
--       If (BinOp Eq (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm Nothing _ -> " --> If{"++id++" == "++str++"}<br>If -- True --> True[ifstm]"
--       If (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm (Just _) _ -> " --> If{"++id++" < "++str++"}<br>If -- True --> True[ifstm]<br>If -- False --> False[else]"
--       If (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm (Just _) _ -> " --> If{"++id++" > "++str++"}<br>If -- True --> True[ifstm]<br>If -- False --> False[else]"
--       If (BinOp Eq (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm (Just _) _ -> " --> If{"++id++" == "++str++"}<br>If -- True --> True[ifstm]<br>If -- False --> False[else]"
--       If _ _ Nothing _ -> " --> If{if < str}<br>If -- True --> True[ifstm]"
--       _ -> "?"
-- -- catchStm (If (Assign _ (Var (Id id _) _) (Const (_ str _ const _) _) _) Exp Nothing _) = " --> If{条件式}"
-- catchStm (If (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm Nothing _) = " --> If{"++id++" < "++str++"}<br>If -- True --> True[ifstm]"
-- catchStm (If (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm Nothing _) = " --> If{"++id++" > "++str++"}<br>If -- True --> True[ifstm]"
-- catchStm (If (BinOp Eq (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm Nothing _) = " --> If{"++id++" == "++str++"}<br>If -- True --> True[ifstm]"
-- catchStm (If (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm (Just _) _) = " --> If{"++id++" < "++str++"}<br>If -- True --> True[ifstm]<br>If -- False --> False[else]"
-- catchStm (If (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm (Just _) _) = " --> If{"++id++" > "++str++"}<br>If -- True --> True[ifstm]<br>If -- False --> False[else]"
-- catchStm (If (BinOp Eq (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stm (Just _) _) = " --> If{"++id++" == "++str++"}<br>If -- True --> True[ifstm]<br>If -- False --> False[else]"
-- catchStm (If _ _ Nothing _) = " --> If{if < str}<br>If -- True --> True[ifstm]"
-- catchStm _ = "?"

-- catchdup :: [Stm] -> Double -> String
-- catchdup (s:st) counter = catchStm s counter ++ catchdup st counter
-- catchdup [] _ = ""

catStm :: [BlockItem] -> Double -> String
-- catStm (item:[]) counter = case item of
--                              BlockStm s -> catchStm s counter
--                              _ -> ""
catStm (item:block) counter = case item of
                                BlockStm s -> catchStm s counter ++ catStm block (counter+1)
                                _ -> ""
catStm _ _ = ""


catchStm :: Stm -> Double -> String
-- catchStm stm counter = trace ("Test: " ++ show stm) $
catchStm stm counter =
    case stm of
      Block blockitem _ ->
        catStm blockitem counter
      If (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " < " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Le (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " < " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " > " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Ge (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " > " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Eq (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " == " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " < " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (catchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Le (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " < " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (catchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " > " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (catchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Ge (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " > " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (catchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Eq (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if\n" ++ id ++ " == " ++ str ++ "}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (catchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If _ block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If _ stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{if}<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (catchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " < " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " <= " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " > " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " >= " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " < " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " <= " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " > " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " >= " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " < " ++ str2 ++ " ; " ++ str3 ++ "-=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " <= " ++ str2 ++ " ; " ++ str3 ++ "-=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " > " ++ str2 ++ " ; " ++ str3 ++ "-=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " >= " ++ str2 ++ " ; " ++ str3 ++ "-=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " < " ++ str2 ++ " ; " ++ str3 ++ "+=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " <= " ++ str2 ++ " ; " ++ str3 ++ "+=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " > " ++ str2 ++ " ; " ++ str3 ++ "+=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\n" ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " >= " ++ str2 ++ " ; " ++ str3 ++ "+=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " < " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " <= " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " > " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " >= " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " < " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " <= " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " > " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " >= " ++ str2 ++ " ; " ++ str3 ++ "++}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " < " ++ str2 ++ " ; " ++ str3 ++ "-=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " <= " ++ str2 ++ " ; " ++ str3 ++ "-=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " > " ++ str2 ++ " ; " ++ str3 ++ "-=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " >= " ++ str2 ++ " ; " ++ str3 ++ "-=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " < " ++ str2 ++ " ; " ++ str3 ++ "+=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " <= " ++ str2 ++ " ; " ++ str3 ++ "+=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " > " ++ str2 ++ " ; " ++ str3 ++ "+=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for\nint " ++ id ++ " = " ++ str ++ " ; " ++ id2 ++ " >= " ++ str2 ++ " ; " ++ str3 ++ "+=" ++ str4 ++"}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For _ _ _ block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{for}<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      While (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block _ ->
          " While" ++ (printf "%.6f" counter) ++ "{While\n" ++ id ++ " < " ++ str ++ "}<br>While" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " While" ++ (printf "%.6f" counter) ++ "<br>While" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "WhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      While (BinOp Le (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block _ ->
          " While" ++ (printf "%.6f" counter) ++ "{While\n" ++ id ++ " <= " ++ str ++ "}<br>While" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " While" ++ (printf "%.6f" counter) ++ "<br>While" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "WhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      While (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block _ ->
          " While" ++ (printf "%.6f" counter) ++ "{While\n" ++ id ++ " > " ++ str ++ "}<br>While" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " While" ++ (printf "%.6f" counter) ++ "<br>While" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "WhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      While (BinOp Ge (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block _ ->
          " While" ++ (printf "%.6f" counter) ++ "{While\n" ++ id ++ " >= " ++ str ++ "}<br>While" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (catchStm block counter) ++ " While" ++ (printf "%.6f" counter) ++ "<br>While" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "WhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      DoWhile block (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) _ ->
          " DoWhile" ++ (printf "%.6f" counter) ++ "{中身を実行} --> " ++ (catchStm block counter) ++ "DoWhileTF" ++ (printf "%.6f" counter)++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ "{While\n" ++ id ++ " < " ++ str ++ "}<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- True --> DoWhile" ++ (printf "%.6f" counter) ++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "DoWhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      DoWhile block (BinOp Le (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) _ ->
          " DoWhile" ++ (printf "%.6f" counter) ++ "{中身を実行} --> " ++ (catchStm block counter) ++ "DoWhileTF" ++ (printf "%.6f" counter)++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ "{While\n" ++ id ++ " <= " ++ str ++ "}<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- True --> DoWhile" ++ (printf "%.6f" counter) ++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "DoWhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      DoWhile block (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) _ ->
          " DoWhile" ++ (printf "%.6f" counter) ++ "{中身を実行} --> " ++ (catchStm block counter) ++ "DoWhileTF" ++ (printf "%.6f" counter)++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ "{While\n" ++ id ++ " > " ++ str ++ "}<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- True --> DoWhile" ++ (printf "%.6f" counter) ++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "DoWhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      DoWhile block (BinOp Ge (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) _ ->
          " DoWhile" ++ (printf "%.6f" counter) ++ "{中身を実行} --> " ++ (catchStm block counter) ++ "DoWhileTF" ++ (printf "%.6f" counter)++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ "{While\n" ++ id ++ " >= " ++ str ++ "}<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- True --> DoWhile" ++ (printf "%.6f" counter) ++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "DoWhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      Exp (Just (FnCall (Var (Id "printf" _) _) ((Const (StringConst _ str _) _):args) _)) _ ->
          " Printf" ++ (printf "%.6f" (counter+0.001)) ++ "[printf\n" ++ str ++ " ] -->"
      Return _ _ -> " Return" ++ (printf "%.6f" (counter+0.1)) ++ "[Return]"
      _ -> ""

correctcatStm :: [BlockItem] -> Double -> String
correctcatStm (item:block) counter = case item of
                                BlockStm s -> correctcatchStm s counter ++ correctcatStm block (counter+1)
                                _ -> ""
correctcatStm _ _ = ""

correctcatchStm :: Stm -> Double -> String
-- catchStm stm counter = trace ("Test: " ++ show stm) $
correctcatchStm stm counter =
    case stm of
      Block blockitem _ ->
        correctcatStm blockitem counter
      If (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Le (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Ge (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Eq (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (correctcatchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Le (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (correctcatchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (correctcatchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Ge (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (correctcatchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If (BinOp Eq (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (correctcatchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If _ block Nothing _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False --> IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      If _ stmt (Just stmf) _ ->
          " If" ++ (printf "%.6f" counter) ++ "{ }<br>If" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm stmt counter) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了]<br>If" ++ (printf "%.6f" counter) ++ " -- False -->" ++ (correctcatchStm stmf (counter+0.0001)) ++ " IfEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Right (Just (Assign (Var (Id id _) _) JustAssign (Const (IntConst str _ _ _) _) _))) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostInc (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (PostDec (Var (Id str3 _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) SubAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Lt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Le (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Gt (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For (Left (InitGroup (DeclSpec _ _ (Tint _ _) _) _ [Init (Id id _) _ _ (Just (ExpInitializer (Const (IntConst str _ _ _) _) _)) _ _] _)) (Just (BinOp Ge (Var (Id id2 _) _) (Const (IntConst str2 _ _ _) _) _)) (Just (Assign (Var (Id str3 _) _) AddAssign (Const (IntConst str4 _ _ _) _) _)) block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      For _ _ _ block _ ->
          " For" ++ (printf "%.6f" counter) ++ "{ }<br>For" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " For" ++ (printf "%.6f" counter) ++ "<br>For" ++ (printf "%.6f" counter) ++ " -- False --> False" ++ (printf "%.6f" counter) ++ "ForEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      While (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block _ ->
          " While" ++ (printf "%.6f" counter) ++ "{ }<br>While" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " While" ++ (printf "%.6f" counter) ++ "<br>While" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "WhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      While (BinOp Le (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block _ ->
          " While" ++ (printf "%.6f" counter) ++ "{ }<br>While" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " While" ++ (printf "%.6f" counter) ++ "<br>While" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "WhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      While (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block _ ->
          " While" ++ (printf "%.6f" counter) ++ "{ }<br>While" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " While" ++ (printf "%.6f" counter) ++ "<br>While" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "WhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      While (BinOp Ge (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) block _ ->
          " While" ++ (printf "%.6f" counter) ++ "{ }<br>While" ++ (printf "%.6f" counter) ++ " -- True -->" ++ (correctcatchStm block counter) ++ " While" ++ (printf "%.6f" counter) ++ "<br>While" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "WhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      DoWhile block (BinOp Lt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) _ ->
          " DoWhile" ++ (printf "%.6f" counter) ++ "{ } --> " ++ (correctcatchStm block counter) ++ "DoWhileTF" ++ (printf "%.6f" counter)++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ "{ }<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- True --> DoWhile" ++ (printf "%.6f" counter) ++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "DoWhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      DoWhile block (BinOp Le (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) _ ->
          " DoWhile" ++ (printf "%.6f" counter) ++ "{ } --> " ++ (correctcatchStm block counter) ++ "DoWhileTF" ++ (printf "%.6f" counter)++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ "{ }<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- True --> DoWhile" ++ (printf "%.6f" counter) ++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "DoWhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      DoWhile block (BinOp Gt (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) _ ->
          " DoWhile" ++ (printf "%.6f" counter) ++ "{ } --> " ++ (correctcatchStm block counter) ++ "DoWhileTF" ++ (printf "%.6f" counter)++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ "{ }<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- True --> DoWhile" ++ (printf "%.6f" counter) ++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "DoWhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      DoWhile block (BinOp Ge (Var (Id id _) _) (Const (IntConst str _ _ _) _) _) _ ->
          " DoWhile" ++ (printf "%.6f" counter) ++ "{ } --> " ++ (correctcatchStm block counter) ++ "DoWhileTF" ++ (printf "%.6f" counter)++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ "{ }<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- True --> DoWhile" ++ (printf "%.6f" counter) ++ "<br>DoWhileTF" ++ (printf "%.6f" counter) ++ " -- False -->" ++ "DoWhileEnd" ++ (printf "%.6f" counter) ++ "[インデント終了] -->"
      Exp (Just (FnCall (Var (Id "printf" _) _) ((Const (StringConst _ str _) _):args) _)) _ ->
          " Printf" ++ (printf "%.6f" (counter+0.001)) ++ "[ ] -->"
      Return _ _ -> " Return" ++ (printf "%.6f" (counter+0.1)) ++ "[Return]"
      _ -> ""
-- catchControl :: [Definition] -> String
-- catchControl defs = "graph TD<br>" ++ concatMap catchDefinition defs

-- catchDefinitions :: [Definition] -> String
-- catchDefinitions defs = concatMap catchDefinition defs

-- catchDefinition :: [Definition] -> String
-- catchDefinition ((FuncDef func _):fs) = catchFunc func 0
-- catchDefinition (FuncDef func _) = catchFunc func 0
-- catchDefinition _ = ""
catchControl :: [Definition] -> String
catchControl defs = "graph TD<br>" ++ catchDefinition defs

catchDefinition :: [Definition] -> String
catchDefinition [FuncDef func _] = catchFunc func 1.0
catchDefinition defs = catchMultipleFuncs defs 1.0

catchMultipleFuncs :: [Definition] -> Double -> String
catchMultipleFuncs [] _ = " B[終了]"
catchMultipleFuncs (FuncDef func _:fs) counter = 
    catchFunc func counter ++ " -->" ++ catchMultipleFuncs fs (counter + 1)

catchFunc :: Func -> Double -> String
catchFunc (Func _ (Id id _) _ _ blockItems _) counter = "func"++ (printf "%.6f" counter) ++ "[" ++ id ++ "] -->" ++ catchBlockItems blockItems counter
catchFunc (OldFunc _ (Id id _) _ _ _ blockItems _) counter = "func"++ (printf "%.6f" counter) ++ "[" ++ id ++ "] -->" ++ catchBlockItems blockItems counter

catchBlockItems :: [BlockItem] -> Double -> String
catchBlockItems items counter =
  let (result, _) = foldl (\(acc, c) item -> let (newStr, newCounter) = catchBlockItem item c in (acc ++ newStr, newCounter)) ("", counter) items
  in result

catchBlockItem :: BlockItem -> Double -> (String, Double)
catchBlockItem (BlockStm stm) counter =
  let stmStr = catchStm stm counter
      newCounter = counter + 0.01
  in (stmStr, newCounter)
catchBlockItem _ counter = ("", counter)

correctcatchControl :: [Definition] -> String
correctcatchControl defs = "graph TD<br>" ++ correctcatchDefinition defs

correctcatchDefinition :: [Definition] -> String
correctcatchDefinition [FuncDef func _] = correctcatchFunc func 1.0
correctcatchDefinition defs = correctcatchMultipleFuncs defs 1.0

correctcatchMultipleFuncs :: [Definition] -> Double -> String
correctcatchMultipleFuncs [] _ = " B[終了]"
correctcatchMultipleFuncs (FuncDef func _:fs) counter = 
    correctcatchFunc func counter ++ " -->" ++ correctcatchMultipleFuncs fs (counter + 1)

correctcatchFunc :: Func -> Double -> String
correctcatchFunc (Func _ (Id id _) _ _ blockItems _) counter = "func"++ (printf "%.6f" counter) ++ "[" ++ id ++ "] -->" ++ correctcatchBlockItems blockItems counter
correctcatchFunc (OldFunc _ (Id id _) _ _ _ blockItems _) counter = "func"++ (printf "%.6f" counter) ++ "[" ++ id ++ "] -->" ++ correctcatchBlockItems blockItems counter

correctcatchBlockItems :: [BlockItem] -> Double -> String
correctcatchBlockItems items counter =
  let (result, _) = foldl (\(acc, c) item -> let (newStr, newCounter) = correctcatchBlockItem item c in (acc ++ newStr, newCounter)) ("", counter) items
  in result

correctcatchBlockItem :: BlockItem -> Double -> (String, Double)
correctcatchBlockItem (BlockStm stm) counter =
  let stmStr = correctcatchStm stm counter
      newCounter = counter + 0.01
  in (stmStr, newCounter)
correctcatchBlockItem _ counter = ("", counter)
-- catchControl :: [Definition] -> String
-- catchControl defs = evalState (proDefs defs) 0

-- proDefs :: [Definition] -> State Double String
-- proDefs defs = do
--     results <- mapM proDef defs
--     return $ "graph TD<br>A[Start]" ++ unlines results

-- proDef :: Definition -> State Double String
-- proDef def = do
--     let stmList = everything (++) (mkQ [] extStm) def
--     concat <$> mapM proStm stmList

-- extStm :: Stm -> [Stm]
-- extStm stm = case stm of
--     If {} -> [stm]
--     -- For {} -> [stm]
--     -- Exp {} -> [stm]
--     _     -> []

-- proStm :: Stm -> State Double String
-- proStm stm = do
--     counter <- get
--     let result = catchStm stm counter
--     put (counter + 1)
--     return result

-- catchControl :: [Definition] -> String
-- catchControl = everything (++) ("A[Start]" `mkQ` catchStm)

abBlockItem :: BlockItem -> ([Int], [Int])
abBlockItem (BlockStm stm)
  = let (kind, bool) = abStm stm --Nothing
     in (kind, bool)
abBlockItem bi = ([], [])

abFunc :: Func -> ([Int], [Int])
abFunc (Func declSpec id decl params blockItems loc)
  = let (kind, bool) = unzip $ map abBlockItem blockItems
      in (concat kind, concat bool)
abFunc (OldFunc declSpec id decl ids initGroup blockItems loc)
  = let (kind, bool) = unzip $ map abBlockItem blockItems
      in (concat kind, concat bool)

abDefinition :: Definition -> ([Int], [Int])
abDefinition (FuncDef func loc)
  = let (kind, bool) = abFunc func
      in  (kind, bool)
abDefinition def = ([], [])

abTranslationUnit :: [Definition] -> ([Int], [Int])
abTranslationUnit defs = let (kind, bool) = unzip $ map abDefinition defs
                           in (concat kind, concat bool)

-- typeStm :: Stm -> Maybe Int -> Int --関数の中にreturnがあるか判定
-- typeStm (Return (Just (Const (IntConst str _ int _) _)) _) _ = 1  
-- typeStm stm _ = 0

countreturn :: Stm -> [Int]
countreturn (Return _ _) = [1]
countreturn _ = [0]

countR :: Stm -> [Int]
countR = everything (zipWith (+)) ([0,0..] `mkQ` countreturn)

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

typeStm :: Stm -> Maybe Int -> Int --関数の中にreturnがあるか判定
typeStm stm _
  | sumList (countR stm) > 0 = 1
  | otherwise = 0

typeBlockItem :: BlockItem -> Int
typeBlockItem (BlockStm stm)
  = let num = typeStm stm Nothing
     in num
typeBlockItem bi = 0

typestruct :: DeclSpec -> [Int] --関数がint型-１
typestruct (DeclSpec storage typequal typespec loc)
  | ((typespec == (Tvoid loc)) == True) = [0]
  | ((typespec == (Tint Nothing loc)) == True) = [1]
  | otherwise = [0]

typeFunc :: Func -> ([Int], [Int])
typeFunc (Func declSpec id decl params blockItems loc)
  = let bool = map typeBlockItem blockItems
        kind = typestruct declSpec 
      in (kind, bool)
typeFunc (OldFunc declSpec id decl ids initGroup blockItems loc)
  = let bool = map typeBlockItem blockItems
        kind = typestruct declSpec 
      in (kind, bool)

typeDefinition :: Definition -> ([Int], [Int])
typeDefinition (FuncDef func loc)
  = let (kind, bool) = typeFunc func
      in  (kind, bool)
typeDefinition def = ([], [])

typeSpec :: [Definition] -> ([[Int]], [[Int]])
typeSpec defs = let (kind, bool) = unzip $ map typeDefinition defs
                  in (kind, bool)

namelist :: Id -> ([String], [Int])
namelist (Id string loc) = ([string], [0])

nameFunc :: Func -> ([String], [Int])
nameFunc (Func declSpec id decl params blockItems loc)
  = let (kind, bool) = namelist id
--        kind2 = nameBlockitem blockItems 
      in (kind, bool)
nameFunc (OldFunc declSpec id decl _ _ blockItems loc)
  = let (kind, bool) = namelist id
      in (kind, bool)

nameDefinition :: Definition -> ([String], [Int])
nameDefinition (FuncDef func loc)
  = let (kind, bool) = nameFunc func
      in  (kind, bool)
nameDefinition def = ([], [])

funcName :: [Definition] -> ([String], [Int])
funcName defs = let (kind, bool) = unzip $ map nameDefinition defs
                  in (concat kind, concat bool)

linenum :: Int -> Int -> [Int] -> [Int]
linenum maxnum num list = if maxnum > num then linenum maxnum (num+1) (list++[num]) else list

printffind :: Int -> String -> Int
printffind num (x1:x2:x3:x4:x5:x6:xs)
  | (((x1 == 'p') && (x2 == 'r') && (x3 == 'i') && (x4 == 'n') && (x5 == 't') && (x6 == 'f')) == True) = printffind (num+1) xs
  | otherwise = printffind num (x2:x3:x4:x5:x6:xs)
printffind num _ = num

printfsearch :: Int -> String -> Int
printfsearch num source = do
                              printffind 0 source

scanffind :: Int -> String -> Int
scanffind num (x1:x2:x3:x4:x5:xs)
  | (((x1 == 's') && (x2 == 'c') && (x3 == 'a') && (x4 == 'n') && (x5 == 'f')) == True) = scanffind (num+1) xs
  | otherwise = scanffind num (x2:x3:x4:x5:xs)
scanffind num _ = num

scanfsearch :: Int -> String -> Int
scanfsearch num source = do
                              scanffind 0 source

iffind :: Int -> String -> Int
iffind num (x1:x2:xs)
  | (((x1 == 'i') && (x2 == 'f')) == True) = iffind (num+1) xs
  | otherwise = iffind num (x2:xs)
iffind num _ = num

ifsearch :: Int -> String -> Int
ifsearch num source = do
                              iffind 0 source

forfind :: Int -> String -> Int
forfind num (x1:x2:x3:xs)
  | (((x1 == 'f') && (x2 == 'o') && (x3 == 'r')) == True) = forfind (num+1) xs
  | otherwise = forfind num (x2:x3:xs)
forfind num _ = num

forsearch :: Int -> String -> Int
forsearch num source = do
                           forfind 0 source

printfboolcheck :: [Int] -> Int -> [Int] -> [Int]
printfboolcheck (b:bs) num list | b == 2 = printfboolcheck bs (num+1) (list++[num])
                                | b == 1 = printfboolcheck bs (num+1) list
                                | otherwise = printfboolcheck bs num list
printfboolcheck _ num list = list

scanfboolcheck :: [Int] -> Int -> [Int] -> [Int]
scanfboolcheck (b:bs) num list | b == 6 = scanfboolcheck bs (num+1) (list++[num])
                               | b == 5 = scanfboolcheck bs (num+1) list
                               | otherwise = scanfboolcheck bs num list
scanfboolcheck _ num list = list

ifboolcheck :: [Int] -> Int -> [Int] -> [Int]
ifboolcheck (b:bs) num list | b == 4 = ifboolcheck bs (num+1) (list++[num])
                            | b == 3 = ifboolcheck bs (num+1) list
                            | otherwise = ifboolcheck bs num list
ifboolcheck _ num list = list

forboolcheck :: [Int] -> Int -> [Int] -> [Int] -> ([Int], [Int])
forboolcheck (b:bs) num list1 list2 | (((b >= 8) && (b <= 14)) == True) = forboolcheck bs (num+1) (list1++[num]) (list2++[b])
                                    | b == 7 = forboolcheck bs (num+1) list1 list2
                                    | otherwise = forboolcheck bs num list1 list2
forboolcheck _ num list1 list2 = (list1, list2)

checklist :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
checklist num1 num2 (s:ss) (b:bs) list | s == 0 = checklist (num1+1) num2 ss (b:bs) list
                                       | (s == 1 && b /= num2) = checklist (num1+1) (num2+1) ss (b:bs) list
                                       | (s == 1 && b == num2) = checklist (num1+1) (num2+1) ss bs (list++[num1])
checklist _ _ _ _ list = list

forchecklist :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
forchecklist num1 num2 (s:ss) (b:bs) list | s == 0 = forchecklist (num1+1) num2 ss (b:bs) list
                                          | (s == 1 && b /= num2) = forchecklist (num1+1) (num2+1) ss (b:bs) list
                                          | (s == 1 && b == num2) = forchecklist (num1+1) (num2+1) ss bs (list++[num1])
forchecklist _ _ _ _ list = list

funccheck :: [[Int]] -> [[Int]] -> [Int] -> [Int]
funccheck (k:ks) (b:bs) list
  | ((k == [0]) == True) = funccheck ks bs (list++[0])
  | (((k == [1]) && ((sum b) == 1)) == True) = funccheck ks bs (list++[0])
  | (((k == [1]) && ((sum b) == 0)) == True) = funccheck ks bs (list++[1])
  |otherwise = funccheck ks bs list
funccheck [] [] list = list

duplicheck :: [String] -> [Int] -> [Int]
duplicheck (l1:l2:ls) list
  | ((l1 == l2) == True) = (list++[1])
  | otherwise = duplicheck (l1:ls) list
duplicheck _ list = (list++[0])

dupli :: [String] -> [Int] -> [Int]
dupli (l:ls) list = (list ++ (duplicheck (l:ls) [])) ++ dupli ls list
dupli _ list = list

stripStart :: String -> String
stripStart = dropWhile isSpace

strlen :: [Int] -> [Int] -> [Int] -> [Int]
strlen (l:ls) (s:ss) list = strlen ls ss (list++[l-s])
strlen _ _ list = list

boxfind :: Int -> String -> Int
boxfind num (x:xs)
  | (x == '{') = boxfind (num+1) xs
  | (x == '}') = boxfind (num-1) xs
  | otherwise = boxfind num xs
boxfind num _ = num

boxsearch :: Int -> String -> Int
boxsearch num source = do
                            boxfind 0 source

numlist :: Bool -> [Int] -> Int -> Int -> Int -> [Int]
numlist b list num num2 len
  | (num > len) = list
  | ((b == True) && (num >= num2)) = numlist b (list++[4]) (num+1) num2 len
  | ((b == True) && (num < num2)) = numlist b (list++[0]) (num+1) num2 len
  | ((b == False) && (num >= num2)) = numlist b (list++[-4]) (num+1) num2 len
  | ((b == False) && (num < num2)) = numlist b (list++[0]) (num+1) num2 len

sumlist :: [Int] -> [Int] -> [Int] -> [Int]
sumlist (l:ls) (s:ss) list = sumlist ls ss (list++[l+s])
sumlist _ _ list = list

divlist :: [Int] -> [Int] -> [Int] -> [Int]
divlist (l:ls) (s:ss) list = divlist ls ss (list++[l-s])
divlist _ _ list = list

divother :: [Int] -> [Int] -> [Int] -> [Int]
divother (l:ls) (s:ss) list
  | (((l == 1) && (s == 1)) ==True) = divother ls ss (list++[0])
  | otherwise = divother ls ss (list++[l])
divother _ _ list = list

makelist :: Int -> Int -> [Int] -> [Int]
makelist num len list
  | (num < len) = makelist (num+1) len (list++[0])
  | otherwise = list

boxnum :: [Int] -> Int -> Int -> [Int] -> [Int]
boxnum (b:bl) num len list
  | (b == 1) = boxnum bl (num+1) len (sumlist list (numlist True [] 1 (num+1) len) [])
  | (b == -1) = boxnum bl (num+1) len (sumlist list (numlist False [] 1 num len) [])
  | otherwise = boxnum bl (num+1) len list
boxnum _ _ _ list = list

divsam :: [Int] -> [Int] -> [Int] -> [Int]
divsam (l:ls) (s:ss) list
  | (s == 1) = divsam ls ss (list++[0])
  | otherwise = divsam ls ss (list++[l])
divsam _ _ list = list

funcfind :: Int -> String -> Int
funcfind num (x1:x2:x3:x4:xs)
  | (((x1 == '(') && (x2 == 'i') && (x3 == 'n') && (x4 == 't')) == True) = funcfind (num+1) xs
  | (((x1 == '(') && (x2 == 'v') && (x3 == 'o') && (x4 == 'i')) == True) = funcfind (num+1) xs
  | otherwise = funcfind num (x2:x3:x4:xs)
funcfind num _ = num

findFunctionStart :: String -> Bool
findFunctionStart code =
    let linesOfCode = Data.List.lines code
        pattern :: String
        pattern = "^[[:space:]]*\\w+[[:space:]]+\\w+\\([^)]*\\)[[:space:]]*\\{"
        matches = Data.List.any (\(line, _) -> line =~ pattern) (Data.List.zip linesOfCode [1..])
      in matches

funcfind' :: String -> Int
funcfind' xs
  | findFunctionStart xs == True = 1
  | otherwise = 0

funcsearch :: Int -> String -> Int
funcsearch num source = funcfind 0 source

funcchecklist :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
funcchecklist num1 num2 (s:ss) (b:bs) list | s == 0 = funcchecklist (num1+1) num2 ss (b:bs) list
                                           | (s == 1 && b == 0) = funcchecklist (num1+1) (num2+1) ss bs list
                                           | (s == 1 && b == 1) = funcchecklist (num1+1) (num2+1) ss bs (list++[num1])
funcchecklist _ _ _ _ list = list

indentpluscheck :: String -> Int -> Int
indentpluscheck (x:xs) num
  | x == '{' = indentpluscheck xs (num+4)
  | otherwise = indentpluscheck xs num
indentpluscheck _ num = num

indentminuscheck :: String -> Int -> Int
indentminuscheck (x:xs) num
  | x == '}' = indentminuscheck xs (num-4)
  | otherwise = indentminuscheck xs num
indentminuscheck _ num = num

addLists :: Num a => [a] -> [a] -> [a]
addLists [] [] = []
addLists (x:xs) (y:ys) = x + y : addLists xs ys
addLists _ _ = []

modifylist :: [Int] -> [Int]
modifylist xs = 0 : (init xs)

updateListOnEmptyLines :: String -> [Int] -> [Int]
updateListOnEmptyLines str list =
  let strings = lines str
    in zipWith (\s num -> if all isWhitespace s then 0 else num) strings list
  where
    isWhitespace :: Char -> Bool
    isWhitespace c = c == ' ' || c == '\t'

addNumbers :: [Int] -> [Int] -> Int -> [Int]
addNumbers (x:xs) list num = addNumbers xs (list++[num+x]) (num+x)
addNumbers _ list num = list

applyIndentPlusCheck :: String -> [Int] -> [Int]
applyIndentPlusCheck str list =
  let linesList = lines str
      result = map (\line -> indentpluscheck line 0) linesList
    in list ++ result

applyIndentMinusCheck :: String -> [Int] -> [Int]
applyIndentMinusCheck str list =
  let linesList = lines str
      result = map (\line -> indentminuscheck line 0) linesList
    in list ++ result

indentcheck :: String -> [Int] -> [Int]
indentcheck str list = do
                         let list2 = addNumbers (applyIndentPlusCheck str list) [] 0
                         let list3 = addNumbers (applyIndentMinusCheck str list) [] 0
                         let list4 = updateListOnEmptyLines str (addLists (modifylist list2) list3)
                           in list4

printindentbool2 :: [Int] -> Int -> [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
printindentbool2 (l:ls) num list bool kind
  | (l > 0) = printindentbool2 ls (num+1) (list++[num]) (bool++[num]) (kind++[1])
  | (l < 0) = printindentbool2 ls (num+1) (list++[num]) (bool++[num]) (kind++[1])
  | otherwise = printindentbool2 ls (num+1) list bool kind
printindentbool2 _ _ list bool kind = (list, bool, kind)

printfbool2 :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
printfbool2 (l:ls) list bool kind = printfbool2 ls (list++[l]) (bool++[l]) (kind++[2])
printfbool2 _ list bool kind = (list, bool, kind)

scanfbool2 :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
scanfbool2 (l:ls) list bool kind = scanfbool2 ls (list++[l]) (bool++[l]) (kind++[3])
scanfbool2 _ list bool kind = (list, bool, kind)

printifbool2 :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
printifbool2 (l:ls) list bool kind = printifbool2 ls (list++[l]) (bool++[l]) (kind++[4])
printifbool2 _ list bool kind = (list, bool, kind)

printduplibool2 :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
printduplibool2 (l:ls) list bool kind = printduplibool2 ls (list++[l]) (bool++[l]) (kind++[5])
printduplibool2 _ list bool kind = (list, bool, kind)

printfuncbool2 :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
printfuncbool2 (l:ls) list bool kind = printfuncbool2 ls (list++[l]) (bool++[l]) (kind++[6])
printfuncbool2 _ list bool kind = (list, bool, kind)

printforbool2 :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
printforbool2 (l:ls) list bool kind = printforbool2 ls (list++[l]) (bool++[l]) (kind++[7])
printforbool2 _ list bool kind = (list, bool, kind)

search :: String -> (String, String, [String], [Int], [[Int]], [[Int]], [Int], [Int], [Definition], [Definition], String, [Int], [Int], [String], [Int],[Int])
search source
  = let source1 = restoreUTF8 source--lines (eraseComments source "")
      in case parseProg source1 of
        Left ex    -> ("","",["error"],[],[],[],[],[],[],[],source1,[],[],[],[],[])
        Right defs0 ->
          let defs = map utf8Definition defs0
              list = []
              (kind2, bool2) = typeSpec defs
              (kind3, bool3) = funcName defs
              ls2 = map untabify (lines source)
            in do
                  let bool = countControl defs
                  let str = lines source1
                  let printfsourcelist = (map (printfsearch 0) str)
                  let printfboollist = (printfboolcheck bool 1 list)
                  let ifsourcelist = (map (ifsearch 0) str)
                  let ifboollist = (ifboolcheck bool 1 list)
                  let scanfsourcelist = (map (scanfsearch 0) str)
                  let scanfboollist = (scanfboolcheck bool 1 list)
                  let forsourcelist = (map (forsearch 0) str)
                  let (forboollist, forbool) = (forboolcheck bool 1 list list)

                  let len = length ls2
                  let lslen = map length ls2
                  let strip = map stripStart ls2
                  let striplen = map length strip
                  let divlen = strlen lslen striplen list
                  let divl = divother divlen (Prelude.map Prelude.length str) list
                  let boxsourcelist = (map (boxsearch 0) str)
                  let bonu = boxnum boxsourcelist 1 len (makelist 1 len list)
                  let botu = divsam bonu (Prelude.map Prelude.length str) list
                  let ind = indentcheck source1 list
                  let div = divlist ind divl list
                  let div2 = updateListOnEmptyLines source1 div
                  let catch = catchControl defs
                  let correctcatch = correctcatchControl defs
                  let funcsourcelist = (map (funcsearch 0) str)
                  let funcsourcelist2 = (map funcfind' str)
                  let count = countControl defs
                  let count2 = countControl2 defs
                  let stm = countStm defs
                  
                  
                  let (in1, in2, in3) = printindentbool2 div2 1 list list list
                  -- let (in1, in2, in3) = (list, list, list)
                  let (pr1, pr2, pr3) = printfbool2 (checklist 1 1 printfsourcelist printfboollist list) in1 in2 in3
                  let (sc1, sc2, sc3) = scanfbool2 (checklist 1 1 scanfsourcelist scanfboollist list) pr1 pr2 pr3
                  let (if1, if2, if3) = printifbool2 (checklist 1 1 ifsourcelist ifboollist list) sc1 sc2 sc3
                  let (du1, du2, du3) = printduplibool2 (funcchecklist 1 1 funcsourcelist2 (dupli kind3 []) list) if1 if2 if3
                  let (fu1, fu2, fu3) = printfuncbool2 (funcchecklist 1 1 funcsourcelist2 (funccheck kind2 bool2 list) list) du1 du2 du3
                  let (fo1, fo2, fo3) = printforbool2 (checklist 1 1 forsourcelist forboollist list) fu1 fu2 fu3
                  (catch, correctcatch, ls2, fo1, bool2, kind2, fo3, div2, defs0, defs, source1, count, forbool, count2, stm, ind)
-- fo1,div1