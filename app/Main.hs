{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, getDirectoryContents, doesFileExist)
import System.FilePath ((</>))
import Control.Exception  -- for SomeException
import Language.C
import Language.C.Quote.C
import Language.C.Syntax
import Language.C.Pretty
import qualified Data.ByteString as BS1
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U8 (fromString, toString)
import qualified MyLib
import Network.Wai as Wai
-- import Network.Wai.Conduit (sourceFile)
import Network.HTTP.Simple
import Network.HTTP.Types as HType ( ok200, status200, status404 )
import Network.HTTP.Types.Header (hContentType)
--import Blaze.ByteString.Builder (copyByteString)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Application.Static as Static
import Network.Wai.Parse
--import Data.Conduit.Binary (sourceFile)
import Data.IORef
-- import Lucid
import Data.Text
import qualified Data.Text.IO as T
import GHC.IO.Encoding
import Data.List
import Data.List.Split(splitOn)
import Data.Char(ord, isSpace)
import qualified Data.ByteString.Lazy as BS --(toStrict,fromStrict)
-- import qualified Data.ByteString.Lazy.UTF8 as LBSU
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS
-- import Data.Text.Lazy (toLazyText)
import Data.Text.Lazy.Builder (fromLazyText, toLazyText)
--import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromMaybe)
import Codec.Binary.UTF8.String
--import Data.Map (Map, insertWith, empty, findWithDefault)
import Text.Regex.Posix


-- findFunctionStart :: String -> [(String, Int)]
-- findFunctionStart code =
--     let linesOfCode = Data.List.lines code
--         pattern :: String
--         pattern = "^[[:space:]]*\\w+[[:space:]]+\\w+\\([^)]*\\)[[:space:]]*\\{"
--         matches = Data.List.filter (\(line, _) -> line =~ pattern) (Data.List.zip linesOfCode [1..])
--     in matches

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

maind :: IO ()
maind = do
    let cCode = ["int add(int a, int b) {",
                "return a + b;\n}\n\nint main() {",
                "return 0;\n}"]
    let functionStarts = Data.List.map funcfind' cCode
    print functionStarts

router :: Wai.Application
router req =
  case Wai.pathInfo req of
    [] -> app req
    ["tutorial"] -> tutorial req
    ["list"] -> listapp req
    ["compare"] -> compareapp req
    ["trouble"] -> trouble req
    ["trouble2"] -> trouble2 req
    ["repo"] -> repo req
    ["teacher"] -> teacher req
    ["submit"] -> teacher req
    _ -> notapp req

samplefunction :: String -> IO String
samplefunction no =
  let filename = "answer_code/" ++ no
    in readFile filename
        -- "1" -> readFile "sampletxt1.txt"
        -- "2" -> readFile "sampletxt2.txt"
        -- "3" -> readFile "sampletxt3.txt"
        -- _ -> readFile "misstxt.txt"

app :: Application
app request respond = do
    case requestMethod request of
        "POST" -> do
            body <- strictRequestBody request
            let rres = LBS.unpack body
            -- let [fileno, res] = Data.List.Split.splitOn "@@@" rres
            -- samtxt <- samplefunction fileno
            -- files <- getDirectoryContents "answer_code"
            -- let validFiles = Data.List.filter (\f -> f /= "." && f /= "..") files
            --     fileLinks = Data.List.map (\f -> "<a href='/?question=" ++ f ++ "'>" ++ f ++ "</a><br>") validFiles
            --     htmlContent = "<html><body>" ++ Data.List.concat fileLinks ++ "</body></html>"
            let restext = LBS.fromStrict (encodeUtf8 (Data.Text.pack (mai rres)))
            -- mai (LBS.unpack body)
            respond $ responseLBS status200 [("Content-Type", "text/plain")] restext
        _ -> do
            respond $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "haslan/index.html" Nothing

listapp :: Application
listapp request respond = do
    case requestMethod request of
        "POST" -> do
            body <- strictRequestBody request
            files <- getDirectoryContents "answer_code"
            let validFiles = Data.List.filter (\f -> f /= "." && f /= "..") files
                fileLinks = Data.List.map (\f -> "<a href='/compare?question=" ++ f ++ "'>" ++ f ++ "</a><br>") validFiles
                htmlContent = "<html><body>" ++ Data.List.concat fileLinks ++ "</body></html>"
            respond $ responseLBS status200 [("Content-Type", "text/plain")] (LBS.fromStrict (encodeUtf8 (Data.Text.pack htmlContent)))
        _ -> do
            respond $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "haslan/listindex.html" Nothing

compareapp :: Application
compareapp request respond = do
    case requestMethod request of
        "POST" -> do
            body <- strictRequestBody request
            let rres = LBS.unpack body
            let [filename, res] = Data.List.Split.splitOn "@@@" rres
            -- samtxt <- samplefunction fileno
            -- tesma fileno
            -- tesma samtxt
            -- files <- getDirectoryContents "answer_code"
            -- let validFiles = Data.List.filter (\f -> f /= "." && f /= "..") files
            --     fileLinks = Data.List.map (\f -> "<a href='/?question=" ++ f ++ "'>" ++ f ++ "</a><br>") validFiles
            --     htmlContent = "<html><body>" ++ Data.List.concat fileLinks ++ "</body></html>"
            let restext = LBS.fromStrict (encodeUtf8 (Data.Text.pack (commai res ("answer_code/" ++ filename))))
            -- mai (LBS.unpack body)
            respond $ responseLBS status200 [("Content-Type", "text/plain")] restext
        _ -> do
            respond $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "haslan/indexs.html" Nothing

tesma :: String -> IO()
tesma source = do
    print source
    let filename = "answer_code/enshu0100.c"
    exists <- doesFileExist filename
    if exists
      then putStrLn "ファイルは存在しています"
      else putStrLn "ファイルが存在しません"

tutorial :: Application
tutorial request respond = do
    case requestMethod request of
        "POST" -> do
            body <- strictRequestBody request
            -- let res = LBS.unpack body
            -- mai2 (LBS.unpack body)
            -- let restext = LBS.fromStrict (encodeUtf8 (Data.Text.pack (mai2 res)))
            respond $ responseLBS status200 [("Content-Type", "text/plain")] body
        _ -> do
            respond $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "haslan/tutorial.html" Nothing

trouble2 :: Application
trouble2 request respond = do
    case requestMethod request of
        "POST" -> do
            body <- strictRequestBody request
            -- let res = LBS.unpack body
            -- mai2 (LBS.unpack body)
            -- let restext = LBS.fromStrict (encodeUtf8 (Data.Text.pack (mai2 res)))
            respond $ responseLBS status200 [("Content-Type", "text/plain")] body
        _ -> do
            respond $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "static/trouble2.html" Nothing


trouble :: Application
trouble request respond = do
    case requestMethod request of
        "POST" -> do
            body <- strictRequestBody request
            let res = LBS.unpack body
            mai2 (LBS.unpack body)
            -- let restext = LBS.fromStrict (encodeUtf8 (Data.Text.pack (mai2 res)))
            respond $ responseLBS status200 [("Content-Type", "text/plain")] body
        _ -> do
            respond $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "haslan/indexs.html" Nothing

teacher :: Application
teacher request respond = do
    case requestMethod request of
        "POST" -> do
            body <- strictRequestBody request
            let res = LBS.unpack body
            let [filename, code] = Data.List.Split.splitOn "@@@" res
            fileList <- answer filename code
            files <- getDirectoryContents "answer_code"
            let validFiles = Data.List.filter (\f -> f /= "." && f /= "..") files
                fileLinks = Data.List.map (\f -> "<a href='/answer_code/" ++ f ++ "'>" ++ f ++ "</a><br>") validFiles
                htmlContent = "<html><body>" ++ Data.List.concat fileLinks ++ "</body></html>"
            let restext = LBS.fromStrict (encodeUtf8 (Data.Text.pack (fileList ++ "@@@@@" ++ htmlContent)))
            respond $ responseLBS status200 [("Content-Type", "text/plain")] restext
        _ -> do
            respond $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "static/teacher.html" Nothing


mai :: String -> String
mai source = do
    let (st, cst, strs, lists, indent, boo, kind, div, defs0, defs, nihon, kindfun, booli, namecount, countstm, ind) = MyLib.search source
    let zi = Prelude.zip lists kind
    let sortList = sortBy (\(a, _) (b, _) -> compare a b) zi
    let (li, ki) = Prelude.unzip sortList
    let list = errorkind0 ki li div booli []
    let duplicateIndices = findDuplicateIndices namecount
    -- let content = readFile "que1.txt"
    -- print samplesource
    -- let res = Prelude.concat list
    -- let ch = if strs == "error" then 1 else 0
    -- let countst = function no
    -- let content = readFileContent "que1.txt"
    -- let (st2, strs2, lists2, indent2, boo2, kind2, div2, defs02, defs2, nihon2, kindfun2, booli2, namecount2, countstm2, ind2) = MyLib.search samtxt
    -- let counts = countstmfunc 1 countstm countstm2 ["<br>"]
    -- let validCode0 = removeComments(source)
    -- let (lineCount0, charCount0) = countValidLinesAndChars(validCode0)
    -- let validCode = removeComments(samtxt)
    -- let (lineCount, charCount) = countValidLinesAndChars(validCode)
    let res = if strs == ["error"]
                  then "解析できないコードです。"
                  else (Prelude.concat list) ++ (Prelude.concat (countStmNumber countstm 1 ["<br>含まれる要素<br>"])) ++ "@@@@@" ++ st
                  --  ++ (Prelude.concat counts) ++ "<br>有効文字数" ++ show charCount0 ++ " 有効行数" ++ show lineCount0 ++ diffcount "<br>有効文字数が" charCount0 charCount ++ diffcount "<br>有効行数が" lineCount0 lineCount
      in res

readFileContent :: String -> String
readFileContent filePath = unsafePerformIO $ readFile filePath

commai :: String -> String -> String
commai source filename = do
    let (st, cst, strs, lists, indent, boo, kind, div, defs0, defs, nihon, kindfun, booli, namecount, countstm, ind) = MyLib.search source
    let zi = Prelude.zip lists kind
    let sortList = sortBy (\(a, _) (b, _) -> compare a b) zi
    let (li, ki) = Prelude.unzip sortList
    let list = errorkind0 ki li div booli []
    let duplicateIndices = findDuplicateIndices namecount
    let sam2 = readFileContent filename
    let (st2, cst2, strs2, lists2, indent2, boo2, kind2, div2, defs02, defs2, nihon2, kindfun2, booli2, namecount2, countstm2, ind2) = MyLib.search sam2
    -- let counts = countstmfunc 1 countstm countstm2 ["<br>"]
    -- let validCode0 = removeComments(source)
    -- let (lineCount0, charCount0) = countValidLinesAndChars(validCode0)
    -- let validCode = removeComments(samtxt)
    -- let (lineCount, charCount) = countValidLinesAndChars(validCode)
    let res = if strs == ["error"]
                  then "解析できないコードです。"
                  else (Prelude.concat list) ++ (Prelude.concat (countStmNumber countstm 1 ["<br>含まれる要素<br>"])) ++ "@@@@@" ++ st ++ "@@@@@" ++ cst2
                  --  ++ (Prelude.concat counts) ++ "<br>有効文字数" ++ show charCount0 ++ " 有効行数" ++ show lineCount0 ++ diffcount "<br>有効文字数が" charCount0 charCount ++ diffcount "<br>有効行数が" lineCount0 lineCount
      in res

mai2 :: String -> IO()
mai2 source0 = do
    let [no, source] = Data.List.Split.splitOn "@@@" source0
    -- let filePath = "static/repo.html"
    -- samplesource <- readFile "sampletxt1.txt"
    -- let [code, comment] = Data.List.Split.splitOn "@@@@@" source
    let (st, cst, strs, lists, indent, boo, kind, div, defs0, defs, nihon, kindfun, booli, namecount, countstm, ind) = MyLib.search source
    -- let (st2, strs2, lists2, indent2, boo2, kind2, div2, defs02, defs2, nihon2, kindfun2, booli2, namecount2, countstm2, ind2) = MyLib.search samplesource
    -- let restext = encodeUtf8 (Data.Text.pack nihon)
    -- let restext2 = encodeUtf8 (Data.Text.pack nihon2)
    print defs
    -- let content = readFileContent "que1.txt"
    -- content <- readFile "question1.txt"
    -- print nihon2
    print st
    -- print st2
    -- print no
    -- print source
    -- print (countStmNumber countstm 1 [])
    -- content <- case no of
    --     "1" -> readFile "question1.txt"
    --     "2" -> readFile "question2.txt"
    --     "3" -> readFile "question3.txt"
    --     _ -> return "No file found"
    -- print content
    -- print nihon
    -- withFile filePath AppendMode $ \handle -> do
    --     let textToWrite = "<details><pre><code>" ++ nihon ++ "</code></pre><br>" ++ nihon2 ++ "</details>"
    --     hPutStr handle textToWrite

answer :: FilePath -> String -> IO String
answer filename code = do
  -- print filename
  -- print code
  let fileName = "answer_code/" ++ filename
  createDirectoryIfMissing True "answer_code"
  writeFile fileName ""
  handle <- openFile fileName AppendMode
  hSetEncoding handle utf8
  hPutStrLn handle code
  hClose handle
  files <- getDirectoryContents "answer_code"
  let validFiles = Data.List.filter (\f -> f /= "." && f /= "..") files
  let textFiles = Data.List.map Data.Text.pack validFiles
  return $ Data.Text.unpack (Data.Text.intercalate "\n" textFiles)

outputStrings :: FilePath -> [String] -> IO ()
outputStrings filename xs = do
  handle <- openFile filename WriteMode
  mapM_ (hPutStrLn handle) xs
  hClose handle

lenfunc :: Int -> Int -> [String] -> [String]
lenfunc len num list
  | (len >= num) = lenfunc len (num+1) (list++[show num])
  | otherwise = list

appendindent :: Int -> String -> String
appendindent num str
  | (num > 0) = appendindent (num-1) ("&nbsp;"++str)
  | otherwise = str

inden :: [Int] -> [String] -> [String] -> [String]
inden (i:is) (l:ls) list
  | (i > 0) = inden is ls (list++[appendindent i l])
  | otherwise = inden is ls (list++[l])
inden _ _ list = list

colorchange :: String -> [Int] -> String
colorchange str (i:is)
  | (str == (show i)) = ("<font color='red'><strong>"++str++"</strong></font>")
  | otherwise = colorchange str is
colorchange str _ = str

fontcol :: [String] -> [Int] -> [String] -> [String]
fontcol (s:ss) boo strlist = fontcol ss boo (strlist++[colorchange s boo])
fontcol _ _ strlist = strlist

specialchara :: String -> String -> String
specialchara (s:ss) str
  | (s == '<') = specialchara ss (str++"&lt")
  | (s == '>') = specialchara ss (str++"&gt")
  | otherwise = specialchara ss (str++[s])
specialchara _ str = str

specialsearch :: Int -> String -> Int
specialsearch num (x:xs)
  | (x == '<')= specialsearch (num+1) xs
  | otherwise = specialsearch num xs
specialsearch num _ = num

special :: [String] -> [String] -> [String]
special (s:ss) str = special ss (str++[specialchara s ""])
special _ str = str

main :: IO ()
main = do
  -- maind
  putStrLn $ "http://localhost:8080"
  Warp.run 8080 router

samplecode :: String -> String
samplecode sample
  | sample == "sample0" = "sampletxt0.txt"
  | sample == "sample1" = "sampletxt1.txt"
  | sample == "sample2" = "sampletxt2.txt"
  -- | otherwise = "0"

divother :: [Int] -> [Int] -> [Int] -> [Int]
divother (l:ls) (s:ss) list
 | (s == 1) = divother ls ss (list++[0])
 | otherwise = divother ls ss (list++[l])
divother _ _ list = list

findDuplicateIndices :: Eq a => [a] -> [Int]
findDuplicateIndices xs = Data.List.reverse $ findDuplicateIndices' xs []
    where
        findDuplicateIndices' [] _ = []
        findDuplicateIndices' (y:ys) seen =
            case elemIndex y ys of
                Just idx -> (Data.List.length xs - Data.List.length ys + idx) : findDuplicateIndices' ys (y:seen)
                Nothing  -> findDuplicateIndices' ys (y:seen)

countstms :: Int -> String -> String
countstms num str
  | num == 0 = ""
  | otherwise = str ++ show num ++ "<br>"

countStmNumber :: [Int] -> Int -> [String] -> [String]
countStmNumber (s:ls) num list
  | num == 1 = countStmNumber ls (num+1) (list++[countstms s "If文 "])
  | num == 2 = countStmNumber ls (num+1) (list++[countstms s "If-Else "])
  | num == 3 = countStmNumber ls (num+1) (list++[countstms s "Switch文 "])
  | num == 4 = countStmNumber ls (num+1) (list++[countstms s "While文 "])
  | num == 5 = countStmNumber ls (num+1) (list++[countstms s "For文 "])
  | num == 6 = countStmNumber ls (num+1) (list++[countstms s "DoWhile文 "])
  | num == 7 = countStmNumber ls (num+1) (list++[countstms s "Printf "])
  | num == 8 = countStmNumber ls (num+1) (list++[countstms s "変数 "])
  | num == 9 = countStmNumber ls (num+1) (list++[countstms s "関数(int) "])
  | num == 10 = countStmNumber ls (num+1) (list++[countstms s "関数(void) "])
countStmNumber _ _ list = list

-- countStmNumber :: [Int] -> Int -> [String] -> [String]
-- countStmNumber (s:ls) num list
--   | num == 1 = countStmNumber ls (num+1) (list++["If "++show s++"<br>"])
--   | num == 2 = countStmNumber ls (num+1) (list++["If-Else "++show s++"<br>"])
--   | num == 3 = countStmNumber ls (num+1) (list++["Switch "++show s++"<br>"])
--   | num == 4 = countStmNumber ls (num+1) (list++["While "++show s++"<br>"])
--   | num == 5 = countStmNumber ls (num+1) (list++["For "++show s++"<br>"])
--   | num == 6 = countStmNumber ls (num+1) (list++["DoWhile "++show s++"<br>"])
--   | num == 7 = countStmNumber ls (num+1) (list++["Printf "++show s++"<br>"])
--   | num == 8 = countStmNumber ls (num+1) (list++["Variable "++show s++"<br>"])
--   | num == 9 = countStmNumber ls (num+1) (list++["Function(int) "++show s++"<br>"])
--   | num == 10 = countStmNumber ls (num+1) (list++["Function(void) "++show s++"<br>"])
-- countStmNumber _ _ list = list

repo :: Application
repo req send = do
  send $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "static/repo.html" Nothing

notapp :: Wai.Application
notapp req send
  = send $ Wai.responseBuilder HType.status404 [] "not found"

indenterror :: Int -> Int -> String
indenterror l i 
  | (i > 0) = show l ++ "行目 インデントをスペース" ++ show i ++ "個分追加してください<br>"
  | (i < 0) = show l ++ "行目 インデントをスペース" ++ show (i*(-1)) ++ "個分減らしてください<br>"
  | otherwise = ""

printferror :: Int -> String
printferror l = show l ++ "行目 printfで指定されている引数の数が違います<br>"

scanferror :: Int -> String
scanferror l = show l ++ "行目 scanfで指定されている引数の数が違います<br>"

iferror :: Int -> String
iferror l = show l ++ "行目 if文の条件式は = ではなく == を使用してください<br>"

duplierror :: Int -> String
duplierror l = show l ++ "行目の関数名が他の関数と重複しています<br>"

funcerror :: Int -> String
funcerror l = show l ++ "行目の関数にreturnがありません<br>"

forerror :: Int -> Int -> String
forerror l b
  | b == 8 = show l ++ "行目 for文の初期化式が正しくありません<br>"
  | b == 9 = show l ++ "行目 for文の条件式が正しくありません<br>"
  | b == 10 = show l ++ "行目 for文の変化式が正しくありません<br>"
  | b == 11 = show l ++ "行目 for文の初期化式と条件式が正しくありません<br>"
  | b == 12 = show l ++ "行目 for文の初期化式と変化式が正しくありません<br>"
  | b == 13 = show l ++ "行目 for文の条件式と変化式が正しくありません<br>"
  | b == 14 = show l ++ "行目 for文の初期化式、条件式、変化式が正しくありません<br>"
  | otherwise = ""

errorkind :: [Int] -> [Int] -> [Int] -> [String] -> [String]
errorkind (k:ks) (l:ls) indent str
  | (k == 1) = errorkind ks ls indent (str++[indenterror l (indent !! (l-1))])
  | (k == 2) = errorkind ks ls indent (str++[printferror l])
  | (k == 3) = errorkind ks ls indent (str++[scanferror l])
  | (k == 4) = errorkind ks ls indent (str++[iferror l])
  | (k == 5) = errorkind ks ls indent (str++[duplierror l])
  | (k == 6) = errorkind ks ls indent (str++[funcerror l])
  | otherwise = str
errorkind _ _ _ str = str

errorkind2 :: [Int] -> [Int] -> [Int] -> [Int] -> [String] -> [String]
errorkind2 (k:ks) (l:ls) indent (b:bs) str
  | (k == 1) = errorkind2 ks ls indent (b:bs) (str++[indenterror l (indent !! (l-1))])
  | (k == 2) = errorkind2 ks ls indent (b:bs) (str++[printferror l])
  | (k == 3) = errorkind2 ks ls indent (b:bs) (str++[scanferror l])
  | (k == 4) = errorkind2 ks ls indent (b:bs) (str++[iferror l])
  | (k == 5) = errorkind2 ks ls indent (b:bs) (str++[duplierror l])
  | (k == 6) = errorkind2 ks ls indent (b:bs) (str++[funcerror l])
  | (k == 7) = errorkind0 ks ls indent bs (str++[forerror l b])
  | otherwise = str
errorkind2 _ _ _ _ str = str

errorkind0 :: [Int] -> [Int] -> [Int] -> [Int] -> [String] -> [String]
errorkind0 (k:ks) (l:ls) indent bs str
  | (bs == []) = errorkind (k:ks) (l:ls) indent str
  | otherwise = errorkind2 (k:ks) (l:ls) indent bs str
errorkind0 _ _ _ _ str = str

-- readFileBasedOnNo :: String -> IO String
-- readFileBasedOnNo no = case no of
--     "1" -> readFile "question1.txt"
--     "2" -> readFile "question2.txt"
--     "3" -> readFile "question3.txt"
--     _ -> return "No file found"

-- function :: String -> [Int]
-- function no =
--     if no == "1"
--         then let (strs2, lists2, indent2, boo2, kind2, div2, defs02, defs2, nihon2, kindfun2, booli2, namecount2, countstm2, ind2) = MyLib.search string1
--                in countstm2
--     else if no == "2"
--         then let (strs2, lists2, indent2, boo2, kind2, div2, defs02, defs2, nihon2, kindfun2, booli2, namecount2, countstm2, ind2) = MyLib.search string2
--                in countstm2
--     else
--         let (strs2, lists2, indent2, boo2, kind2, div2, defs02, defs2, nihon2, kindfun2, booli2, namecount2, countstm2, ind2) = MyLib.search string3
--           in countstm2

countfunc :: Int -> Int -> String -> String
countfunc x y str
  | x - y > 0 = str ++ "多い<br>"
  -- | y - x > 0 = str ++ "少ない<br>"
countfunc _ _ _ = ""

countstmfunc :: Int -> [Int] -> [Int] -> [String] -> [String]
countstmfunc no (x:xs) (y:ys) list
  | no == 1 = countstmfunc (no+1) xs ys (list++[countfunc x y "Ifが"])
  | no == 2 = countstmfunc (no+1) xs ys (list++[countfunc x y "If-Elseが"])
  | no == 3 = countstmfunc (no+1) xs ys (list++[countfunc x y "Switchが"])
  | no == 4 = countstmfunc (no+1) xs ys (list++[countfunc x y "Whileが"])
  | no == 5 = countstmfunc (no+1) xs ys (list++[countfunc x y "Forが"])
  | no == 6 = countstmfunc (no+1) xs ys (list++[countfunc x y "Dowhileが"])
  | no == 7 = countstmfunc (no+1) xs ys (list++[countfunc x y "Printfが"])
  | no == 8 = countstmfunc (no+1) xs ys (list++[countfunc x y "Variableが"])
  | no == 9 = countstmfunc (no+1) xs ys (list++[countfunc x y "Function(int)が"])
  | no == 10 = countstmfunc (no+1) xs ys (list++[countfunc x y "Function(void)が"])
countstmfunc _ _ _ list = list

removeComments :: String -> String
removeComments input = removeMultiLineComments $ removeSingleLineComments input

removeSingleLineComments :: String -> String
removeSingleLineComments = Data.List.unlines . Data.List.map removeComment . Data.List.lines
  where
    removeComment line = case line =~ ("//" :: String) :: (String, String, String) of
        (before, _, _) -> before

removeMultiLineComments :: String -> String
removeMultiLineComments input = removeComments' input False
  where
    removeComments' :: String -> Bool -> String
    removeComments' "" _ = ""
    removeComments' str inComment =
        let (before, comment, after) = str =~ ("/\\*|\\*/" :: String) :: (String, String, String)
        in case comment of
            "/*" -> before ++ removeComments' after True
            "*/" -> removeComments' after False
            _    -> if inComment
                    then removeComments' after True
                    else before ++ removeComments' after False

countValidLinesAndChars :: String -> (Int, Int)
countValidLinesAndChars input =
    let validLines = Data.List.filter (not . Data.List.all isSpace) . Data.List.lines $ input
        validChars = Data.List.foldl' (\acc line -> acc + Data.List.length (Data.List.filter (not . isSpace) line)) 0 validLines
    in (Data.List.length validLines, validChars)

diffcount :: String -> Int -> Int -> String
diffcount str x y
  | x > y = str ++ "多い"
  -- | x < y = str ++ "少ない"
  | otherwise = ""
