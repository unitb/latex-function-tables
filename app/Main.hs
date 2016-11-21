{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Main where

import Text.LaTeX
import Text.LaTeX.FunctionTable
import System.Process

short :: LaTeX
short = 
    documentclass [] article
 <> usepackage [] "multirow"
 <> title "A short message"
 <> author "John Short"
 <> document (mconcat 
        [ maketitle 
        , myTable 
        , newline <> newline
        , rendertex myFT
        , newline <> newline
        , "This is a paragraph"
        , newline <> newline
        , rendertex myFT' ])

myTable :: LaTeX
myTable = tabular Nothing [LeftColumn,RightColumn] $ execLaTeXM $ do
    textell $ "true" & "false"
    textell $ lnbk
    textell $ "false" & "true"
    textell $ lnbk
    textell $ "false" & "false"

myFT :: FunctionTable LaTeXLI
myFT = makeTable           "Letter" $ do
        cell "a"           "A"
        branch "b" $ do
            cell "c"       "C"
            cell "\\neg c" "B"
    -- Condition (Cols 1) $ ("a", Cell "A") :| [("b", Condition (Cols 1) $ ("c",Cell "C") :| [("not c",Cell "B")])]

myFT' :: FunctionTable LaTeXLI
myFT' = makeTable               "x" $ do
        cell "C0"               "x_0"
        branch "\\neg C0" $ do
            branch "C1" $ do
                cell "C2"       "x_2"
                cell "\\neg C2" "x_3"
            branch "\\neg C1" $ do
                cell "C2"       "x_4"
                cell "\\neg C2" "x_5"

main :: IO ()
main = do
    renderFile "table.tex" short
    rawSystem "pdflatex" ["table.tex"]
    return ()

-- main = $(runIO (renderFile "table.tex" short >> rawSystem "pdflatex" ["table.tex"]) >> [| return () |])
-- main = renderFile "short.tex" short
