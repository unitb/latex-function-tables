{-# LANGUAGE OverloadedStrings,QuasiQuotes
        ,TemplateHaskell
        ,FlexibleInstances
        ,UndecidableInstances
        ,MultiParamTypeClasses
        ,FunctionalDependencies
        ,GeneralizedNewtypeDeriving #-}
module Text.LaTeX.FunctionTable 
    ( makeTable 
    , LaTeXLI (..)
    , FunctionTable 
    , FunctionTable' (..)
    , TableCells
    , TableCells' (..)
    , branch, cell, cellH
    , subtables
    , isubtables
    , fromLaTeX
    , tex 
    )
where

import Text.LaTeX.Internal.FunctionTable
