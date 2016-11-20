{-# LANGUAGE OverloadedStrings,QuasiQuotes
        ,TemplateHaskell
        ,FlexibleInstances
        ,UndecidableInstances
        ,MultiParamTypeClasses
        ,FunctionalDependencies
        ,GeneralizedNewtypeDeriving #-}
module Text.LaTeX.FunctionTable 
    ( makeTable 
    , FunctionTable (..)
    , TableCells (..)
    , branch, cell )
where

import Text.LaTeX.Internal.FunctionTable
