{-# LANGUAGE OverloadedStrings,QuasiQuotes
        ,TemplateHaskell
        ,FlexibleContexts
        ,FlexibleInstances
        ,UndecidableInstances
        ,MultiParamTypeClasses
        ,FunctionalDependencies
        ,GeneralizedNewtypeDeriving #-}
module Text.LaTeX.Internal.FunctionTable where


import Control.Lens hiding ((&))
import Control.Monad.Writer

import Data.List as L
import Data.List.NonEmpty as N hiding (repeat)
import Data.Maybe
import Data.Semigroup hiding ((<>))
import Text.LaTeX
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class

data TableCells a = Cell a | Condition Cols (NonEmpty (LaTeX,TableCells a))
    deriving (Show,Eq,Functor,Traversable,Foldable)

data FunctionTable a = Table a (TableCells a)
    deriving (Show,Eq,Functor,Traversable,Foldable)

newtype Rows = Rows Int
    deriving (Show,Eq,Ord,Num)
newtype Cols = Cols Int
    deriving (Show,Eq,Ord,Num)

data Row a = Row [Either Filler Heading] a
    deriving (Show,Eq,Functor,Traversable,Foldable)

data Filler = Filler 
        { _fillerWidth :: Cols 
        , _fillerIsLast :: Bool
        }
    deriving (Show,Eq)

data Heading = Heading 
        { _title :: LaTeX 
        , _headingHeight :: Rows 
        , _headingWidth :: Cols 
        , _headingIsLast :: Bool
        }
    deriving (Show,Eq)

makePrisms ''TableCells
makeFields ''Heading
makeFields ''Filler

type M a = WriterT [(LaTeX,TableCells a)] Maybe

makeTable :: a -> M a () -> FunctionTable a
makeTable x t = Table x $ Condition (Cols 1) $ fromJust $ nonEmpty =<< execWriterT t

cell :: String -> String -> M LaTeX ()
cell l x = tell [(math $ TeXRaw $ fromString l,Cell $ math $ TeXRaw $ fromString x)]

branch :: String -> M a () -> M a ()
branch l t = do
    xs <- lift $ nonEmpty =<< execWriterT t
    tell [(math $ TeXRaw $ fromString l,Condition (Cols 1) xs)]

depth :: TableCells a -> Int
depth (Cell _) = 0
depth (Condition (Cols w) xs) = w + maximum (depth.snd <$> xs)

witdth :: TableCells a -> Int
witdth (Cell _) = 1
witdth (Condition _ xs) = sum (witdth.snd <$> xs)

columnSpecOf :: TableCells a -> [TableSpec]
columnSpecOf t = cols
    where
        d = depth t
        cols = VerticalLine : L.intersperse VerticalLine (replicate d LeftColumn) 
                ++ [VerticalLine,VerticalLine,CenterColumn,VerticalLine,VerticalLine]

rowToLatex :: Render a => Cols -> Row a -> LaTeX
rowToLatex (Cols size) (Row hs x) = 
                           foldr (&) (rendertex x) (L.map oneHeading hs) 
                        <> lnbk <> cline (n+1) size
    where
        colSpec = [VerticalLine,LeftColumn,VerticalLine]
        oneHeading (Left (Filler (Cols c) _)) 
                = multicolumn c colSpec ""
        oneHeading (Right (Heading h (Rows r) (Cols c) isLast))
                | isLast    = multicolumn (c + (size - total) - 1) (colSpec ++ [VerticalLine]) (multirow r h)
                | otherwise = multicolumn c colSpec (multirow r h)
        canCut x = has (_Right.rows.filtered (<= 1)) x 
                    || has (_Left.isLast.filtered id) x 
        b = L.dropWhile canCut $ L.reverse hs
        Cols n = sum $ view width <$> b
        Cols total = sum $ view width <$> hs

instance (HasWidth a c,HasWidth b c) => HasWidth (Either a b) c where
    width f (Left x) = Left <$> width f x
    width f (Right x) = Right <$> width f x
instance HasWidth Cols Cols where
    width f x = f x

texFunctionTable :: Render a => FunctionTable a -> LaTeX
texFunctionTable (Table x t) = tabular Nothing (columnSpecOf t) $ 
        hline <> 
        rowToLatex sz (Row cond x) <>
        hline <> 
        hline <> 
        (foldMap (rowToLatex sz) . texFunctionTableRows) t <> 
        hline
    where
        cond | sz <= 1   = []
             | otherwise = [Right $ Heading "" 1 1 True]
        sz = Cols $ depth t + 1

-- \multirow{number rows}{width}{text}
multirow :: LaTeXC l => Int -> l -> l 
multirow n = liftL $ \l -> TeXComm "multirow"
      [ FixArg $ rendertex n
      , FixArg . TeXRaw $ "*"
      , FixArg l
      ]

rows :: Lens' Heading Rows
rows f (Heading l r c b) = (\r' -> Heading l r' c b) <$> f r

cols :: Lens' Heading Cols
cols f (Heading l r c b) = (\c' -> Heading l r c' b) <$> f c

condition :: Rows -> Cols -> LaTeX -> NonEmpty LaTeX
condition (Rows n) (Cols m) t = first :| rest 
    where
        first = multicolumn m [VerticalLine,CenterColumn,VerticalLine] (multirow n t)
        rest  = replicate (n-1) (multicolumn m [VerticalLine,LeftColumn,VerticalLine] "")

makeLast :: Heading -> Heading
makeLast (Heading a b c _) = Heading a b c True

addHeading :: Heading -> Row a -> Row a
addHeading h (Row [] x) = Row [Right $ makeLast h] x
addHeading h (Row hs x) = Row (Right h:hs) x

addMargin :: Cols -> Row a -> Row a
addMargin n (Row hs x) = Row (Left (Filler n False):hs) x

addLastMargin :: Cols -> Row a -> Row a
addLastMargin n (Row hs x) = Row (Left (Filler n True):hs) x

mapNonEmpty :: (a -> b)
            -> ([a] -> [b])
            -> NonEmpty a
            -> NonEmpty b
mapNonEmpty f g (x :| xs) = f x :| g xs

texFunctionTableRows :: Render a 
                     => TableCells a 
                     -> NonEmpty (Row a)
texFunctionTableRows (Cell x) = pure $ Row [] x
texFunctionTableRows (Condition w ts) 
        = sconcat $ uncurry heading <$> ts
    where
        heading h t = mapNonEmpty 
                (addHeading h') 
                (_Snoc %~ bimap (L.map $ addMargin w) (addLastMargin w)) 
                r
            where
                h' = Heading h (Rows n) w False
                r = texFunctionTableRows t
                n = N.length r

instance Render a => Render (FunctionTable a) where
    render = render . texFunctionTable
