{-# LANGUAGE OverloadedStrings,QuasiQuotes
        ,ImplicitParams
        ,CPP
        ,TemplateHaskell
        ,ConstraintKinds
        ,FlexibleContexts
        ,FlexibleInstances
        ,StandaloneDeriving
        ,UndecidableInstances
        ,MultiParamTypeClasses
        ,FunctionalDependencies
        ,GeneralizedNewtypeDeriving #-}
module Text.LaTeX.Internal.FunctionTable where


import Control.Applicative
import Control.Lens hiding ((&))
import Control.Monad.Writer

import Data.Bifoldable
import Data.Bitraversable
import Data.List as L
import Data.List.NonEmpty as N hiding (repeat)
import Data.Maybe
import Data.Semigroup hiding ((<>))
import Data.Text

#if MIN_VERSION_base(4,9,0)
import GHC.Stack
#else
import GHC.Stack
import GHC.SrcLoc
#endif
import           Language.Haskell.TH.Quote 
import qualified Language.Haskell.TH.Syntax as TH

import Text.LaTeX
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class  hiding (fromLaTeX)

data LaTeXLI = LaTeXLI SrcLoc String 
    deriving (Show,Eq)

tex :: QuasiQuoter 
tex = QuasiQuoter
        { quoteExp  = \str -> [e| raw $(TH.lift str) |]
        , quotePat  = undefined
        , quoteType = undefined 
        , quoteDec  = undefined }

data TableCells' a b = Cell b | Condition Cols (NonEmpty (a,TableCells' a b))
    deriving (Show,Eq,Functor,Foldable,Traversable)

data FunctionTable' a b = Table b (TableCells' a b)
    deriving (Show,Eq,Functor,Traversable,Foldable)

type TableCells a = TableCells' a a
type FunctionTable a = FunctionTable' a a

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

makePrisms ''TableCells'
makeFields ''Heading
makeFields ''Filler

instance Bifunctor FunctionTable' where
    bimap = bimapDefault
instance Bifunctor TableCells' where
    bimap = bimapDefault
instance Bifoldable FunctionTable' where
    bifoldMap = bifoldMapDefault
instance Bifoldable TableCells' where
    bifoldMap = bifoldMapDefault
instance Bitraversable FunctionTable' where
    bitraverse f g (Table h t) = liftA2 Table (g h) (bitraverse f g t)
instance Bitraversable TableCells' where
    bitraverse _ g (Cell x) = Cell <$> g x
    bitraverse f g (Condition w ts) = Condition w <$> traverse (bitraverse f $ bitraverse f g) ts

type Pre = (?loc :: CallStack)

subtables :: Traversal' (TableCells' a b) (a,TableCells' a b)
subtables _f x@(Cell _) = pure x
subtables f (Condition k xs) = Condition k <$> traverse f xs

isubtables :: IndexedTraversal' Int (TableCells' a b) (a,TableCells' a b)
isubtables _f x@(Cell _) = pure x
isubtables f (Condition k xs) = Condition k <$> itraverse (indexed f) xs

type M a = WriterT [(a,TableCells a)] Maybe

fromLaTeX :: Pre => LaTeX -> LaTeXLI
fromLaTeX = LaTeXLI (snd $ L.last $ getCallStack ?loc) . unpack . render

makeLaTeXLI :: Pre => String -> LaTeXLI
makeLaTeXLI = LaTeXLI (snd $ L.last $ getCallStack ?loc)

makeTable :: Pre
          => LaTeX 
          -> M LaTeXLI () 
          -> FunctionTable LaTeXLI
makeTable x t = Table (fromLaTeX x)
            $ Condition (Cols 1) 
            $ fromJust $ nonEmpty =<< execWriterT t

cell :: Pre => LaTeX -> LaTeX -> M LaTeXLI ()
-- cell l x = tell [(math $ TeXRaw $ fromString l,Cell $ math $ TeXRaw $ fromString x)]
cell l x = tell [(fromLaTeX l, Cell $ fromLaTeX x)]

branch :: Pre => LaTeX -> M LaTeXLI () -> M LaTeXLI ()
branch l t = do 
    xs <- lift $ nonEmpty =<< execWriterT t
    tell [(fromLaTeX l,Condition (Cols 1) xs)]

depth :: TableCells' a b -> Int
depth (Cell _) = 0
depth (Condition (Cols w) xs) = w + L.maximum (depth.snd <$> xs)

witdth :: TableCells' a b -> Int
witdth (Cell _) = 1
witdth (Condition _ xs) = sum (witdth.snd <$> xs)

columnSpecOf :: TableCells' a b -> [TableSpec]
columnSpecOf t = cols
    where
        d = depth t
        cols = VerticalLine : L.intersperse VerticalLine (L.replicate d LeftColumn) 
                ++ [VerticalLine,VerticalLine,CenterColumn,VerticalLine,VerticalLine]

rowToLatex :: Render a => Cols -> Row a -> LaTeX
rowToLatex (Cols size) (Row hs x) = 
                           L.foldr (&) (rendertex x) (L.map oneHeading hs) 
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
        Cols total = sum $ view width <$> hs

instance (HasWidth a c,HasWidth b c) => HasWidth (Either a b) c where
    width f (Left x) = Left <$> width f x
    width f (Right x) = Right <$> width f x
instance HasWidth Cols Cols where
    width f x = f x

texFunctionTable :: (Render a,Render b) 
                 => FunctionTable' a b 
                 -> LaTeX
texFunctionTable (Table x t) = tabular Nothing (columnSpecOf t) $ 
        hline <> 
        rowToLatex sz (Row cond x) <>
        hline <> 
        hline <> 
        (foldMap (rowToLatex sz) . texFunctionTableRows) t
    where
        cond | sz <= 1   = []
             | otherwise = [Right $ Heading "" 1 1 True]
        sz = Cols $ depth t + 1

-- \multirow{number rows}{width}{text}
multirow :: LaTeXC l => Int -> l -> l 
multirow n | n <= 1    = id
           | otherwise = liftL $ \l -> TeXComm "multirow"
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
        rest  = L.replicate (n-1) (multicolumn m [VerticalLine,LeftColumn,VerticalLine] "")

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
                     => TableCells' a b
                     -> NonEmpty (Row b)
texFunctionTableRows (Cell x) = pure $ Row [] x
texFunctionTableRows (Condition w ts) 
        = sconcat $ uncurry heading <$> ts
    where
        heading h t = mapNonEmpty 
                (addHeading h') 
                (_Snoc %~ bimap (L.map $ addMargin w) (addLastMargin w)) 
                r
            where
                h' = Heading (rendertex h) (Rows n) w False
                r = texFunctionTableRows t
                n = N.length r

instance Render LaTeXLI where
    render (LaTeXLI _ l) = render . math . TeXRaw . fromString $ l

instance (Render a,Render b) => Render (FunctionTable' a b) where
    render = render . texFunctionTable
