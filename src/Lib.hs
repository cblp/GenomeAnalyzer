{-# LANGUAGE LambdaCase #-}

module Lib
    ( ordP
    ) where

import           Data.Char                 (ord)
import           Language.Haskell.TH       (integerL, litP)
import           Language.Haskell.TH.Quote (QuasiQuoter (..))

-- Integer pattern from 'ord' of the given character
ordP :: QuasiQuoter -- String -> PatQ
ordP = QuasiQuoter
    { quotePat = \case
          [c] -> litP . integerL . toInteger $ ord c
          s   -> error $ "bad Char " ++ show s
    , quoteExp  = error "ordP is not an expression quasiquoter"
    , quoteType = error "ordP is not a type quasiquoter"
    , quoteDec  = error "ordP is not a declaraion quasiquoter"
    }
