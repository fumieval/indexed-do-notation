{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.IndexedDo
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Language.Haskell.IndexedDo (ido) where

import Control.Monad.Indexed

import Language.Haskell.Meta
import Language.Haskell.TH
import Language.Haskell.TH.Quote

ido :: QuasiQuoter
ido = QuasiQuoter { quoteExp = \str -> case parseExp str of
        Right (DoE ss) -> return (go ss)
        Right _ -> fail "Expecting do notation"
        Left err -> fail (show err)
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        } where
    go [NoBindS e] = e
    go (BindS p e : ss) = VarE 'ibind `AppE` LamE [p] (go ss) `AppE` e
    go (NoBindS e : ss) = VarE 'ibind `AppE` LamE [WildP] (go ss) `AppE` e
    go (LetS ds : ss) = LetE ds (go ss)