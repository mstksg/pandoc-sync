{-# LANGUAGE TemplateHaskell #-}

module Text.Pandoc.Lens.App (
    AsHTMLMathMethod(..)
  ) where

import           Text.Pandoc
import           Control.Lens

makeClassyPrisms ''HTMLMathMethod
