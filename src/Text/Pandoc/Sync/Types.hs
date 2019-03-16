{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Sync.Types (
    GlobalOptions(..)
  , Mode(..)
  , Reference(..)
  , BranchOptions(..)
  , Branch(..)
  , Config(..)
  ) where

import           Data.Map                (Map)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           Numeric.Natural
import           Text.Pandoc.Sync.Format
import qualified Data.Map                as M
import qualified Dhall                   as D

data GlobalOptions = GlobalOptions
    { goAlwaysBackup :: Bool
    , goVariables    :: Map Text Text
    , goDiscover     :: Bool
    }
  deriving (Show, Eq, Ord, Generic)

data Mode = MReadWrite
          | MWriteOnly
          | MReadOnly
  deriving (Show, Eq, Ord, Generic)

data Reference = RSelf | RDefault | RFile FilePath
  deriving (Show, Eq, Ord, Generic)

data BranchOptions = BranchOptions
    { boAlwaysBackup :: Maybe Bool
    , boVariables    :: Map Text Text
    , boDiscover     :: Maybe Bool
    , boMode         :: Maybe Mode
    , boPriority     :: Natural
    , boReference    :: Reference
    }
  deriving (Show, Eq, Ord, Generic)

data Branch = Branch
    { brOptions :: BranchOptions
    , brDir     :: FilePath
    , brFormats :: [SomeFormat]
    }
  deriving (Show, Eq, Ord, Generic)
      
data Config = Config { cfgOptions :: GlobalOptions
                     , cfgBanches :: [Branch]
                     }
  deriving (Show, Eq, Ord, Generic)

decodeVariables :: D.Type (Map Text Text)
decodeVariables = fmap M.fromList . D.list . D.record $
    (,) <$> D.field "key" D.auto
        <*> D.field "val" D.auto

instance D.Interpret GlobalOptions where
    autoWith _ = D.record $
      GlobalOptions <$> D.field "always-backup" D.auto
                    <*> D.field "variables" decodeVariables
                    <*> D.field "discover" D.auto

instance D.Interpret Mode where
    autoWith _ = D.union . mconcat $
      [ MReadWrite <$ D.constructor "ReadWrite" D.unit
      , MWriteOnly <$ D.constructor "WriteOnly" D.unit
      , MReadOnly  <$ D.constructor "ReadOnly" D.unit
      ]

instance D.Interpret Reference where
    autoWith _ = D.union . mconcat $
      [ RSelf    <$  D.constructor "Self" D.unit
      , RDefault <$  D.constructor "Default" D.unit
      , RFile    <$> D.constructor "File" D.auto
      ]

instance D.Interpret BranchOptions where
    autoWith _ = D.record $
      BranchOptions <$> D.field "always-backup" D.auto
                    <*> D.field "variables" decodeVariables
                    <*> D.field "discover" D.auto
                    <*> D.field "mode" D.auto
                    <*> D.field "priority" D.auto
                    <*> D.field "reference" D.auto

instance D.Interpret Branch where
    autoWith _ = D.record $
        Branch <$> D.field "options" D.auto
               <*> D.field "dir"     D.auto
               <*> D.field "formats" D.auto

instance D.Interpret Config where
    autoWith _ = D.record $
        Config <$> D.field "options" D.auto
               <*> D.field "branches" D.auto

