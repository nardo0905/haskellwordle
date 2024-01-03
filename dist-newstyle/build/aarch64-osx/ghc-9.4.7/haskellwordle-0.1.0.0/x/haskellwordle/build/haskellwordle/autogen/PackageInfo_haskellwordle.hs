{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskellwordle (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskellwordle"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A Wordle-style game written in Haskel. Supports two game modes - \"guess\" and \"help\""
copyright :: String
copyright = ""
homepage :: String
homepage = ""
