{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_assignment2_mmorpgaddicts (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "assignment2_mmorpgaddicts"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = ""
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/sunwaydcis/assignment-2-mmorpgaddicts.git"
