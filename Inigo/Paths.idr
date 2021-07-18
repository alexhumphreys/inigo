module Inigo.Paths

import public System.Path

public export
data InigoPackagePath
  = TomlPath String
  | DhallPath String

export
inigoTomlPath : InigoPackagePath
inigoTomlPath = TomlPath "Inigo.toml"

export
inigoDhallPath : InigoPackagePath
inigoDhallPath = DhallPath "Inigo.dhall"

export
inigoIPkgPath : String
inigoIPkgPath = "Inigo.ipkg"

export
inigoWorkDir : String
inigoWorkDir = ".inigo-work"

export
inigoDepDir : String
inigoDepDir = inigoWorkDir </> "deps"

export
inigoDepPkgCache : String
inigoDepPkgCache = inigoWorkDir </> "deps.cache"

export
DEBUG : Bool
DEBUG = True
