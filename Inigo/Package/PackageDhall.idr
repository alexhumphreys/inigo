module Inigo.Package.PackageDhall

import Data.List
import Data.Maybe
import Extra.Either
import Extra.List
import Extra.String
import Fmt
import public Inigo.Package.ExtraDep
import Inigo.Package.Package
import Inigo.Package.ParseHelpers
import Inigo.Paths
import Inigo.PkgTree
import Inigo.Async.Promise
import SemVar
import System.Path
import Toml

import Idrall.API.V2
import Idrall.Parser
import Idrall.APIv1

import Language.Reflection
%language ElabReflection

data Download'
  = Git String
  | SubDir
%runElab (deriveFromDhall ADT `{ Download' })

downloadDhallType : String
downloadDhallType = "< SubDir | Git Text >"

Show Download' where
  show (Git x) = "\{downloadDhallType}.Git \{show x}"
  show SubDir = "\{downloadDhallType}.SubDir"

record ExtraDep' where
  constructor MkExtraDep'
  download : Download'
  url : String
  subDirs : List String
%runElab (deriveFromDhall Record `{ ExtraDep' })

Show ExtraDep' where
  show (MkExtraDep' {download, url, subDirs}) =
    """
    { download = \{show download}
    , url = \{show url}
    , subDirs = \{show subDirs}
    }
    """

record DepPackage where
  constructor MkDepPackage
  ns : String
  name : String
  requirement : String
%runElab (deriveFromDhall Record `{ DepPackage })

Show DepPackage where
  show (MkDepPackage ns name requirement) = "{ ns = \{show ns}, name = \{show name}, requirement = \{show requirement} }"

public export
record PackageDhall' where
  constructor MkPackageDhall'
  depends : List String
  deps : List (DepPackage)
  description : Maybe String
  devDeps : List (DepPackage)
  executable : Maybe String
  license : Maybe String
  link : Maybe String
  main : Maybe String
  modules : List String
  ns : String
  package : String
  readme : Maybe String
  sourcedir : String
  version : String
  extraDeps : List ExtraDep'
%runElab (deriveFromDhall Record `{ PackageDhall' })

shortDepPackage : String
shortDepPackage = "{ package : { name : Text, ns : Text }, requirement : Text }"

Show PackageDhall' where
  show (MkPackageDhall' {depends, deps, description, devDeps, executable, license, link, main, modules, ns, package, readme, sourcedir, version, extraDeps}) =
    """
    { depends = \{show depends} : List Text
    , deps = \{show deps} : List \{ shortDepPackage }
    , description = \{show description}
    , devDeps = \{show devDeps}
    , executable = \{show executable}
    , license = \{show license}
    , link = \{show link}
    , main = \{show main}
    , modules = \{show modules}
    , ns = \{show ns}
    , package = \{show package}
    , readme = \{show readme}
    , sourcedir = \{show sourcedir}
    , version = \{show version}
    , extraDeps =
      \{show extraDeps}
    }
    """

parsePackageDhall' : String -> IO $ Either String PackageDhall'
parsePackageDhall' path = do
  Right package <- liftIOEither $ deriveFromDhallString {ty=PackageDhall'} path
    | Left err => do
        pure $ Left $ show err
  pure $ Right package

requirementFromDhall : String -> Either String Requirement
requirementFromDhall x =
  case parseRequirement x of
       Nothing => Left "Error parsing Requirement"
       (Just r) => pure r

versionFromDhall : String -> Either String Version
versionFromDhall x =
  case parseVersion x of
       Nothing => Left "Error parsing Version"
       (Just v) => pure v

extradepFromDhall : ExtraDep' -> ExtraDep
extradepFromDhall (MkExtraDep' (Git x) url subDirs) =
  MkExtraDep Git x url subDirs
extradepFromDhall (MkExtraDep' SubDir url subDirs) =
  MkExtraDep SubDir () url subDirs

depFromDhall : DepPackage -> Either String (List String, Requirement)
depFromDhall (MkDepPackage ns name requirement) =
  pure ([ns, name], !(requirementFromDhall requirement))

inigoPackageFromDhall : PackageDhall' -> Either String Package
inigoPackageFromDhall (MkPackageDhall' {depends, deps, description, devDeps, executable, license, link, main, modules, ns, package, readme, sourcedir, version, extraDeps}) =
  let packageVersion = !(versionFromDhall version)
      packageDeps = !(traverse depFromDhall deps)
      packageDevDeps = !(traverse depFromDhall devDeps)
      packageExtraDeps = map extradepFromDhall extraDeps
  in
  pure $ MkPackage
    { ns=ns
    , package=package
    , version=packageVersion
    , description=description
    , link=link
    , readme=readme
    , modules=modules
    , depends=depends
    , license=license
    , sourcedir=sourcedir
    , main=main
    , executable=executable
    , deps=packageDeps
    , devDeps=packageDevDeps
    , extraDeps=packageExtraDeps
    }

export
parsePackageDhall : String -> Promise $ Either String Package
parsePackageDhall x = do
  Right package <- liftIO $ parsePackageDhall' x | Left err => pure $ Left err
  pure $ inigoPackageFromDhall package

doitp : String -> IO ()
doitp x = do
    Right package <- liftIO $ parsePackageDhall' x
      | Left err => putStrLn $ show err
    let p = inigoPackageFromDhall package
    putStrLn $ show p
