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
  = Git_ String
  | SubDir_
%runElab (deriveFromDhall ADT `{ Download' })

downloadDhallType : String
downloadDhallType = "< SubDir_ | Git_ Text >"

Show Download' where
  show (Git_ x) = "\{downloadDhallType}.Git_ \{show x}"
  show SubDir_ = "\{downloadDhallType}.SubDir"

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

record PackageInfo where
  constructor MkPackageInfo
  ns : String
  name : String
%runElab (deriveFromDhall Record `{ PackageInfo })

Show PackageInfo where
  show (MkPackageInfo ns name) = "{ ns = \{show ns}, name = \{show name} }"

record DepPackage where
  constructor MkDepPackage
  package : PackageInfo
  requirement : String
%runElab (deriveFromDhall Record `{ DepPackage })

Show DepPackage where
  show (MkDepPackage package requirement) = "{ package = \{show package}, requirement = \{show requirement} }"

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

{-
public export
record PackageDhall where
  constructor MkPackageDhall
  depends : List String
  -- deps : List (DepPackage)
  description : Maybe String
  -- devDeps : List (DepPackage)
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
  -- extraDeps : List ExtraDep TODO

%runElab (deriveFromDhall Record `{ PackageDhall })
-}

doitd : String -> IO String
doitd x = do
  x <- liftIOEither $ deriveFromDhallString {ty=Download'} x
  putStrLn $ show x
  pure $ show x

doit : String -> IO String
doit x = do
  x <- liftIOEither $ deriveFromDhallString {ty=PackageDhall'} x
  putStrLn $ show x
  pure $ show x

parsePackageDhall' : String -> IO $ Either String PackageDhall'
parsePackageDhall' path = do
  putStrLn $ show $ parseExpr path
  Right package <- liftIOEither $ deriveFromDhallString {ty=PackageDhall'} path
    | Left err => do
        putStrLn "OTHER"
        pure $ Left $ show err
  putStrLn $ show package
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
extradepFromDhall (MkExtraDep' (Git_ x) url subDirs) =
  MkExtraDep Git x url subDirs
extradepFromDhall (MkExtraDep' SubDir_ url subDirs) =
  MkExtraDep SubDir () url subDirs

depFromDhall : DepPackage -> Either String (List String, Requirement)
depFromDhall (MkDepPackage (MkPackageInfo ns name) requirement) =
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

{-
export
parsePackageDhall : String -> Promise $ Either String Package
parsePackageDhall x = do
  Right package <- liftIO $ parsePackageDhall' x | Left err => pure $ Left err
  pure $ inigoPackageFromDhall package
  -}
