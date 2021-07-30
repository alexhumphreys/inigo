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
%runElab (deriveFromDhall Record `{ PackageDhall' })

shortDepPackage : String
shortDepPackage = "{ package : { name : Text, ns : Text }, requirement : Text }"

Show PackageDhall' where
  show (MkPackageDhall' depends deps) =
    """
    { depends = \{show depends} : List Text
    , deps = \{show deps} : List \{ shortDepPackage }
    }
    """

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

doit : String -> IO String
doit x = do
  x <- liftIOEither $ deriveFromDhallString {ty=PackageDhall'} x
  putStrLn $ show x
  pure $ show x

parsePackageDhall' : String -> IO $ Either String PackageDhall
parsePackageDhall' path = do
  putStrLn $ show $ parseExpr path
  Right package' <- liftIOEither $ deriveFromDhallString {ty=PackageDhall'} "./package.dhall"
    | Left err => do
        putStrLn "OTHER"
        pure $ Left $ show err
  putStrLn $ show package'
  Right package <- liftIOEither $ deriveFromDhallString {ty=PackageDhall} path
    | Left err => do
        putStrLn "HERE"
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

depFromDhall : DepPackage -> Either String (List String, Requirement)
depFromDhall (MkDepPackage (MkPackageInfo ns name) requirement) =
  pure ([ns, name], !(requirementFromDhall requirement))

inigoPackageFromDhall : PackageDhall -> Either String Package
-- inigoPackageFromDhall (MkPackageDhall ns package version description link readme modules depends license sourcedir main executable deps devDeps) =
inigoPackageFromDhall (MkPackageDhall depends description executable link license main modules ns package readme sourcedir version) =
  let packageVersion = !(versionFromDhall version)
      -- packageDeps = !(traverse depFromDhall deps)
      -- packageDevDeps = !(traverse depFromDhall devDeps)
      in
      pure $ MkPackage ns package packageVersion description link readme modules depends license sourcedir main executable [] [] [] -- TODO add extra deps

export
parsePackageDhall : String -> Promise $ Either String Package
parsePackageDhall x = do
  Right package <- liftIO $ parsePackageDhall' x | Left err => pure $ Left err
  pure $ inigoPackageFromDhall package
