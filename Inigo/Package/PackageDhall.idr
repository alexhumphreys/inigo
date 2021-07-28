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

record DepPackage where
  constructor MkDepPackage
  package : PackageInfo
  requirement : String
%runElab (deriveFromDhall Record `{ DepPackage })

public export
record PackageDhall where
  constructor MkPackageDhall
  ns : String
  package : String
  version : String
  description : Maybe String
  link : Maybe String
  readme : Maybe String
  modules : List String
  depends : List String
  license : Maybe String
  sourcedir : String
  main : Maybe String
  executable : Maybe String
  deps : List (DepPackage)
  devDeps : List (DepPackage)
  -- extraDeps : List ExtraDep TODO

%runElab (deriveFromDhall Record `{ PackageDhall })

parsePackageDhall' : String -> IO $ Either String PackageDhall
parsePackageDhall' path = do
  putStrLn $ show $ parseExpr "True"
  putStrLn "True"
  Right package <- liftIOEither $ deriveFromDhallString {ty=PackageDhall} "True"
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
inigoPackageFromDhall (MkPackageDhall ns package version description link readme modules depends license sourcedir main executable deps devDeps) =
  let packageVersion = !(versionFromDhall version)
      packageDeps = !(traverse depFromDhall deps)
      packageDevDeps = !(traverse depFromDhall devDeps) in
  pure $ MkPackage ns package packageVersion description link readme modules depends license sourcedir main executable packageDeps packageDevDeps [] -- TODO add extra deps

export
parsePackageDhall : String -> Promise $ Either String Package
parsePackageDhall x = do
  Right package <- liftIO $ parsePackageDhall' x | Left err => pure $ Left err
  pure $ inigoPackageFromDhall package
