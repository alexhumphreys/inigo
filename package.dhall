{ depends = [] : List Text
, deps = [{ ns = "Base" , name = "IdrTest", requirement = "0.0.1" }]
, description = Some "a random test package"
, devDeps = [{ ns = "Other" , name = "SomeDebug", requirement = "1.0.1" }]
, executable = Some "MyPkg"
, license = Some "stuff"
, link = Some "www.example.com"
, main = None Text
, modules = [] : List Text
, ns = "Alexhumphreys"
, package = "MyPkg"
, readme = Some "./README.md"
, sourcedir = "."
, version = "9.0.0"
, localDeps = ["./someDir"] : List Text
{-
, extraDeps =
  [ { download = < SubDir | Git : Text >.Git "abc2345"
    , url = "git@github.com/mydep/foo"
    , subDirs = ["."]
    }
  ]
  -}
}

