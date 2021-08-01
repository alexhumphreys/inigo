{ depends = [] : List Text
, deps = [{ package = { ns = "Base" , name = "IdrTest" }, requirement = "0.0.1" }]
, description = Some "a random test package"
, devDeps = [{ package = { ns = "Other" , name = "SomeDebug" }, requirement = "1.0.1" }]
, executable = Some "MyPkg"
, license = Some "stuff"
, link = Some "www.example.com"
, main = None
, modules = [] : List Text
, ns = "Alexhumphreys"
, package = "MyPkg"
, readme = Some "./README.md"
, sourcedir = "."
, version = "9.0.0"
, extraDeps =
  [ { download = < SubDir_ | Git_ : Text >.Git_ "abc2345"
    , url = "git@github.com/mydep/foo"
    , subDirs = ["."]
    }
  ]
}

