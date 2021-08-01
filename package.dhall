{ depends = [] : List Text
, deps = [{ package = { ns = "Base" , name = "IdrTest" }, requirement = "0.0.1" }]
, description = Some "a random test package"
, devDeps = [{ package = { ns = "Other" , name = "SomeDebug" }, requirement = "1.0.1" }]
, executable = Some "MyPkg"
, license = Some "stuff"
}

