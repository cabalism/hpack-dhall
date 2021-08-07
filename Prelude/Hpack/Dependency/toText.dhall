let Dependency = ./Type.dhall

let VersionRange/toText = ../VersionRange/toText.dhall

let Dependency/toText
    : ∀(x : Dependency) → Text
    = λ(x : Dependency) → "${x.name} ${VersionRange/toText x.range}"

in  Dependency/toText
