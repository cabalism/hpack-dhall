let Dependency = ./Type.dhall

let VersionRange = ../VersionRange/Type.dhall

let Dependency/dep
    : ∀(x : Text) → ∀(l : Text) → ∀(u : Text) → Dependency
    = λ(x : Text) →
      λ(l : Text) →
        \(u : Text) → { name = x, range = { lower = l, upper = u } }

in  Dependency/dep
