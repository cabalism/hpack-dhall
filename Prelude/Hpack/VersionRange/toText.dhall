let VersionRange = ./Type.dhall

let VersionRange/toText
    : ∀(x : VersionRange) → Text
    = λ(x : VersionRange) → ">= ${x.lower} && < ${x.upper}"

in  VersionRange/toText
