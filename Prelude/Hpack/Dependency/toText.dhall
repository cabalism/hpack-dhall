let Dep = ./Type.dhall

let Dep/toText
    : ∀(x : Dep) → Text
    = λ(x : Dep) → "${x.name} >= ${x.lower} && < ${x.upper}"

in Dep/toText
