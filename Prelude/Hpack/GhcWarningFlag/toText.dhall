let GhcWarningFlag = ./Type.dhall

let GhcWarning = ../GhcWarning/Type.dhall

let GhcWarning/toText = ../GhcWarning/toText.dhall

let GhcWarningFlag/toText
    : ∀(x : GhcWarningFlag) → Text
    = λ(x : GhcWarningFlag) →
        merge
          { Yes = λ(x : GhcWarning) → "-W${GhcWarning/toText x}"
          , No = λ(x : GhcWarning) → "-Wno-${GhcWarning/toText x}"
          }
          x

in  GhcWarningFlag/toText
