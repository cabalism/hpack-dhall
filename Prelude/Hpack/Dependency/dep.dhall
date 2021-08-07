let Dep = ./Type.dhall

let Dep/dep : forall (x : Text) -> forall (l : Text) -> forall (u : Text) -> Dep =  \(x : Text) -> \(l : Text) -> \(u : Text) -> { name = x, lower = l, upper = u }

in Dep/dep
