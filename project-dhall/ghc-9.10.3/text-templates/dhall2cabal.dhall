let TYPES = ./../../../updo/types.dhall

let empty-constraints = ./../../../updo/empty/constraints.dhall

let empty-source-pkgs = ./../../../updo/empty/source-pkgs.dhall

let null = https://prelude.dhall-lang.org/List/null

in  \(pkgs-done : List Text) ->
    \(stackage-resolver : Text) ->
      let pkgs-todo = ../../pkgs-upgrade-todo.dhall

      let pkg-config =
            { constraints = ./../constraints.dhall ? empty-constraints
            , source-pkgs =
              { deps-external = ./../deps-external.dhall ? empty-source-pkgs
              , deps-internal = ./../deps-internal.dhall ? empty-source-pkgs
              , forks-external = ./../forks-external.dhall ? empty-source-pkgs
              , forks-internal = ./../forks-internal.dhall ? empty-source-pkgs
              }
            }

      in  ''
          ${../../../updo/text-templates/dhall2cabal.dhall
              TYPES.Verbosity.Quiet
              TYPES.Stackage.StackageLocal
              stackage-resolver
              ( if    null Text pkgs-todo
                then  TYPES.PkgSet.AllPkgs pkgs-done
                else  TYPES.PkgSet.PkgUpgrade
                        { todo = pkgs-todo, done = pkgs-done }
              )
              pkg-config}
          ${./cabal-snippet.dhall}''
