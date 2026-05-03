let H =
      https://sellout.github.io/hall/package.dhall
        sha256:e3bb101e3c8ba2f65aa5b4f3156a6ef116f07ad24ecf3c7214f48a686517168c

let Pvp = H.PVP

let ghc = H.GHC.Release

let language = H.GHC.LanguageEdition

let hpack = H.hpack

in  λ(minimumGhcVersion : Pvp.Version) →
      let GHC2024 = language.compatibleWith language.GHC2024 minimumGhcVersion

      in  { defaults =
            { author = "Greg Pfeil <greg@technomadic.org>"
            , extra-doc-files = [ "CHANGELOG.md", "README.md", "docs/*.md" ]
            , flags.noisy-deprecations
              =
              { description =
                  ''
                  Prior to GHC 9.10, the `DEPRECATED` pragma can’t distinguish
                  between terms and types. Consenquently, you can get spurious
                  warnings when there’s a name collision and the name in the
                  other namespace is deprecated. Or you can choose to not get
                  those warnings, at the risk of not being warned when there’s a
                  name collision and the namespace you’re referencing is the one
                  that’s deprecated.
                  ''
              , default = True
              , manual = True
              }
            , ghc-options = [ "-Wtrustworthy-safe" ]
            , language = GHC2024.baseEdition.name
            , default-extensions =
                  GHC2024.additionalExtensions
                # (   [ "DefaultSignatures"
                      , "FunctionalDependencies"
                      , "LiberalTypeSynonyms"
                      , "PackageImports"
                      , "ParallelListComp"
                      , "RecursiveDo"
                      , "TransformListComp"
                      , "NoForeignFunctionInterface"
                      , "NoGeneralizedNewtypeDeriving"
                      , "NoImplicitPrelude"
                      , "NoMonomorphismRestriction"
                      , "NoPatternGuards"
                      ]
                    # ( if    Pvp.lessThanEqual
                                ghc.v8-0-1.version
                                minimumGhcVersion
                        then    [ "StrictData"
                                , "TemplateHaskellQuotes"
                                , "NoTypeApplications"
                                ]
                              # ( if    Pvp.lessThanEqual
                                          ghc.v9-0-1.version
                                          minimumGhcVersion
                                  then    [ "LexicalNegation", "QualifiedDo" ]
                                        # ( if    Pvp.lessThanEqual
                                                    ghc.v9-10-1.version
                                                    minimumGhcVersion
                                            then  [ "RequiredTypeArguments" ]
                                            else  [] : List Text
                                          )
                                  else  [ "NegativeLiterals" ]
                                )
                        else  [] : List Text
                      )
                  )
            , when =
                  hpack.Conditional.maybe
                    ghc.v8-0-1.version
                    minimumGhcVersion
                    (   hpack.Alternative.empty
                      ⫽ { default-extensions = [ "StrictData" ]
                        , -- all-missed-spec: This one just reports unfixable things, AFAICT.
                          -- missing-local-sig: Type inference good.
                          -- unsafe: Warns even when `Unsafe` is explicit, not inferred. See
                          -- https://gitlab.haskell.org/ghc/ghc/-/issues/16689
                          ghc-options =
                          [ "-Weverything"
                          , "-Wno-all-missed-specialisations"
                          , "-Wno-missing-local-signatures"
                          , "-Wno-unsafe"
                          ]
                        }
                    )
                    ( Some
                        (   hpack.Alternative.empty
                          ⫽ { ghc-options = [ "-Wall" ] }
                        )
                    )
                # hpack.Conditional.maybe
                    ghc.v8-10-1.version
                    minimumGhcVersion
                    (   hpack.Alternative.empty
                      ⫽ { -- inferred-safe: If we didn’t allow inferred-safe imports, nothing would be `Safe`.
                          -- prepositive: We support GHC versions without qualified-post.
                          ghc-options =
                          [ "-Wno-inferred-safe-imports"
                          , "-Wno-prepositive-qualified-module"
                          ]
                        }
                    )
                    (None hpack.Alternative.Type)
                # hpack.Conditional.maybe
                    ghc.v9-2-1.version
                    minimumGhcVersion
                    (   hpack.Alternative.empty
                      ⫽ { -- missing-kind: We support GHC versions without kind signatures.
                          ghc-options =
                          [ "-Wno-missing-kind-signatures" ]
                        }
                    )
                    (None hpack.Alternative.Type)
                # hpack.Conditional.maybe
                    ghc.v9-8-1.version
                    minimumGhcVersion
                    (   hpack.Alternative.empty
                      ⫽ { -- missing-poly-kind: We support GHC versions without kind signatures.
                          -- missing-role: Inference good.
                          ghc-options =
                          [ "-Wno-missing-poly-kind-signatures"
                          , "-Wno-missing-role-annotations"
                          ]
                        }
                    )
                    (None hpack.Alternative.Type)
                # [ hpack.Conditional.Type.If
                      (   hpack.Alternative.empty
                        ⫽ { condition = "flag(noisy-deprecations)"
                          , cpp-options = [ "-DSELLOUT_NOISY_DEPRECATIONS" ]
                          }
                      )
                  ]
            }
          , doctests =
              λ(base : { name : Text, versions : List Pvp.Version }) →
              λ(doctest : { name : Text, versions : List Pvp.Version }) →
              λ ( overrides
                : { dependencies :
                      List { name : Text, versions : List Pvp.Version }
                  , when : List hpack.Conditional.Type
                  }
                ) →
                { main = "doctests.hs"
                , source-dirs = [ "tests" ]
                , default-extensions = [ "Unsafe" ]
                , dependencies =
                    H.Cabal.buildPvpDependencies
                      ([ base, doctest ] # overrides.dependencies)
                , when =
                      hpack.Conditional.maybe
                        ghc.v8-0-1.version
                        minimumGhcVersion
                        (   hpack.Alternative.empty
                          ⫽ { ghc-options =
                              [ "-Wno-missing-import-lists", "-Wno-safe" ]
                            }
                        )
                        ( Some
                            (   hpack.Alternative.empty
                              ⫽ { ghc-options =
                                  [ "-fno-warn-missing-import-lists" ]
                                }
                            )
                        )
                    # hpack.Conditional.maybe
                        ghc.v8-4-1.version
                        minimumGhcVersion
                        (   hpack.Alternative.empty
                          ⫽ { ghc-options = [ "-Wno-missing-export-lists" ] }
                        )
                        (None hpack.Alternative.Type)
                    # hpack.Conditional.maybe
                        ghc.v8-8-1.version
                        minimumGhcVersion
                        (   hpack.Alternative.empty
                          ⫽ { ghc-options =
                              [ "-Wno-missing-deriving-strategies" ]
                            }
                        )
                        (None hpack.Alternative.Type)
                    # hpack.Conditional.maybe
                        ghc.v8-10-1.version
                        minimumGhcVersion
                        (   hpack.Alternative.empty
                          ⫽ { -- `doctest` requires the package containing the
                              -- doctests as a dependency to ensure it gets
                              -- built before this test-suite, even though the
                              -- package appears to be unused.
                              ghc-options =
                              [ "-Wno-unused-packages" ]
                            }
                        )
                        (None hpack.Alternative.Type)
                    # overrides.when
                }
          }
