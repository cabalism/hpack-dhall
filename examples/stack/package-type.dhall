{ author : Text
, category : Text
, custom-setup : { dependencies : List Text }
, dependencies : List Text
, description : Text
, executables :
    { stack :
        { dependencies : List Text
        , generated-other-modules : List Text
        , ghc-options : List Text
        , main : Text
        , source-dirs : Text
        , when :
            List
              { condition : Text
              , cpp-options : Optional Text
              , dependencies : Optional (List Text)
              , ld-options : Optional (List Text)
              }
        }
    , stack-integration-test :
        { dependencies : List Text
        , ghc-options : List Text
        , main : Text
        , source-dirs : List Text
        , when :
            List
              { buildable : Optional Bool
              , condition : Text
              , ld-options : Optional (List Text)
              }
        }
    }
, extra-source-files : List Text
, flags :
    { developer-mode : { default : Bool, description : Text, manual : Bool }
    , disable-git-info : { default : Bool, description : Text, manual : Bool }
    , hide-dependency-versions :
        { default : Bool, description : Text, manual : Bool }
    , integration-tests : { default : Bool, description : Text, manual : Bool }
    , static : { default : Bool, description : Text, manual : Bool }
    , supported-build : { default : Bool, description : Text, manual : Bool }
    }
, ghc-options : List Text
, github : Text
, homepage : Text
, language : Text
, library :
    { exposed-modules : List Text
    , generated-exposed-modules : List Text
    , ghc-options : List Text
    , source-dirs : Text
    , when :
        List
          { condition : Text
          , `else` : { c-sources : Text, source-dirs : Text }
          , `then` : { source-dirs : Text }
          }
    }
, license : Text
, maintainer : Text
, name : Text
, spec-version : Text
, synopsis : Text
, tests :
    { stack-test :
        { dependencies : List Text
        , ghc-options : List Text
        , main : Text
        , source-dirs : Text
        , verbatim : Text
        , when :
            List
              { condition : Text
              , `else` : { source-dirs : Text }
              , `then` : { source-dirs : Text }
              }
        }
    }
, version : Text
, when :
    List
      { condition : Text
      , dependencies : Optional Text
      , `else` :
          Optional
            { cpp-options : Optional Text
            , dependencies : Optional (List Text)
            , verbatim : Optional Text
            }
      , `then` :
          Optional { cpp-options : Text, dependencies : Optional (List Text) }
      }
}
