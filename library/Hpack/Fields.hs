{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Hpack.Fields (cmp) where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.String (IsString())
import Control.Applicative (liftA2)
import Data.Foldable (asum)

-- | A default field ordering comparison.
cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    fromMaybe fallback
    . asum
    $ [ liftA2 compare (elemIndex a xs) (elemIndex b xs) | xs <- fields ]
    where
        -- NOTE: There can be short form conditions with no then or else. In
        -- that case always put condition before the other field name.
        fallback =
            if | a == "condition" -> LT
               | b == "condition" -> GT
               | elem a topLevelFields -> LT
               | elem b topLevelFields -> GT
               | otherwise -> a `compare` b

        fields =
            [ topLevelFields
            , libraryFields ++ commonFields
            , runnableFields ++ commonFields
            , flagFields
            , conditionalFields
            , defaultsFields
            ]

-- | All <https://github.com/sol/hpack#top-level-fields top-level> fields combined.
topLevelFields :: IsString a => [a]
topLevelFields =
    headerFields
    ++ repoFields
    ++ packageFields
    ++ commonFields
    ++ stanzasFields

-- | The header subset of
-- <https://github.com/sol/hpack#top-level-fields top-level> fields.
headerFields :: IsString a => [a]
headerFields =
    [ "spec-version"
    , "name"
    , "version"
    , "synopsis"
    , "description"
    , "category"
    , "stability"
    , "homepage"
    , "bug-reports"
    , "author"
    , "maintainer"
    , "copyright"
    , "license"
    , "license-file"
    , "license-files"
    , "tested-with"
    , "build-type"
    ]

-- | The package subset of
-- <https://github.com/sol/hpack#top-level-fields top-level> fields.
packageFields :: IsString a => [a]
packageFields =
    [ "extra-source-files"
    , "extra-doc-files"
    , "data-files"
    , "data-dir"
    ]

-- | The source repository subset of
-- <https://github.com/sol/hpack#top-level-fields top-level> fields.
repoFields :: IsString a => [a]
repoFields =
    [ "github"
    , "git"
    ]

-- | The stanzas subset of
-- <https://github.com/sol/hpack#top-level-fields top-level> fields.
stanzasFields :: IsString a => [a]
stanzasFields =
    [ "custom-setup"
    , "flags"
    , "library"
    , "internal-libraries"
    , "executables"
    , "executable"
    , "tests"
    , "benchmarks"
    , "defaults"
    ]

-- | The <https://github.com/sol/hpack#common-fields common> fields.
commonFields :: IsString a => [a]
commonFields =
    [ "buildable"
    , "source-dirs"
    , "default-extensions"
    , "other-extensions"
    , "ghc-options"
    , "ghc-prof-options"
    , "ghcjs-options"
    , "cpp-options"
    , "cc-options"
    , "c-sources"
    , "cxx-options"
    , "cxx-sources"
    , "js-sources"
    , "extra-lib-dirs"
    , "extra-libraries"
    , "include-dirs"
    , "install-includes"
    , "frameworks"
    , "extra-framework-dirs"
    , "ld-options"
    , "dependencies"
    , "pkg-config-depends"
    , "build-tools"
    , "system-build-tools"
    , "when" -- conditional
    ]


-- | The <https://github.com/sol/hpack#library-fields library> fields.
libraryFields :: IsString a => [a]
libraryFields =
    [ "exposed"
    , "exposed-modules"
    , "generated-exposed-modules"
    , "other-modules"
    , "generated-other-modules"
    , "reexported-modules"
    , "signatures"
    ]

-- | The <https://github.com/sol/hpack#executable-fields executable>,
-- <https://github.com/sol/hpack#test-fields test> and
-- <https://github.com/sol/hpack#benchmark-fields benchmark> fields are all the
-- same.
runnableFields :: IsString a => [a]
runnableFields =
    [ "main"
    , "other-modules"
    , "generated-other-modules"
    ]

-- | The <https://github.com/sol/hpack#flags flag> fields.
flagFields :: IsString a => [a]
flagFields =
    [ "description"
    , "manual"
    , "default"
    ]

-- | The <https://github.com/sol/hpack#-conditionals conditional> fields.
conditionalFields :: IsString a => [a]
conditionalFields =
    [ "condition"
    , "then"
    , "else"
    ]

-- | The <https://github.com/sol/hpack#defaults defaults> fields.
defaultsFields :: IsString a => [a]
defaultsFields =
    [ "github"
    , "ref"
    , "path"
    , "local"
    ]
