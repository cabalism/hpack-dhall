{ author = "Commercial Haskell SIG"
, category = "Development"
, custom-setup.dependencies
  =
  [ "base >= 4.14.3.0 && < 5", "Cabal < 3.12", "filepath" ]
, dependencies =
  [ "base >= 4.14.3.0 && < 5"
  , "Cabal >= 3.8.1.0"
  , "aeson >= 2.0.3.0"
  , "aeson-warning-parser"
  , "ansi-terminal >= 1.0"
  , "array"
  , "async"
  , "attoparsec"
  , "base64-bytestring"
  , "bytestring"
  , "casa-client"
  , "companion"
  , "conduit"
  , "conduit-extra"
  , "containers"
  , "cryptonite"
  , "directory"
  , "echo"
  , "exceptions"
  , "extra"
  , "file-embed"
  , "filelock"
  , "filepath"
  , "fsnotify >= 0.4.1"
  , "generic-deriving"
  , "hi-file-parser >= 0.1.4.0"
  , "hpack"
  , "hpc"
  , "http-client"
  , "http-client-tls"
  , "http-conduit"
  , "http-download"
  , "http-types"
  , "memory"
  , "microlens"
  , "mtl"
  , "mustache"
  , "neat-interpolation"
  , "open-browser"
  , "optparse-applicative >= 0.18.1.0"
  , "pantry >= 0.8.3"
  , "path"
  , "path-io"
  , "persistent >= 2.14.0.0 && < 2.15"
  , "persistent-sqlite"
  , "pretty"
  , "process >= 1.6.13.2"
  , "project-template"
  , "random"
  , "rio >= 0.1.22.0"
  , "rio-prettyprint >= 0.1.4.0"
  , "split"
  , "stm"
  , "tar"
  , "template-haskell"
  , "text"
  , "time"
  , "transformers"
  , "unix-compat"
  , "unordered-containers"
  , "vector"
  , "yaml"
  , "zlib"
  ]
, description =
    ''
    Please see the documentation at <https://docs.haskellstack.org>
    for usage information.
    .
    If building a 'stack' executable for distribution, please download the
    source code from <https://github.com/commercialhaskell/stack/releases>
    and build it using Stack itself in order to ensure identical behaviour
    to official binaries. This package on Hackage is provided for convenience
    and bootstrapping purposes.
    .
    Note that the API for the library is not currently stable, and may
    change significantly, even between minor releases. It is
    currently only intended for use by the executable.
    ''
, executables =
  { stack =
    { dependencies = [ "stack" ]
    , generated-other-modules = [ "Build_stack", "Paths_stack" ]
    , ghc-options = [ "-threaded", "-rtsopts" ]
    , main = "Main.hs"
    , source-dirs = "src/main"
    , when =
      [ { condition = "flag(static)"
        , cpp-options = None Text
        , dependencies = None (List Text)
        , ld-options = Some [ "-static", "-pthread" ]
        }
      , { condition = "!(flag(disable-git-info))"
        , cpp-options = Some "-DUSE_GIT_INFO"
        , dependencies = Some [ "githash", "optparse-simple" ]
        , ld-options = None (List Text)
        }
      , { condition = "flag(hide-dependency-versions)"
        , cpp-options = Some "-DHIDE_DEP_VERSIONS"
        , dependencies = None (List Text)
        , ld-options = None (List Text)
        }
      , { condition = "flag(supported-build)"
        , cpp-options = Some "-DSUPPORTED_BUILD"
        , dependencies = None (List Text)
        , ld-options = None (List Text)
        }
      ]
    }
  , stack-integration-test =
    { dependencies = [ "filepath", "hspec", "optparse-generic" ]
    , ghc-options = [ "-threaded", "-rtsopts", "-with-rtsopts=-N" ]
    , main = "IntegrationSpec.hs"
    , source-dirs = [ "test/integration", "test/integration/lib" ]
    , when =
      [ { buildable = Some False
        , condition = "!(flag(integration-tests))"
        , ld-options = None (List Text)
        }
      , { buildable = None Bool
        , condition = "flag(static)"
        , ld-options = Some [ "-static", "-pthread" ]
        }
      ]
    }
  }
, extra-source-files =
  [ "CONTRIBUTING.md"
  , "ChangeLog.md"
  , "README.md"
  , "stack.yaml"
  , "doc/*.md"
  , "src/setup-shim/StackSetupShim.hs"
  , "test/package-dump/ghc-7.10.txt"
  , "test/package-dump/ghc-7.8.4-osx.txt"
  , "test/package-dump/ghc-7.8.txt"
  , "test/package-dump/ghc-head.txt"
  , "src/test/Stack/Untar/test1.tar.gz"
  , "src/test/Stack/Untar/test2.tar.gz"
  , "cabal.project"
  , "cabal.config"
  ]
, flags =
  { developer-mode =
    { default = False
    , description = "By default, output extra developer information."
    , manual = True
    }
  , disable-git-info =
    { default = False
    , description =
        "Disable inclusion of current Git information in the Stack executable when it is built."
    , manual = True
    }
  , hide-dependency-versions =
    { default = False
    , description =
        "Hides dependency versions from 'stack --version'. Used only when building a Stack executable for official release. Note to packagers/distributors: DO NOT OVERRIDE THIS FLAG IF YOU ARE BUILDING STACK ANY OTHER WAY (e.g. using Cabal or from Hackage), as it makes debugging support requests more difficult."
    , manual = True
    }
  , integration-tests =
    { default = False
    , description = "Run the integration test suite."
    , manual = True
    }
  , static =
    { default = False
    , description =
        "When building the Stack executable, or the stack-integration-test executable, pass the -static and -pthread flags to the linker used by GHC."
    , manual = True
    }
  , supported-build =
    { default = False
    , description =
        "If false, causes 'stack --version' to issue a warning about the build being unsupported. Used only when building a Stack executable for official release. Note to packagers/distributors: DO NOT OVERRIDE THIS FLAG IF YOU ARE BUILDING STACK ANY OTHER WAY (e.g. using Cabal or from Hackage), as it makes debugging support requests more difficult."
    , manual = True
    }
  }
, ghc-options =
  [ "-fwrite-ide-info"
  , "-hiedir=.hie"
  , "-Wall"
  , "-Wmissing-export-lists"
  , "-optP-Wno-nonportable-include-path"
  ]
, github = "commercialhaskell/stack"
, homepage = "http://haskellstack.org"
, language = "GHC2021"
, library =
  { exposed-modules =
    [ "Control.Concurrent.Execute"
    , "Data.Attoparsec.Args"
    , "Data.Attoparsec.Combinators"
    , "Data.Attoparsec.Interpreter"
    , "Data.Monoid.Map"
    , "Network.HTTP.StackClient"
    , "Options.Applicative.Args"
    , "Options.Applicative.Builder.Extra"
    , "Options.Applicative.Complicated"
    , "Path.CheckInstall"
    , "Path.Extra"
    , "Path.Find"
    , "Stack.Build"
    , "Stack.Build.Cache"
    , "Stack.Build.ConstructPlan"
    , "Stack.Build.Execute"
    , "Stack.Build.Haddock"
    , "Stack.Build.Installed"
    , "Stack.Build.Source"
    , "Stack.Build.Target"
    , "Stack.BuildPlan"
    , "Stack.Clean"
    , "Stack.Config"
    , "Stack.Config.Build"
    , "Stack.Config.Docker"
    , "Stack.Config.Nix"
    , "Stack.ConfigCmd"
    , "Stack.Constants"
    , "Stack.Constants.Config"
    , "Stack.Coverage"
    , "Stack.DefaultColorWhen"
    , "Stack.Docker"
    , "Stack.DockerCmd"
    , "Stack.Dot"
    , "Stack.Eval"
    , "Stack.Exec"
    , "Stack.FileWatch"
    , "Stack.GhcPkg"
    , "Stack.Ghci"
    , "Stack.Ghci.Script"
    , "Stack.Hoogle"
    , "Stack.IDE"
    , "Stack.Init"
    , "Stack.List"
    , "Stack.Ls"
    , "Stack.Lock"
    , "Stack.New"
    , "Stack.Nix"
    , "Stack.Options.BenchParser"
    , "Stack.Options.BuildMonoidParser"
    , "Stack.Options.BuildParser"
    , "Stack.Options.CleanParser"
    , "Stack.Options.ConfigParser"
    , "Stack.Options.Completion"
    , "Stack.Options.DockerParser"
    , "Stack.Options.DotParser"
    , "Stack.Options.EvalParser"
    , "Stack.Options.ExecParser"
    , "Stack.Options.GhcBuildParser"
    , "Stack.Options.GhciParser"
    , "Stack.Options.GhcVariantParser"
    , "Stack.Options.GlobalParser"
    , "Stack.Options.HaddockParser"
    , "Stack.Options.HpcReportParser"
    , "Stack.Options.InitParser"
    , "Stack.Options.LogLevelParser"
    , "Stack.Options.LsParser"
    , "Stack.Options.NewParser"
    , "Stack.Options.NixParser"
    , "Stack.Options.PackageParser"
    , "Stack.Options.PathParser"
    , "Stack.Options.ResolverParser"
    , "Stack.Options.SDistParser"
    , "Stack.Options.ScriptParser"
    , "Stack.Options.SetupParser"
    , "Stack.Options.TestParser"
    , "Stack.Options.UpgradeParser"
    , "Stack.Options.UploadParser"
    , "Stack.Options.Utils"
    , "Stack.Package"
    , "Stack.PackageDump"
    , "Stack.Path"
    , "Stack.Prelude"
    , "Stack.Query"
    , "Stack.Runners"
    , "Stack.Script"
    , "Stack.SDist"
    , "Stack.Setup"
    , "Stack.Setup.Installed"
    , "Stack.SetupCmd"
    , "Stack.SourceMap"
    , "Stack.Storage.Project"
    , "Stack.Storage.User"
    , "Stack.Storage.Util"
    , "Stack.Templates"
    , "Stack.Types.AddCommand"
    , "Stack.Types.AllowNewerDeps"
    , "Stack.Types.ApplyGhcOptions"
    , "Stack.Types.ApplyProgOptions"
    , "Stack.Types.Build"
    , "Stack.Types.Build.Exception"
    , "Stack.Types.BuildConfig"
    , "Stack.Types.BuildOpts"
    , "Stack.Types.CabalConfigKey"
    , "Stack.Types.ColorWhen"
    , "Stack.Types.CompilerBuild"
    , "Stack.Types.CompilerPaths"
    , "Stack.Types.Compiler"
    , "Stack.Types.Config"
    , "Stack.Types.Config.Exception"
    , "Stack.Types.ConfigMonoid"
    , "Stack.Types.ConfigureOpts"
    , "Stack.Types.Curator"
    , "Stack.Types.Docker"
    , "Stack.Types.DockerEntrypoint"
    , "Stack.Types.DownloadInfo"
    , "Stack.Types.DumpLogs"
    , "Stack.Types.DumpPackage"
    , "Stack.Types.EnvConfig"
    , "Stack.Types.EnvSettings"
    , "Stack.Types.ExtraDirs"
    , "Stack.Types.GHCDownloadInfo"
    , "Stack.Types.GHCVariant"
    , "Stack.Types.GhcOptionKey"
    , "Stack.Types.GhcOptions"
    , "Stack.Types.GhcPkgId"
    , "Stack.Types.GlobalOpts"
    , "Stack.Types.GlobalOptsMonoid"
    , "Stack.Types.IsMutable"
    , "Stack.Types.LockFileBehavior"
    , "Stack.Types.NamedComponent"
    , "Stack.Types.Nix"
    , "Stack.Types.Package"
    , "Stack.Types.PackageName"
    , "Stack.Types.ParentMap"
    , "Stack.Types.Platform"
    , "Stack.Types.Project"
    , "Stack.Types.ProjectAndConfigMonoid"
    , "Stack.Types.ProjectConfig"
    , "Stack.Types.PvpBounds"
    , "Stack.Types.Resolver"
    , "Stack.Types.Runner"
    , "Stack.Types.SCM"
    , "Stack.Types.SetupInfo"
    , "Stack.Types.SourceMap"
    , "Stack.Types.StackYamlLoc"
    , "Stack.Types.TemplateName"
    , "Stack.Types.UnusedFlags"
    , "Stack.Types.Version"
    , "Stack.Types.VersionedDownloadInfo"
    , "Stack.Uninstall"
    , "Stack.Unpack"
    , "Stack.Update"
    , "Stack.Upgrade"
    , "Stack.Upload"
    , "System.Info.ShortPathName"
    , "System.Permissions"
    , "System.Process.Pager"
    , "System.Terminal"
    ]
  , generated-exposed-modules = [ "Paths_stack" ]
  , ghc-options = [ "-Widentities" ]
  , source-dirs = "src/"
  , when =
    [ { condition = "os(windows)"
      , `else` =
        { c-sources = "src/unix/cbits/uname.c", source-dirs = "src/unix/" }
      , `then`.source-dirs = "src/windows/"
      }
    ]
  }
, license = "BSD3"
, maintainer = "manny@fpcomplete.com"
, name = "stack"
, spec-version = "0.35.0"
, synopsis = "The Haskell Tool Stack"
, tests.stack-test
  =
  { dependencies = [ "QuickCheck", "hspec", "raw-strings-qq", "stack" ]
  , ghc-options = [ "-threaded" ]
  , main = "Spec.hs"
  , source-dirs = "src/test"
  , verbatim =
      ''
      build-tool-depends:
          hspec-discover:hspec-discover
      ''
  , when =
    [ { condition = "os(windows)"
      , `else`.source-dirs = "src/test/unix/"
      , `then`.source-dirs = "src/test/windows/"
      }
    ]
  }
, version = "2.12.0"
, when =
  [ { condition = "os(windows)"
    , dependencies = None Text
    , `else` = Some
      { cpp-options = None Text
      , dependencies = Some [ "unix" ]
      , verbatim = Some
          ''
          build-tool-depends:
              hsc2hs:hsc2hs
          ''
      }
    , `then` = Some
      { cpp-options = "-DWINDOWS", dependencies = Some [ "Win32" ] }
    }
  , { condition = "impl(ghc >= 9.4.5) && os(windows)"
    , dependencies = Some "network >= 3.1.2.9"
    , `else` =
        None
          { cpp-options : Optional Text
          , dependencies : Optional (List Text)
          , verbatim : Optional Text
          }
    , `then` = None { cpp-options : Text, dependencies : Optional (List Text) }
    }
  , { condition = "flag(developer-mode)"
    , dependencies = None Text
    , `else` = Some
      { cpp-options = Some "-DSTACK_DEVELOPER_MODE_DEFAULT=False"
      , dependencies = None (List Text)
      , verbatim = None Text
      }
    , `then` = Some
      { cpp-options = "-DSTACK_DEVELOPER_MODE_DEFAULT=True"
      , dependencies = None (List Text)
      }
    }
  ]
}
