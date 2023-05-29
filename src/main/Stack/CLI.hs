{-# LANGUAGE NoImplicitPrelude #-}

module Stack.CLI
  ( commandLineHandler
  ) where

import           BuildInfo ( hpackVersion, versionString' )
import           Data.Attoparsec.Interpreter ( getInterpreterArgs )
import           Data.Char ( toLower )
import qualified Data.List as L
import           Options.Applicative
                   ( Parser, ParserFailure, ParserHelp, ParserResult (..), flag
                   , handleParseResult, help, helpError, idm, long, metavar
                   , overFailure, renderFailure, strArgument, switch )
import           Options.Applicative.Help ( errorHelp, stringChunk, vcatChunks )
import           Options.Applicative.Builder.Extra
                   ( boolFlags, extraHelpOption, textOption )
import           Options.Applicative.Complicated
                   ( addCommand, addSubCommands, complicatedOptions )
import qualified RIO.Process ( exec )
import           RIO.Process ( withProcessContextNoLogging )
import           Stack.Build ( buildCmd )
import           Stack.Clean ( CleanCommand (..), cleanCmd )
import           Stack.ConfigCmd as ConfigCmd
import           Stack.Constants ( globalFooter, osIsWindows, stackProgName )
import           Stack.Coverage ( hpcReportCmd )
import           Stack.Docker
                   ( dockerCmdName, dockerHelpOptName, dockerPullCmdName )
import           Stack.DockerCmd ( dockerPullCmd, dockerResetCmd )
import qualified Stack.Dot ( dot )
import           Stack.Exec ( SpecialExecCmd (..), execCmd )
import           Stack.Eval ( evalCmd )
import           Stack.InitGhc ( initGhcCmd )
import           Stack.IDE
                   ( ListPackagesCmd (..), OutputStream (..), idePackagesCmd
                   , ideTargetsCmd
                   )
import qualified Stack.Nix as Nix
import           Stack.Options.BuildParser ( buildOptsParser,  )
import           Stack.Options.GlobalParser ( globalOptsParser )
import           Stack.Options.Utils ( GlobalOptsContext (..) )
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withDefaultEnvConfig )
import           Stack.Prelude
import           Stack.Types.AddCommand ( AddCommand )
import           Stack.Types.BuildOpts ( BuildCommand (..))
import           Stack.Types.GlobalOptsMonoid ( GlobalOptsMonoid (..) )
import           Stack.Types.Runner ( Runner )
import           Stack.Types.Version ( stackVersion )
import qualified System.Directory as D
import           System.Environment ( getProgName, withArgs )
import           System.FilePath ( pathSeparator )

commandLineHandler ::
     FilePath
  -> String
  -> Bool
  -> IO (GlobalOptsMonoid, RIO Runner ())
commandLineHandler currentDir progName isInterpreter =
  -- Append the relevant default (potentially affecting the LogLevel) *after*
  -- appending the global options of the `stack` command to the global options
  -- of the subcommand - see #5326.
  first (<> defaultGlobalOpts) <$> complicatedOptions
    stackVersion
    (Just versionString')
    hpackVersion
    "stack - The Haskell Tool Stack"
    ""
    "Stack's documentation is available at https://docs.haskellstack.org/. \
    \Command 'stack COMMAND --help' for help about a Stack command. Stack also \
    \supports the Haskell Error Index at https://errors.haskell.org/."
    (globalOpts OuterGlobalOpts)
    (Just failureCallback)
    addCommands
 where
  defaultGlobalOpts = if isInterpreter
    then
      -- Silent except when errors occur - see #2879
      mempty { globalMonoidLogLevel = First (Just LevelError) }
    else mempty
  failureCallback f args =
    case L.stripPrefix "Invalid argument" (fst (renderFailure f "")) of
      Just _ -> if isInterpreter
                  then parseResultHandler args f
                  else secondaryCommandHandler args f
                      >>= interpreterHandler currentDir args
      Nothing -> parseResultHandler args f

  parseResultHandler args f =
    if isInterpreter
    then do
      let hlp = errorHelp $ stringChunk
            (unwords ["Error executing interpreter command:"
                      , progName
                      , unwords args])
      handleParseResult (overFailure (vcatErrorHelp hlp) (Failure f))
    else handleParseResult (Failure f)

  addCommands = do
    initGhcVersion

  initGhcVersion = addCommand'
    "get-ghc-version"
    "Get the appropriate GHC Version for your project."
    initGhcCmd
    (buildOptsParser Build)

  -- addCommand hiding global options
  addCommand' ::
       String
    -> String
    -> (a -> RIO Runner ())
    -> Parser a
    -> AddCommand
  addCommand' cmd title constr =
    addCommand
      cmd
      title
      globalFooter
      constr
      (\_ gom -> gom)
      (globalOpts OtherCmdGlobalOpts)

  globalOpts :: GlobalOptsContext -> Parser GlobalOptsMonoid
  globalOpts kind = globalOptsParser currentDir kind
   where
    hide = kind /= BuildCmdGlobalOpts

-- | fall-through to external executables in `git` style if they exist
-- (i.e. `stack something` looks for `stack-something` before
-- failing with "Invalid argument `something'")
secondaryCommandHandler ::
     [String]
  -> ParserFailure ParserHelp
  -> IO (ParserFailure ParserHelp)
secondaryCommandHandler args f =
  -- don't even try when the argument looks like a path or flag
  if elem pathSeparator cmd || "-" `L.isPrefixOf` L.head args
     then pure f
  else do
    mExternalExec <- D.findExecutable cmd
    case mExternalExec of
      Just ex -> withProcessContextNoLogging $ do
        -- TODO show the command in verbose mode
        -- hPutStrLn stderr $ unwords $
        --   ["Running", "[" ++ ex, unwords (tail args) ++ "]"]
        _ <- RIO.Process.exec ex (L.tail args)
        pure f
      Nothing -> pure $ fmap (vcatErrorHelp (noSuchCmd cmd)) f
 where
  -- FIXME this is broken when any options are specified before the command
  -- e.g. stack --verbosity silent cmd
  cmd = stackProgName ++ "-" ++ L.head args
  noSuchCmd name = errorHelp $ stringChunk
    ("Auxiliary command not found in path '" ++ name ++ "'.")

interpreterHandler ::
     Monoid t
  => FilePath
  -> [String]
  -> ParserFailure ParserHelp
  -> IO (GlobalOptsMonoid, (RIO Runner (), t))
interpreterHandler currentDir args f = do
  -- args can include top-level config such as --extra-lib-dirs=... (set by
  -- nix-shell) - we need to find the first argument which is a file, everything
  -- afterwards is an argument to the script, everything before is an argument
  -- to Stack
  (stackArgs, fileArgs) <- spanM (fmap not . D.doesFileExist) args
  case fileArgs of
    (file:fileArgs') -> runInterpreterCommand file stackArgs fileArgs'
    [] -> parseResultHandler (errorCombine (noSuchFile firstArg))
 where
  firstArg = L.head args

  spanM _ [] = pure ([], [])
  spanM p xs@(x:xs') = do
    r <- p x
    if r
    then do
      (ys, zs) <- spanM p xs'
      pure (x:ys, zs)
    else
      pure ([], xs)

  -- if the first argument contains a path separator then it might be a file,
  -- or a Stack option referencing a file. In that case we only show the
  -- interpreter error message and exclude the command related error messages.
  errorCombine =
    if pathSeparator `elem` firstArg
    then overrideErrorHelp
    else vcatErrorHelp

  overrideErrorHelp h1 h2 = h2 { helpError = helpError h1 }

  parseResultHandler fn = handleParseResult (overFailure fn (Failure f))
  noSuchFile name = errorHelp $ stringChunk
    ("File does not exist or is not a regular file '" ++ name ++ "'.")

  runInterpreterCommand path stackArgs fileArgs = do
    progName <- getProgName
    iargs <- getInterpreterArgs path
    let parseCmdLine = commandLineHandler currentDir progName True
        -- Implicit file arguments are put before other arguments that
        -- occur after "--". See #3658
        cmdArgs = stackArgs ++ case break (== "--") iargs of
          (beforeSep, []) -> beforeSep ++ ["--"] ++ [path] ++ fileArgs
          (beforeSep, optSep : afterSep) ->
            beforeSep ++ [optSep] ++ [path] ++ fileArgs ++ afterSep
     -- TODO show the command in verbose mode
     -- hPutStrLn stderr $ unwords $
     --   ["Running", "[" ++ progName, unwords cmdArgs ++ "]"]
    (a,b) <- withArgs cmdArgs parseCmdLine
    pure (a,(b,mempty))

-- Vertically combine only the error component of the first argument with the
-- error component of the second.
vcatErrorHelp :: ParserHelp -> ParserHelp -> ParserHelp
vcatErrorHelp h1 h2 = h2 { helpError = vcatChunks [helpError h2, helpError h1] }
