{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}

module Ide.Plugin.Cabal where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text as T
import           Development.IDE            as D
import           GHC.Generics
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Types
import qualified Ide.Plugin.Cabal.Parse as Parse
import qualified Ide.Plugin.Cabal.Diag as Diag
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as Map
import Control.DeepSeq (NFData)
import           Data.Hashable
import           Data.Typeable
import           Language.LSP.Server
import qualified Development.IDE.Core.Shake as Shake


import qualified Language.LSP.Types                    as LSP

-- import           Control.Concurrent.STM.Stats          (atomically)
import           Control.Monad.Extra
import Development.IDE.Core.Shake (restartShakeSession)

data Log
  = LogText T.Text
  | LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log
    LogText log' -> pretty log'

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultCabalPluginDescriptor plId)
  { pluginRules = exampleRules recorder
  , pluginHandlers = mkPluginHandler STextDocumentCodeLens (codeLens recorder)
  , pluginCommands = [PluginCommand commandId' "example adding" addTodoCmd]
  , pluginNotificationHandlers = mconcat
  [ mkPluginNotificationHandler LSP.STextDocumentDidOpen $
      \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
      whenUriFile _uri $ \file -> do
          logDebug (ideLogger ide) $ "Opened text document: " <> getUri _uri
          restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " (modified)") []
          runAction "CabalParse" ide $ void $ use Example file

  , mkPluginNotificationHandler LSP.STextDocumentDidChange $
      \ide vfs _ (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        logDebug (ideLogger ide) $ "Modified text document: " <> getUri _uri
        restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " (modified)") []
        runAction "CabalParse" ide $ void $ use Example file

  , mkPluginNotificationHandler LSP.STextDocumentDidSave $
      \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
        whenUriFile _uri $ \file -> do
          logDebug (ideLogger ide) $ "Saved text document: " <> getUri _uri
          restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " (modified)") []
          runAction "CabalParse" ide $ void $ use Example file

  , mkPluginNotificationHandler LSP.STextDocumentDidClose $
        \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
          whenUriFile _uri $ \file -> do
              let msg = "Closed text document: " <> getUri _uri
              logDebug (ideLogger ide) msg
              restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " (modified)") []
              runAction "CabalParse" ide $ void $ use Example file
  ]

  }
  where
    whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
    whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

data Example = Example
    deriving (Eq, Show, Typeable, Generic)
instance Hashable Example
instance NFData   Example

type instance RuleResult Example = ()

exampleRules :: Recorder (WithPriority Log) -> Rules ()
exampleRules recorder = do
  define (cmapWithPrio LogShake recorder) $ \Example file -> do
    _pm <- liftIO $ Parse.parseCabalFile (fromNormalizedFilePath file)
    let diagLst = case _pm of
          (_, Left (_, pErrorNE)) ->
            NE.toList $ NE.map (Diag.errorDiag file) pErrorNE
          _ -> []
    return (diagLst, Just ())

commandId' :: CommandId
commandId' = "codelens.cabal.parse"

codeLens :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'TextDocumentCodeLens
codeLens recorder _ideState plId CodeLensParams{_textDocument=TextDocumentIdentifier uri} = liftIO $ do
    log' Debug $ LogText "ExampleCabal.codeLens entered (ideLogger)"
    case uriToFilePath' uri of
      Just (toNormalizedFilePath -> filePath) -> do
        res <- Parse.parseCabalFile (fromNormalizedFilePath filePath)
        let a = case res of
              (_, Left (_, pErrorNE)) ->
                let diagLst = NE.map (Diag.errorDiag filePath) pErrorNE
                in List $ NE.toList $ NE.map (\(_, _, diag') -> mkCodeLens diag') diagLst
              _ -> List []
        pure $ Right a
      Nothing -> pure $ Right $ List []
  where
    log' = logWith recorder
    mkCodeLens diag' =
      let
        cmdParams = AddTodoParams uri diag'
        cmd = mkLspCommand plId commandId' title (Just [toJSON cmdParams])
        title = "ERROR: " <> Diag._message diag'
      in CodeLens (Diag._range diag') (Just cmd) Nothing
data AddTodoParams = AddTodoParams
  { file     :: Uri  -- ^ Uri of the file to add the pragma to
  , diag :: Diagnostic
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

addTodoCmd :: CommandFunction IdeState AddTodoParams
addTodoCmd _ide (AddTodoParams uri diag') = do
  let
    textEdits = List
      [TextEdit (Diag._range diag')
                  ("-- Parser error:" <> Diag._message diag' <> "\n")
      ]
    res = WorkspaceEdit
      (Just $ Map.singleton uri textEdits)
      Nothing
      Nothing
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing res) (\_ -> pure ())
  return $ Right Null
