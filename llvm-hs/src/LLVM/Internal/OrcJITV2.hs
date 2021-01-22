module LLVM.Internal.OrcJITV2
  ( ExecutionSession
  , withExecutionSession
  , lookupSymbol
  , createJITDylib
  , ThreadSafeContext
  , withThreadSafeContext
  , createThreadSafeContext
  , disposeThreadSafeContext
  , withThreadSafeModule
  , createThreadSafeModule
  , disposeThreadSafeModule
  , ObjectLayer
  , withRTDyldObjectLinkingLayer
  , IRLayer
  , withIRCompileLayer
  , irLayerAdd
  ) where

import LLVM.Prelude

import Control.Exception
import Foreign.C
import Foreign.Ptr

import LLVM.Internal.Module (Module, readModule)
import LLVM.Internal.OrcJIT (ExecutionSession(..), JITDylib(..), withExecutionSession)
import LLVM.Internal.Target (TargetMachine(..))

import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJITV2 as FFI
import qualified LLVM.Internal.FFI.Target as FFI

newtype ThreadSafeContext = ThreadSafeContext (Ptr FFI.ThreadSafeContext)

newtype ThreadSafeModule = ThreadSafeModule (Ptr FFI.ThreadSafeModule)

data IRLayer = IRLayer
  { _getIRLayer :: Ptr FFI.IRLayer
  , _getDataLayout :: Ptr FFI.DataLayout
  }
newtype ObjectLayer = ObjectLayer (Ptr FFI.ObjectLayer)

createJITDylib :: ExecutionSession -> String -> IO JITDylib
createJITDylib (ExecutionSession es) s = withCString s
  (fmap JITDylib . FFI.createJITDylib es)

getJITDylibByName :: ExecutionSession -> String -> IO JITDylib
getJITDylibByName (ExecutionSession es) s = withCString s
  (fmap JITDylib . FFI.getJITDylibByName es)

lookupSymbol :: ExecutionSession -> JITDylib -> String -> IO Word64
lookupSymbol (ExecutionSession es) (JITDylib dylib) s = withCString s $ \cStr ->
  FFI.lookupSymbol es dylib cStr

createThreadSafeContext :: IO ThreadSafeContext
createThreadSafeContext = ThreadSafeContext <$> FFI.createThreadSafeContext

disposeThreadSafeContext :: ThreadSafeContext -> IO ()
disposeThreadSafeContext (ThreadSafeContext ctx) = FFI.disposeThreadSafeContext ctx

withThreadSafeContext :: (ThreadSafeContext -> IO a) -> IO a
withThreadSafeContext = bracket createThreadSafeContext disposeThreadSafeContext

createThreadSafeModule :: Module -> IO ThreadSafeModule
createThreadSafeModule m = do
  mPtr <- readModule m
  ThreadSafeModule <$> FFI.createThreadSafeModule mPtr

disposeThreadSafeModule :: ThreadSafeModule -> IO ()
disposeThreadSafeModule (ThreadSafeModule m) = FFI.disposeThreadSafeModule m

withThreadSafeModule :: Module -> (ThreadSafeModule -> IO a) -> IO a
withThreadSafeModule m = bracket (createThreadSafeModule m) disposeThreadSafeModule

createRTDyldObjectLinkingLayer :: ExecutionSession -> IO ObjectLayer
createRTDyldObjectLinkingLayer (ExecutionSession es) =
  ObjectLayer <$> FFI.createRTDyldObjectLinkingLayer es

disposeObjectLayer :: ObjectLayer -> IO ()
disposeObjectLayer (ObjectLayer ol) = FFI.disposeObjectLayer ol

withRTDyldObjectLinkingLayer :: ExecutionSession -> (ObjectLayer -> IO a) -> IO a
withRTDyldObjectLinkingLayer es =
  bracket
    (createRTDyldObjectLinkingLayer es)
    disposeObjectLayer

createIRCompileLayer :: ExecutionSession -> ObjectLayer -> TargetMachine -> IO IRLayer
createIRCompileLayer (ExecutionSession es) (ObjectLayer ol) (TargetMachine tm) = do
  dl <- FFI.createTargetDataLayout tm
  il <- FFI.createIRCompileLayer es ol tm
  pure $ IRLayer il dl

disposeIRLayer :: IRLayer -> IO ()
disposeIRLayer (IRLayer il _) = FFI.disposeIRLayer il

withIRCompileLayer :: ExecutionSession -> ObjectLayer -> TargetMachine -> (IRLayer -> IO a) -> IO a
withIRCompileLayer es ol tm =
  bracket
    (createIRCompileLayer es ol tm)
    disposeIRLayer

irLayerAdd :: ThreadSafeModule -> JITDylib -> IRLayer -> IO ()
irLayerAdd (ThreadSafeModule m) (JITDylib dylib) (IRLayer il dl) = do
  FFI.irLayerAdd m dylib dl il
