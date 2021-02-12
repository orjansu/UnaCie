
module ThreadUtils (forkedEitherComp) where

import Control.Concurrent   ( forkIO, killThread, newEmptyMVar
                            , putMVar, takeMVar )
import Control.Monad        (void)
import System.Posix.Signals (Handler(..), installHandler, sigINT)

{-
  Information:
  -----------------------------------------------------------------------------
  - Thread helper functions;
  - Used for e.g., evaluating terms with the abstract machine.
-}

-- Compute the result of an Either with a new thread and allowing user to
-- cancel via Ctrl-C;
forkedEitherComp :: Either String b -> String -> IO (Either String b)
forkedEitherComp comp cancelMsg
 = do
     mVar     <- newEmptyMVar
     threadID <- forkIO $ putMVar mVar comp
     old      <- installHandler sigINT (handler threadID mVar) Nothing
     result   <- takeMVar mVar
     void (installHandler sigINT old Nothing)
     return result
   where handler id var = CatchOnce $ do
                           putMVar var (Left cancelMsg)
                           killThread id
