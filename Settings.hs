module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    ) where

import Yesod (MonadControlIO)
import Database.Persist.Postgresql
import LocalSettings -- this needs to define connStr, for connecting to the DB. It is separate to keep the password private when Settings is under version control
import qualified Text.Hamlet
import qualified Text.Cassius
import qualified Text.Julius

hamletFile x = Text.Hamlet.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
cassiusFile x = Text.Cassius.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"
juliusFile x = Text.Julius.juliusFileDebug $ "julius/" ++ x ++ ".julius"

--hamletFile x = Text.Hamlet.hamletFile $ "hamlet/" ++ x ++ ".hamlet"
--cassiusFile x = Text.Cassius.cassiusFile $ "cassius/" ++ x ++ ".cassius"
--juliusFile x = Text.Julius.juliusFile $ "julius/" ++ x ++ ".julius"

connectionCount :: Int
connectionCount = 10

withConnectionPool :: MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withPostgresqlPool connStr connectionCount

runConnectionPool :: MonadControlIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool
