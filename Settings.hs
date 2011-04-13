module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    ) where


import qualified Text.Hamlet
import qualified Text.Cassius
import qualified Text.Julius

hamletFile x = Text.Hamlet.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
cassiusFile x = Text.Cassius.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"
juliusFile x = Text.Julius.juliusFileDebug $ "julius/" ++ x ++ ".julius"

--hamletFile x = Text.Hamlet.hamletFile $ "hamlet/" ++ x ++ ".hamlet"
--cassiusFile x = Text.Cassius.cassiusFile $ "cassius/" ++ x ++ ".cassius"
--juliusFile x = Text.Julius.juliusFile $ "julius/" ++ x ++ ".julius"

