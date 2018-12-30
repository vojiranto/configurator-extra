{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}

module Data.Configurator.FromConfig where

import           Universum
import           Control.Monad.Free
import           Data.Configurator.Types
import qualified Data.HashMap.Strict as HM
import           Data.Configurator
import           Language.Haskell.TH.MakeFunctor

{-
data EConfig = EConfig
    { par1 :: ...
    , par2 :: ...
    }

fromConfig cfg = do
    par1 <- cfg .: "par1"
    par2 <- cfg .: "par2"
    pure $ EConfig par1 par2

-}

type ConfigValue  = HM.HashMap Name Value
type ConfigParser = Free ConfigParserF

data ConfigParserF next where
    Require     :: ConfigValue -> Text -> (a -> next) -> ConfigParserF next
    FromMaybe   :: Maybe a -> (a -> next) -> ConfigParserF next

makeFunctorInstance ''ConfigParserF

class FromConfig a where
    fromConfig :: ConfigValue -> Text -> ConfigParser a

instance Configured a => FromConfig a where 
    fromConfig cfg txt = fromMaybeToParser mResult 
        where
            mResult :: Maybe a
            mResult = convert =<< mValue

            mValue :: Maybe Value
            mValue = HM.lookup txt cfg


(.:) :: FromConfig a => ConfigValue -> Text -> ConfigParser a
(.:) cfg txt = liftF $ Require cfg txt id

fromMaybeToParser :: Maybe a -> ConfigParser a
fromMaybeToParser val = liftF $ FromMaybe val id

eitherParseConfig :: FromConfig a => ConfigValue -> Either String a
eitherParseConfig = undefined