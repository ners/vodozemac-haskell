{-# LANGUAGE OverloadedStrings #-}

module Vodozemac.Olm.Message where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import Foreign
import Prelude

data Message
    = PreKeyMessage Object
    | NormalMessage Object

instance ToJSON Message where
    toJSON (PreKeyMessage o) = toJSON o
    toJSON (NormalMessage o) = toJSON o

instance FromJSON Message where
    parseJSON = withObject "Message" $ \o -> do
        t :: Int <- o .: "type"
        case t of
            0 -> pure . PreKeyMessage $ o
            1 -> pure . NormalMessage $ o
            _ -> mzero
