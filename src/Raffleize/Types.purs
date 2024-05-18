module Raffleize.Types

  where


import Control.Alternative
import Data.Tuple

import Control.Bind ((>>=))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Decode.Decoders (decodeArray, decodeInt, decodeString, decodeTuple)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show (class Show)

type AssetClass = Tuple String String


newtype  ValueItem = ValueItem ((Tuple AssetClass Int)) 


derive instance genericValue:: Generic ValueItem _
derive instance newtypeValue:: Newtype ValueItem  _
derive newtype instance showValue :: Show ValueItem
derive newtype instance eqValue :: Eq ValueItem

instance decodeJsonValueItem :: DecodeJson ValueItem where
  decodeJson json = decodeArray Right json >>= f
    where
    f :: Array Json -> Either JsonDecodeError ValueItem
    f = case _ of
      [ a, b, c ] -> ValueItem <$> (Tuple <$> (Tuple <$> decodeString a <*> decodeString b) <*> decodeInt c)
      _ -> Left $ TypeMismatch "Value"


instance encodeJsonTeam :: EncodeJson ValueItem where
  encodeJson = genericEncodeJson

type Value = List ValueItem





-- -- instances
-- instance showRaffleConfig :: Show RaffleConfig where
--   show = genericShow
-- instance showRaffleParam :: Show RaffleParam where
--   show = genericShow
-- instance showRaffleStateData :: Show RaffleStateData where
--   show = genericShow
