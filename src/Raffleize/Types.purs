module Raffleize.Types
  where

import Data.Tuple (Tuple(..))
import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Decode.Decoders (decodeArray, decodeInt, decodeString)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

type AssetClass
  = Tuple String String

newtype ValueItem
  = ValueItem ((Tuple AssetClass Int))

derive instance genericValue :: Generic ValueItem _

derive instance newtypeValue :: Newtype ValueItem _

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

type Value
  = Array ValueItem

flattenValueItem :: ValueItem -> Array String
flattenValueItem (ValueItem ((Tuple (Tuple cs tn) i))) = [ cs, tn, show i ]

flattenValue :: Value -> Array (Array String)
flattenValue = map flattenValueItem

-- -- instances
-- instance showRaffleConfig :: Show RaffleConfig where
--   show = genericShow
-- instance showRaffleParam :: Show RaffleParam where
--   show = genericShow
-- instance showRaffleStateData :: Show RaffleStateData where
--   show = genericShow
