-- File auto generated by purescript-bridge! --
module RaffleizeDApp.CustomTypes.TicketTypes where

import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(..))
import Data.Newtype (class Newtype)
import Prim (Int, String)
import Raffleize.Types (AssetClass)
import Type.Proxy (Proxy(Proxy))

import Prelude

newtype TicketStateData =
    TicketStateData {
      tNumber :: Int
    , tSecretHash :: String
    , tSecret :: Maybe String
    , tRaffle :: AssetClass
    , tRaffleValidator :: String
    }

instance encodeJsonTicketStateData :: EncodeJson TicketStateData where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions
instance decodeJsonTicketStateData :: DecodeJson TicketStateData where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions
derive instance genericTicketStateData :: Generic TicketStateData _
derive instance newtypeTicketStateData :: Newtype TicketStateData _

--------------------------------------------------------------------------------
_TicketStateData :: Iso' TicketStateData { tNumber :: Int, tSecretHash :: String, tSecret :: Maybe String, tRaffle :: AssetClass, tRaffleValidator :: String}
_TicketStateData = _Newtype

--------------------------------------------------------------------------------
