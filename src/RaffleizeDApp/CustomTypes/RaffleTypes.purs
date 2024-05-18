-- File auto generated by purescript-bridge! --
module RaffleizeDApp.CustomTypes.RaffleTypes where

import Prelude

import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Prim (Int, Number, String)
import Raffleize.Types (AssetClass, Value)
import Type.Proxy (Proxy(Proxy))

newtype RaffleConfig =
    RaffleConfig {
      rCommitDDL :: Number
    , rRevealDDL :: Number
    , rTicketPrice :: Int
    , rMinTickets :: Int
    , rStake :: Value
    }

derive instance eqRaffleConfig :: Eq RaffleConfig
instance encodeJsonRaffleConfig :: EncodeJson RaffleConfig where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions
instance decodeJsonRaffleConfig :: DecodeJson RaffleConfig where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions
derive instance genericRaffleConfig :: Generic RaffleConfig _
derive instance newtypeRaffleConfig :: Newtype RaffleConfig _

--------------------------------------------------------------------------------
_RaffleConfig :: Iso' RaffleConfig { rCommitDDL :: Number, rRevealDDL :: Number, rTicketPrice :: Int, rMinTickets :: Int, rStake :: Value}
_RaffleConfig = _Newtype

--------------------------------------------------------------------------------
newtype RaffleParam =
    RaffleParam {
      rMaxNoOfTickets :: Int
    , rMinRevealingWindow :: Int
    , rMinTicketPrice :: Int
    , rRaffleValidatorHash :: String
    , rTicketValidatorHash :: String
    , rTicketCollateral :: Int
    , rRaffleCollateral :: Int
    }

instance encodeJsonRaffleParam :: EncodeJson RaffleParam where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions
instance decodeJsonRaffleParam :: DecodeJson RaffleParam where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions
derive instance genericRaffleParam :: Generic RaffleParam _
derive instance newtypeRaffleParam :: Newtype RaffleParam _

--------------------------------------------------------------------------------
_RaffleParam :: Iso' RaffleParam { rMaxNoOfTickets :: Int, rMinRevealingWindow :: Int, rMinTicketPrice :: Int, rRaffleValidatorHash :: String, rTicketValidatorHash :: String, rTicketCollateral :: Int, rRaffleCollateral :: Int}
_RaffleParam = _Newtype

--------------------------------------------------------------------------------
newtype RaffleStateData =
    RaffleStateData {
      rRaffleID :: AssetClass
    , rParam :: RaffleParam
    , rConfig :: RaffleConfig
    , rSoldTickets :: Int
    , rRevealedTickets :: Int
    , rRefundedTickets :: Int
    , rRandomSeed :: Int
    }

instance encodeJsonRaffleStateData :: EncodeJson RaffleStateData where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions
instance decodeJsonRaffleStateData :: DecodeJson RaffleStateData where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions
derive instance genericRaffleStateData :: Generic RaffleStateData _
derive instance newtypeRaffleStateData :: Newtype RaffleStateData _

--------------------------------------------------------------------------------
_RaffleStateData :: Iso' RaffleStateData { rRaffleID :: AssetClass, rParam :: RaffleParam, rConfig :: RaffleConfig, rSoldTickets :: Int, rRevealedTickets :: Int, rRefundedTickets :: Int, rRandomSeed :: Int}
_RaffleStateData = _Newtype

--------------------------------------------------------------------------------
instance showRaffleConfig :: Show RaffleConfig where
  show = genericShow
instance showRaffleParam :: Show RaffleParam where
  show = genericShow
instance showRaffleStateData :: Show RaffleStateData where
  show = genericShow