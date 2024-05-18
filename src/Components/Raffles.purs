module Components.Raffles

  where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Raffleize.Types as Raffleize.Types
import RaffleizeDApp.CustomTypes.RaffleTypes as Raffleize.Types
import Web.Event.Event (Event)
import Web.Event.Event as Event

type State =
  { loading :: Boolean
  , result :: Maybe String
  }

data Action
  =  MakeRequest Event

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = { loading: false, result: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form
    [ HE.onSubmit \ev -> MakeRequest ev ]
    [ HH.h1_ [ HH.text "Raffles" ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Get Raffles" ]
    , HH.p_
        [ HH.text $ if st.loading then "Working..." else "" ]
    , HH.div_
        case st.result of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.json ("http://localhost:8082/raffle" )
    let r = case hush response of 
                Nothing -> jsonEmptyObject
                Just resp -> (resp.body)
    let v =  lmap printJsonDecodeError $ decodeJson @Raffleize.Types.RaffleStateData r

    H.modify_ _ { loading = false, result =  Just $ show (v)}