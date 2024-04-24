module Button where

import Data.Maybe (Maybe(..), fromMaybe)
import Prelude

import Cardano.Wallet.Cip30 (Api, enable, getBalance)
import Csl as Csl
import Effect.Aff.Class (class MonadAff)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA

type ConnectedWallet = { api :: Api, name :: String, balance :: String }

type WalletConnectState = { availableWalletExtensions :: Array String, connectedWallet :: Maybe ConnectedWallet }

data WalletConnectAction = ConnectWallet String

component :: forall query input output m. MonadAff m => H.Component query (Array String) output m
component =
  H.mkComponent
    { initialState: \ws -> { availableWalletExtensions: ws, connectedWallet: Nothing }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. WalletConnectState -> H.ComponentHTML WalletConnectAction cs m
render state =
  HH.div_
    [ renderWalletConnect state
    , case state.connectedWallet of
        Nothing -> HH.div_ []
        Just w -> HH.text (show (w.balance))
    ]

renderWalletListItem ∷ forall cs m. String -> H.ComponentHTML WalletConnectAction cs m
renderWalletListItem s = HH.li [ HE.onClick \_ -> ConnectWallet s ] [ HH.a_ [ HH.text s ] ]

-- <div class="card-body">
--       <h3 class="card-title">Card title!</h3>
--       <p>you can use any element as a dropdown.</p>
--     </div>
renderWalletsList ∷ forall cs m. Array String -> H.ComponentHTML WalletConnectAction cs m
renderWalletsList ws = case ws of
  [] ->
    HH.div [ HP.classes [ HH.ClassName "dropdown-content z-[1] card card-compact w-64 p-2 shadow bg-primary text-primary-content" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "card-body" ] ]
          [ HH.h3 [ HP.classes [ HH.ClassName "card-title" ] ] [ HH.text "no wallets" ]
          , HH.p_ [ HH.text "no wallets" ]
          ]
      ]
  _ ->
    HH.ul
      [ HP.tabIndex 0
      , HP.classes [ HH.ClassName "dropdown-content z-[1] menu p-2 shadow bg-base-100 rounded-box w-52" ]
      ]
      (renderWalletListItem <$> ws)

renderWalletConnect ∷ forall cs m. WalletConnectState -> H.ComponentHTML WalletConnectAction cs m
renderWalletConnect { availableWalletExtensions, connectedWallet } =
  HH.div [ HP.classes [ HH.ClassName "flex justify-end", HH.ClassName "dropdown dropdown-bottom dropdown-end" ] ]
    [ HH.div
        [ HP.tabIndex 0
        , HPA.role "button"
        , HP.classes [ HH.ClassName "btn m-1" ]
        ]
        [ HH.text (printConnectedWallet connectedWallet) ]
    , renderWalletsList availableWalletExtensions
    ]

handleAction :: forall cs o m. MonadAff m => WalletConnectAction → H.HalogenM WalletConnectState WalletConnectAction cs o m Unit
handleAction = case _ of
  ConnectWallet wallet -> do
    api <- liftAff $ enable wallet [ { cip: 30 } ]
    b <- liftAff $ getBalance api
    let b2 = fromMaybe Csl.value.zero $ Csl.value.fromHex b
    -- let b3 = flattenValue
    H.modify_ \st -> st { connectedWallet = Just { api: api, name: wallet, balance: ((Csl.bigNum.toStr $ Csl.value.coin b2)) } }

printConnectedWallet :: Maybe ConnectedWallet -> String
printConnectedWallet mw = case mw of
  Nothing -> "Connect"
  Just w -> "Connected" <> " " <> w.name