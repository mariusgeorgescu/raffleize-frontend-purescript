module Main where

import Cardano.Wallet.Cip30
import Prelude

import Components.WalletConnectButton as Button
import Components.Raffles as Raffles
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  -- ws <- getAvailableWallets
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Raffles.component unit body