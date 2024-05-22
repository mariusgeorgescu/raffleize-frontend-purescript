module Components.Raffles
  ( Action(..)
  , State
  , component
  , handleAction
  , initialState
  , renderDeadline
  , renderRaffleParam
  )
  where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime, date, time)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (drop, length, take)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HSE
import Raffleize.Types (AssetClass, ValueItem(..), flattenValue, Value)
import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleConfig(..), RaffleInfo(..), RaffleParam(..), RaffleStateData(..))
import Web.UIEvent.MouseEvent (MouseEvent)


type State =
  { loading :: Boolean
  , result :: Maybe RaffleInfo
  }

data Action
  =  MakeRequest MouseEvent

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
render st | st.loading == true  =   HH.button [ HP.classes [ HH.ClassName "btn btn-primary"]] [HH.span [HP.classes [ HH.ClassName "loading loading-spinner"]] [HH.text "Loading"]]
render {loading : l, result : Nothing } =   HH.button 
                                              [ HP.disabled l  
                                              , HE.onClick \ev -> MakeRequest ev
                                              , HP.classes [ HH.ClassName "btn btn-primary"]
                                              ]
                                              [ HH.text "Update raffle info" ]
render {loading : _, result : Just ri} = renderRaffleInfo ri
                                                      
                

renderRaffleStateData ∷ forall cs m. RaffleStateData -> H.ComponentHTML Action cs m
renderRaffleStateData r@(RaffleStateData rsd) = HH.div [ HP.classes [ HH.ClassName "stats" ] ] 
                                                      [ HH.div [ HP.classes [ HH.ClassName "stats stats-vertical" ] ] 
                                                        [
                                                          renderRaffleConfig rsd.rConfig
                                                        , renderCollapse false "Progress" $ renderRaffleStateData' r
                                                        , renderRaffleParam rsd.rParam 
                                                        ]
                                                      ]


renderRaffleStateData' ∷ forall cs m. RaffleStateData -> H.ComponentHTML Action cs m
renderRaffleStateData' (RaffleStateData rsd) = HH.div [ HP.classes [ HH.ClassName "stats stats-vertical" ] ] 
                                                      [ renderStatSecondary (shortString 5 (fst rsd.rRaffleID)) "Policy ID" "Raffleize Minting Policy ID"
                                                      , renderStatSecondary (shortString 5 (snd rsd.rRaffleID)) "Raffle ID" "Raffleize Token Name"
                                                      , renderStat rsd.rSoldTickets "Ticket Sold" "Number of tickets sold"
                                                      , renderStat rsd.rRevealedTickets "Ticket Revealed" "Number of tickets revealed"
                                                      , renderStat rsd.rRefundedTickets "Ticket Refunded" "Number of tickets refunded"
                                                      , renderStat rsd.rRandomSeed "Random Seed" "Current random seed"
                                                      ]


renderRaffleConfig ∷ forall cs m. RaffleConfig -> H.ComponentHTML Action cs m
renderRaffleConfig (RaffleConfig cfg) = renderCollapse true "Configuration" $    HH.div [ HP.classes [ HH.ClassName "stats stats-vertical" ] ] 
                                                    [  renderDeadline cfg.rCommitDDL "Commit Deadline" "Deadline for buying tickets to the raffle"
                                                      , renderDeadline cfg.rRevealDDL "Reveal Deadline" "Deadline for revealing secrets of the tickets"
                                                      , renderStat cfg.rMinTickets "Minimum Tickets Threshold" "Minimum tickets to be sold"
                                                      , renderStat cfg.rTicketPrice "Ticket Price" "Ticket price in Lovelaces"
                                                      , renderValueTable cfg.rStake
                                                      ] 
                                                    

renderRaffleParam ∷ forall cs m. RaffleParam -> H.ComponentHTML Action cs m
renderRaffleParam (RaffleParam params) = renderCollapse false "Protocol parameters" $    
                                                    HH.div [ HP.classes [ HH.ClassName "stats stats-vertical" ] ] 
                                                      [ 
                                                        renderStat params.rMaxNoOfTickets "Max. No. of Tickets" "Maximum number of tickets that can be sold"
                                                      , renderStat params.rMinRevealingWindow "Revealing window" "Minimum time window between commit and reveal deadlines"
                                                      , renderStat params.rMinTicketPrice "Min. Ticket Price" "Minimum price for raffle tickets (in Lovelaces)"
                                                      , renderStatSecondary (shortString 5 params.rRaffleValidatorHash) "Raffle Validator Hash" "The hash of the raffle validator"
                                                      , renderStatSecondary (shortString 5 params.rTicketValidatorHash) "Ticket Validator Hash" "The hash of the ticket validator"
                                                      , renderStat params.rRaffleCollateral "Raffle Collateral" "Amount that must be locked with the raffle state (in Lovelaces)"
                                                      , renderStat params.rTicketCollateral "Ticket Collateral" "Amount that must be locked with the raffle state (in Lovelaces)"
                                                      ] 
                                                  



renderCollapse ∷ ∀ w i.  Boolean -> String → HH.HTML w i → HH.HTML w i
renderCollapse expanded title content =  HH.div [HP.classes [ HH.ClassName "collapse border collapse-arrow bg-base-200" ]] 
                              [  HH.input [HP.type_ HP.InputCheckbox , HP.checked expanded, HP.classes [ HH.ClassName "peer" ]]
                              ,  HH.div [HP.classes [ HH.ClassName "collapse-title"]] [HH.text title]
                              ,  HH.div [HP.classes [ HH.ClassName "collapse-content"]] [content]
                              ] 

renderStatSecondary  ∷ forall w i. String -> String -> String -> HH.HTML w i
renderStatSecondary  value title desc =   HH.div [ HP.classes [ HH.ClassName "stats rounded-none" ] ] 
                                          [ HH.div [ HP.classes [ HH.ClassName "stat-title" ] ] [ HH.text title] 
                                            , HH.div [ HP.classes [ HH.ClassName "stat-value text-sm" ] ] [ HH.text value] 
                                            , HH.div [ HP.classes [ HH.ClassName "stat-desc" ] ] [ HH.text desc] 
                                          ]


mapToTH :: forall w i. Array String -> Array (HH.HTML w i)
mapToTH strings = map (\s -> HH.th [] [HH.text s]) strings

mapToTD :: forall w i. Array String -> Array (HH.HTML w i)
mapToTD strings = map (\s -> HH.td [] [HH.text s]) strings


renderTable  ∷ forall w i.  Array String → Array (Array String )→ HH.HTML w i
renderTable columns contents =  HH.div [ HP.classes [ HH.ClassName "overflow-x-auto" ] ] 
                      [ HH.table [HP.classes [ HH.ClassName "table" ] ] 
                         [
                          HH.thead [] [HH.tr [] (mapToTH columns)]
                          , HH.tbody [] ((\c -> HH.tr [] (mapToTD c) ) <$> contents)
                         ]
                      ] 

renderValueTable :: forall w i. Value -> HH.HTML w i
renderValueTable value = renderTable ["Currency Symbol", "Token Name", "Quantty"] (flattenValue value)



renderDeadline ∷ ∀ w i. Number → String → String → HH.HTML w i
renderDeadline =  renderStatSecondary <<< printPOSIX' 

renderStat∷ ∀ w i a . Show a => a → String → String → HH.HTML w i
renderStat =  renderStatSecondary <<< show 


printPOSIX ∷ Number → Maybe String
printPOSIX n =  do  
  dt <- toDateTime <$> instant (Milliseconds n ) 
  fdt <- hush $ formatDateTime "D-MMM-YY HH:mm" dt
  pure fdt

printPOSIX' ∷ Number → String
printPOSIX'  =  fromMaybe "" <<< printPOSIX 


shortString :: Int -> String -> String
shortString i s = let len = length s 
                in if len > (2 * i) then  
                    take i s <> "..." <> drop (len - i) s else s



-- TODO CARD TOOLTIP PROGRESS
-- TODO CARD TOOLTIP PROGRESS
-- TODO CARD TOOLTIP PROGRESS
-- TODO CARD TOOLTIP PROGRESS

renderRaffleInfo∷ forall cs m. RaffleInfo -> H.ComponentHTML Action cs m
renderRaffleInfo (RaffleInfo ri) = HH.div [ HP.classes [ HH.ClassName "flex flex-col lg:flex-row" ] ] 
                                    [ HH.div [ HP.classes [ HH.ClassName "flex flex-col" ] ] 
                                      [
                                      renderRaffleTimeline ri.riRsd
                                      ,HH.div [ HP.classes [ HH.ClassName "divider divider-primary" ] ] []
                                      ,renderRaffleTimeline ri.riRsd
                                      ]
                                    ,HH.div [ HP.classes [ HH.ClassName "divider divider-horizontal divider-primary" ] ] []
                                    ,HH.img [HP.src ri.riImage, HP.width 340, HP.height 340]

                                    ]


renderRaffleTimeline∷ forall cs m. RaffleStateData -> H.ComponentHTML Action cs m
renderRaffleTimeline (RaffleStateData rsd)  =  HH.ul [ HP.classes  [HH.ClassName "timeline" ]]
                                                [
                                                  HH.li [] 
                                                  [ HH.div [ HP.classes  [HH.ClassName "timeline-start timeline-box" ]] [HH.text "Created"]      
                                                  , HH.div [ HP.classes  [HH.ClassName "timeline-middle bg-success text-success" ]] [HH.text "|"]
                                                  , HH.hr [ HP.classes  [HH.ClassName "bg-warning" ]]                                   
                                                  ]
                                                  , HH.li [] 
                                                  [ HH.hr [ HP.classes  [HH.ClassName "bg-warning" ]]   
                                                  , HH.div [ HP.classes  [HH.ClassName "timeline-end timeline-box" ]] [HH.text "Commit Deadline"]
                                                  , HH.div [ HP.classes  [HH.ClassName "timeline-middle bg-warning text-warning" ]] [HH.text "|"]
                                                  , HH.hr [ HP.classes  [HH.ClassName "bg-neutral text-neutral" ]]                                      
                                                  ]
                                                  , HH.li [] 
                                                  [ HH.hr [ HP.classes  [HH.ClassName "bg-neutral text-neutral" ]]   
                                                  , HH.div [ HP.classes  [HH.ClassName "timeline-start timeline-box" ]] [HH.text "Reveal Deadline"]
                                                  , HH.div [ HP.classes  [HH.ClassName "timeline-middle bg-neutral text-neutral" ]] [HH.text "|"]                             
                                                  ]
                                                ]



-- <div class="flex flex-col w-full lg:flex-row">
--   <div class="grid flex-grow h-32 card bg-base-300 rounded-box place-items-center">content</div> 
--   <div class="divider lg:divider-horizontal">OR</div> 
--   <div class="grid flex-grow h-32 card bg-base-300 rounded-box place-items-center">content</div>
-- </div>


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  MakeRequest event -> do
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.json ("http://localhost:8082/info" )
    let r = case hush response of 
                Nothing -> jsonEmptyObject
                Just resp -> (resp.body)
    let ri =  lmap printJsonDecodeError $ decodeJson @RaffleInfo r

    H.modify_ _ { loading = false, result =  hush ri}