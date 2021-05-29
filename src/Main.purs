module Main where

import Prelude

import Control.Apply.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import WAGS.Change (change)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Functions.Validated (loop, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame, Frame0, Scene)
import WAGS.Graph.AudioUnit (TGain, TSawtoothOsc, TSpeaker)
import WAGS.Graph.Optionals (gain_, sawtoothOsc_)
import WAGS.Graph.Parameter (AudioParameterTransition(..), AudioParameter_(..))
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, getMicrophoneAndCamera, makeUnitCache)
import WAGS.Patch (patch)
import WAGS.Run (SceneI, run)

vol = 1.4 :: Number

type SceneType
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { swt0 :: Unit }
    , swt0 :: TGain /\ { osc0 :: Unit }
    , osc0 :: TSawtoothOsc /\ {}
    }

type FrameTp p i o a
  = Frame (SceneI Unit Unit) FFIAudio (Effect Unit) p i o a

deltaIter :: List (Number /\ Number)
deltaIter = (1.0 /\ 0.0) : (1.08 /\ 1.0) : (1.17 /\ 0.3) : (1.63 /\ 0.1) : (1.83 /\ 0.0) : Nil

changeIter :: forall proof. Boolean -> List (Number /\ Number) -> FrameTp proof SceneType SceneType Unit
changeIter _ Nil = proof `WAGS.bind` flip withProof unit

changeIter forceSet (a : b) = WAGS.do
  doChanges forceSet a
  changeIter false b

doChanges :: forall proof. Boolean ->  Number /\ Number -> FrameTp proof SceneType SceneType Unit
doChanges forceSet (a /\ b)  = WAGS.do
  change { swt0: gain_ (AudioParameter { param: Just b, timeOffset: a, transition: LinearRamp, forceSet }) } $> unit

createFrame :: FrameTp Frame0 {} SceneType Unit
createFrame =
  patch
    :*> change
        { mix: gain_ 0.2
        , swt0: gain_ 0.0
        , osc0: sawtoothOsc_ 440.0
        }
    :*> changeIter true deltaIter

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
piece =
  createFrame
    @|> loop (const $ proof `WAGS.bind` flip withProof unit)

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 15 (initialTime - adj)
  in
    fOf 15

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State
  = { unsubscribe :: Effect Unit
    , audioCtx :: Maybe AudioContext
    }

data Action
  = StartAudio
  | StopAudio

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

classes :: forall r p. Array String -> HP.IProp ( class :: String | r ) p
classes = HP.classes <<< map ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ classes [ "w-screen", "h-screen" ] ]
    [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
        [ HH.div [ classes [ "flex-grow" ] ] []
        , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
            [ HH.div [ classes [ "flex-grow" ] ] []
            , HH.div_
                [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                    [ HH.text "Fun with feedback" ]
                , HH.p [ classes [ "text-center" ] ]
                    [ HH.text "Use headphones!" ]
                , HH.button
                    [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StartAudio ]
                    [ HH.text "Start audio" ]
                , HH.button
                    [ classes [ "text-2xl", "m-5", "bg-pink-500", "p-3", "rounded-lg", "text-white", "hover:bg-pink-400" ], HE.onClick \_ -> StopAudio ]
                    [ HH.text "Stop audio" ]
                ]
            , HH.div [ classes [ "flex-grow" ] ] []
            ]
        , HH.div [ classes [ "flex-grow" ] ] []
        ]
    ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    { microphone } <- H.liftAff $ getMicrophoneAndCamera true false
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    let
      ffiAudio =
        (defaultFFIAudio audioCtx unitCache)
          { microphone = toNullable microphone
          }
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) (pure unit) { easingAlgorithm } (FFIAudio ffiAudio) piece)
            (const $ pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
