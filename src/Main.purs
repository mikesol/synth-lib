module Main where

import Prelude

import Control.Apply.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import EPWF (calcSlope)
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
import Math (pow)
import WAGS.Change (change)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Functions.Validated (loop, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame, Frame0, Scene)
import WAGS.Graph.AudioUnit (Bandpass, TBandpass, TDelay, TGain, TSawtoothOsc, TSpeaker)
import WAGS.Graph.Optionals (GetSetAP, bandpass_, delay_, gain_, sawtoothOsc_)
import WAGS.Graph.Parameter (AudioParameterTransition(..), AudioParameter_(..))
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, makeUnitCache)
import WAGS.Patch (patch)
import WAGS.Run (SceneI, run)

vol = 1.4 :: Number

type SceneType
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { swt0 :: Unit }
    , del0 :: TDelay /\ { swt0 :: Unit }
    , att0 :: TGain /\ { del0 :: Unit }
    , swt0 :: TGain /\ { bpf0 :: Unit, bpf1 :: Unit, bpf2 :: Unit, att0 :: Unit }
    , bpf0 :: TBandpass /\ { osc0 :: Unit }
    , bpf1 :: TBandpass /\ { osc0 :: Unit }
    , bpf2 :: TBandpass /\ { osc0 :: Unit }
    , osc0 :: TSawtoothOsc /\ {}
    }

type FrameTp p i o a
  = Frame (SceneI Unit Unit) FFIAudio (Effect Unit) p i o a

stT = 0.1 :: Number
endT = 8.1 :: Number

gtime :: Number -> Number
gtime n
  | n < 0.0 = stT
  | n <= 1.0 = calcSlope 0.0 stT 1.0 endT n
  | otherwise = endT

deltaGain :: List (Number /\ Number)
deltaGain = (gtime 0.0 /\ 0.0) : (gtime 0.5 /\ 1.0) : (gtime 1.0 /\ 0.0) : Nil

deltaDel :: List (Number /\ Number)
deltaDel = (gtime 0.0 /\ 0.1) : (gtime 0.1 /\ 0.03) : (gtime 0.2 /\ 0.1) : (gtime 0.3 /\ 0.2) : (gtime 0.4 /\ 0.1) : (gtime 0.5 /\ 0.03) : (gtime 0.6 /\ 0.1) : (gtime 0.8 /\ 0.4) : (gtime 1.0 /\ 0.03) : Nil

deltaBPF0Freq :: List (Number /\ Number)
deltaBPF0Freq = (gtime 0.0 /\ fund * 3.0) : (gtime 0.25 /\ fund * 2.0) : (gtime 1.0 /\ fund * 1.0) : Nil

deltaBPF1Freq :: List (Number /\ Number)
deltaBPF1Freq = (gtime 0.0 /\ fund * 4.0) : (gtime 0.75 /\ fund * 6.0) : (gtime 1.0 /\ fund * 4.0) : Nil

deltaBPF2Freq :: List (Number /\ Number)
deltaBPF2Freq = (gtime 0.0 /\ fund * 8.0) : (gtime 0.25 /\ fund * 9.0) : (gtime 0.5 /\ fund * 8.0) : (gtime 0.75 /\ fund * 10.0) : (gtime 1.0 /\ fund * 8.0) : Nil

deltaOsc0Freq :: List (Number /\ Number) -- no change
deltaOsc0Freq = (gtime 0.7 /\ fund) : (gtime 0.77 /\ fund * 0.98) : (gtime 0.86 /\ fund * 1.00) : (gtime 1.0 /\ fund * 1.00) : Nil

changeIter :: forall proof. (Boolean -> Number /\ Number -> FrameTp proof SceneType SceneType Unit) -> Boolean -> List (Number /\ Number) -> FrameTp proof SceneType SceneType Unit
changeIter _ _ Nil = proof `WAGS.bind` flip withProof unit

changeIter dc forceSet (a : b) = WAGS.do
  dc forceSet a
  changeIter dc false b

changeGain :: forall proof. Boolean -> Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeGain forceSet (a /\ b) = WAGS.do
  change { swt0: gain_ (AudioParameter { param: Just b, timeOffset: a, transition: LinearRamp, forceSet }) } $> unit

myQ = 100.0 :: Number

cbpf :: Number -> Number -> Boolean -> Bandpass GetSetAP GetSetAP
cbpf a b forceSet =
  bandpass_
    { freq: AudioParameter { param: Just b, timeOffset: a, transition: LinearRamp, forceSet }
    , q: myQ
    }

changeBPF0Freq :: forall proof. Boolean -> Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeBPF0Freq forceSet (a /\ b) = WAGS.do
  change { bpf0: cbpf a b forceSet } $> unit

changeBPF1Freq :: forall proof. Boolean -> Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeBPF1Freq forceSet (a /\ b) = WAGS.do
  change { bpf1: cbpf a b forceSet } $> unit

changeBPF2Freq :: forall proof. Boolean -> Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeBPF2Freq forceSet (a /\ b) = WAGS.do
  change { bpf2: cbpf a b forceSet } $> unit

changeDel :: forall proof. Boolean -> Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeDel forceSet (a /\ b) = WAGS.do
  change { del0: delay_ (AudioParameter { param: Just b, timeOffset: a, transition: LinearRamp, forceSet }) } $> unit

changeOsc :: forall proof. Boolean -> Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeOsc forceSet (a /\ b) = WAGS.do
  change { osc0: sawtoothOsc_ (AudioParameter { param: Just b, timeOffset: a, transition: LinearRamp, forceSet }) } $> unit

fund :: Number
fund = let md = 12.0 in 440.0 * (2.0 `pow` (md / 12.0))

createFrame :: FrameTp Frame0 {} SceneType Unit
createFrame =
  patch
    :*> change
        { mix: gain_ 1.0
        , swt0: gain_ 0.0
        , att0: gain_ 0.5
        , del0: delay_ 0.1
        , osc0: sawtoothOsc_ fund
        , bpf0: bandpass_ { freq: fund * 3.0, q: myQ }
        , bpf1: bandpass_ { freq: fund * 4.0, q: myQ }
        , bpf2: bandpass_ { freq: fund * 8.0, q: myQ }
        }
    :*> changeIter changeGain true deltaGain
    :*> changeIter changeBPF0Freq true deltaBPF0Freq
    :*> changeIter changeBPF1Freq true deltaBPF1Freq
    :*> changeIter changeBPF2Freq true deltaBPF2Freq
    :*> changeIter changeDel true deltaDel
    :*> changeIter changeOsc true deltaOsc0Freq

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
                    [ HH.text "Beautiful alien" ]
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
    --{ microphone } <- H.liftAff $ getMicrophoneAndCamera true false
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache) -- { microphone = toNullable microphone }
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
