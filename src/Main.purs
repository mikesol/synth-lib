module Main where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Apply.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Monad.Indexed.Qualified as Ix
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import FRP.Event (subscribe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Math (pow)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (freeze, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (OnOff(..), TBandpass, TDelay, TGain, TSawtoothOsc, TSpeaker)
import WAGS.Graph.Parameter (AudioParameter, ff)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, makeUnitCache)
import WAGS.Math (calcSlope)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, SceneI, RunEngine, run)

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
  = IxWAG RunAudio RunEngine p Unit i o a

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

changeIter :: forall proof. (Number /\ Number -> FrameTp proof SceneType SceneType Unit) -> List (Number /\ Number) -> FrameTp proof SceneType SceneType Unit
changeIter _ Nil = ipure unit

changeIter dc (a : b) = Ix.do
  dc a
  changeIter dc b

changeGain :: forall proof. Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeGain (a /\ b) = WAGS.do
  ichange { swt0: ff a $ pure b } $> unit

myQ = 100.0 :: Number

cbpf :: Number -> Number -> AudioParameter
cbpf a b = ff a (pure b)

changeBPF0Freq :: forall proof. Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeBPF0Freq (a /\ b) = WAGS.do
  ichange { bpf0: cbpf a b } $> unit

changeBPF1Freq :: forall proof. Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeBPF1Freq (a /\ b) = WAGS.do
  ichange { bpf1: cbpf a b } $> unit

changeBPF2Freq :: forall proof. Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeBPF2Freq (a /\ b) = WAGS.do
  ichange { bpf2: cbpf a b } $> unit

changeDel :: forall proof. Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeDel (a /\ b) = WAGS.do
  ichange { del0: ff a $ pure b } $> unit

changeOsc :: forall proof. Number /\ Number -> FrameTp proof SceneType SceneType Unit
changeOsc (a /\ b) = WAGS.do
  ichange { osc0: ff a $ pure b } $> unit

fund :: Number
fund = let md = 12.0 in 440.0 * (2.0 `pow` (md / 12.0))

createFrame :: FrameTp Frame0 {} SceneType Unit
createFrame =
  ipatch
    :*> ichange
        { mix: 1.0
        , swt0: 0.0
        , att0: 0.5
        , del0: 0.1
        , osc0: { freq: fund, onOff: On }
        , bpf0: { freq: fund * 3.0, q: myQ }
        , bpf1: { freq: fund * 4.0, q: myQ }
        , bpf2: { freq: fund * 8.0, q: myQ }
        }
    :*> changeIter changeGain deltaGain
    :*> changeIter changeBPF0Freq deltaBPF0Freq
    :*> changeIter changeBPF1Freq deltaBPF1Freq
    :*> changeIter changeBPF2Freq deltaBPF2Freq
    :*> changeIter changeDel deltaDel
    :*> changeIter changeOsc deltaOsc0Freq

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece =
  (const createFrame)
    @!> freeze

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
