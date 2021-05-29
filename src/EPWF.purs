module EPWF (makePiecewise) where

import Prelude
import Control.Comonad.Cofree (Cofree, (:<))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested ((/\), type (/\))
import WAGS.Graph.Parameter (AudioParameter, AudioParameterTransition(..), AudioParameter_(..))

type TimeHeadroom
  = { time :: Number, headroom :: Number }

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

makePiecewise :: NonEmpty List (Number /\ Number) -> TimeHeadroom -> Cofree ((->) TimeHeadroom) AudioParameter
makePiecewise = makePiecewise' true

makePiecewise' :: Boolean -> NonEmpty List (Number /\ Number) -> TimeHeadroom -> Cofree ((->) TimeHeadroom) AudioParameter
makePiecewise' forceSet (a /\ b :| Nil) _ =
  AudioParameter
    { param: Just b
    , timeOffset: 0.0
    , transition: LinearRamp
    , forceSet
    }
    :< makePiecewise' false (a /\ b :| Nil)

makePiecewise' forceSet v@(a /\ b :| (Cons (c /\ d) e)) { time, headroom }
  | time <= c =
    let
      lookahead = time + headroom
    in
      ( if lookahead >= c then
          AudioParameter
            { param: Just d
            , timeOffset: c - time
            , transition: LinearRamp
            , forceSet
            }
        else
          AudioParameter { param: Just (calcSlope a b c d time), timeOffset: 0.0, transition: LinearRamp, forceSet }
      )
        :< makePiecewise' false v
  | otherwise = makePiecewise' false (c /\ d :| e) { time, headroom }
