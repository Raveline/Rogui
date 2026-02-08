module Rogui.Animation
  ( AnimationSequence,
    animateValue,
    animateCycle,
    animateBounce,
  )
where

import qualified Data.Sequence as Seq

-- | Animation sequence: list of (value, duration in seconds)
type AnimationSequence a = Seq.Seq (a, Double)

animationTotalDuration :: AnimationSequence a -> Double
animationTotalDuration = sum . fmap snd

findInAnimation :: Double -> AnimationSequence a -> Maybe a
findInAnimation remaining ((val, dur) Seq.:<| rest)
  | remaining < dur = Just val
  | otherwise = findInAnimation (remaining - dur) rest
findInAnimation _ _ = Nothing

getLastAnimation :: AnimationSequence a -> Maybe a
getLastAnimation (_ Seq.:|> lastItem) = Just $ fst lastItem
getLastAnimation _ = Nothing

-- | Get current value from an animation sequence based on elapsed time (in seconds).
-- Cycles infinitely through the sequence.
-- You are expected to maintain a state on your side to know
-- when the animation started, don't pass the total elapsed time
-- directly, or your animation could start in the middle.
animateCycle :: AnimationSequence a -> Double -> Maybe a
animateCycle s elapsed =
  let totalDuration = animationTotalDuration s
      currentTime = elapsed - fromIntegral (floor (elapsed / totalDuration) :: Int) * totalDuration
   in findInAnimation currentTime s

-- | One-shot animation (stops at last frame)
-- You are expected to maintain a state on your side to know
-- when the animation started, don't pass the total elapsed time
-- directly, or your animation might never start.
animateValue :: AnimationSequence a -> Double -> Maybe a
animateValue s elapsed =
  let totalDuration = animationTotalDuration s
   in if elapsed >= totalDuration
        then getLastAnimation s
        else findInAnimation elapsed s

-- | Bounce animation (reverses at end)
-- You are expected to maintain a state on your side to know
-- when the animation started, don't pass the total elapsed time
-- directly, or your animation could start in the middle.
animateBounce :: AnimationSequence a -> Double -> Maybe a
animateBounce s@(animation Seq.:|> _lastItem) elapsed = animateCycle (animation <> Seq.reverse s) elapsed
animateBounce Seq.Empty _ = Nothing
