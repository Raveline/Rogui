module Rogui.Animation
  ( AnimationSequence,
    animateValue,
    animateCycle,
    animateBounce,
  )
where

import qualified Data.Sequence as Seq

-- | Animation sequence: list of (value, duration in frames / steps)
type AnimationSequence a = Seq.Seq (a, Int)

animationTotalDuration :: AnimationSequence a -> Int
animationTotalDuration = sum . fmap snd

findInAnimation :: Int -> AnimationSequence a -> Maybe a
findInAnimation remaining ((val, dur) Seq.:<| rest)
  | remaining < dur = Just val
  | otherwise = findInAnimation (remaining - dur) rest
findInAnimation _ _ = Nothing

getLastAnimation :: AnimationSequence a -> Maybe a
getLastAnimation (_ Seq.:|> lastItem) = Just $ fst lastItem
getLastAnimation _ = Nothing

-- | Get current value from an animation sequence based on step count
-- Cycles infinitely through the sequence.
-- You are expected to maintain a state on your side to know
-- when the animation started, don't pass the current steps or frame
-- count directly, or your animation could start in the middle.
animateCycle :: AnimationSequence a -> Int -> Maybe a
animateCycle s step =
  let totalDuration = animationTotalDuration s
      currentStep = step `mod` totalDuration
   in findInAnimation currentStep s

-- | One-shot animation (stops at last frame)
-- You are expected to maintain a state on your side to know
-- when the animation started, don't pass the current steps or frame
-- count directly, or your animation might never start.
animateValue :: AnimationSequence a -> Int -> Maybe a
animateValue s step =
  let totalDuration = animationTotalDuration s
   in if step >= totalDuration
        then getLastAnimation s
        else findInAnimation step s

-- | Bounce animation (reverses at end)
-- You are expected to maintain a state on your side to know
-- when the animation started, don't pass the current steps or frame
-- count directly, or your animation could start in the middle.
animateBounce :: AnimationSequence a -> Int -> Maybe a
animateBounce s@(animation Seq.:|> _lastItem) step = animateCycle (animation <> Seq.reverse s) step
animateBounce Seq.Empty _ = Nothing