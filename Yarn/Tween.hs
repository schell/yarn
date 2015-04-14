{-# OPTIONS_GHC #-}
{-# LANGUAGE Rank2Types #-}
module Yarn.Tween where

import Yarn.Core
import Yarn.Event
import Control.Applicative

easeInQuad :: Tween m f
easeInQuad = tween $ \c t b -> c * t*t + b

easeOutQuad :: Tween m f
easeOutQuad = tween $ \c t b -> (-c) * (t * (t - 2)) + b

easeInOutQuad :: Tween m f
easeInOutQuad = easeInOut easeInQuad easeOutQuad

easeInCubic :: Tween m f
easeInCubic = tween $ \c t b -> c * t*t*t + b

easeOutCubic :: Tween m f
easeOutCubic = tween $ \c t b -> let t' = t - 1 in c * (t'*t'*t' + 1) + b

easeInOutCubic :: Tween m f
easeInOutCubic = easeInOut easeInCubic easeOutCubic

easeInPow :: Int -> Tween m f
easeInPow power = tween $ \c t b -> c * (t^power) + b

easeOutPow :: Int -> Tween m f
easeOutPow power = tween $ \c t b ->
    let t' = t - 1
        c' = if power `mod` 2 == 1 then c else -c
        i  = if power `mod` 2 == 1 then 1 else -1
    in c' * ((t'^power) + i) + b

easeInSine :: Floating f => Tween m f
easeInSine = tween $ \c t b -> let cos' = cos (t * (pi / 2))
                               in -c * cos' + c + b

easeOutSine :: Floating f => Tween m f
easeOutSine = tween $ \c t b -> let cos' = cos (t * (pi / 2)) in c * cos' + b

easeInOutSine :: Floating f => Tween m f
easeInOutSine = tween $ \c t b -> let cos' = cos (pi * t)
                                  in (-c / 2) * (cos' - 1) + b

easeInExpo :: Floating f => Tween m f
easeInExpo = tween $ \c t b -> let e = 10 * (t - 1) in c * (2**e) + b

easeOutExpo :: Floating f => Tween m f
easeOutExpo = tween $ \c t b -> let e = -10 * t in c * (-(2**e) + 1) + b

easeInOutExpo :: Floating f => Tween m f
easeInOutExpo = easeInOut easeInExpo easeOutExpo

easeInCirc :: Floating f => Tween m f
easeInCirc = tween $ \c t b -> let s = sqrt (1 - t*t) in -c * (s - 1) + b

easeOutCirc :: Floating f => Tween m f
easeOutCirc = tween $ \c t b -> let t' = (t - 1)
                                    s  = sqrt (1 - t'*t')
                                in c * s + b

easeInOutCirc :: Floating f => Tween m f
easeInOutCirc = easeInOut easeInCirc easeOutCirc

easeInOut :: Tween m f -> Tween m f -> Tween m f
easeInOut ein eout start end dur =
    let middle = start + (end - start) / 2
        up     = ein start middle (dur/2)
        down   = eout middle end (dur/2)
    in (up ~> before (dur/2)) `andThen` down

linear :: Tween m f
linear = tween $ \c t b -> c * t + b

type Tween m f = (Monad m, Ord f, Num f, Fractional f)
               => f -> f -> f -> Yarn f m () (Event f)

---- | Applies a tweening function `f` in order to interpolate a value from
---- `start` to `end` over `dur`.
tween :: (Applicative f1, Applicative f, Num s, Monad m1, Monad m, Num t, Ord t, Fractional t)
      => (f1 s -> Yarn t m a1 t -> f s -> Yarn t m1 a b) -> s -> s -> t -> Yarn t m1 a (Event b)
tween f start end dur = (f c t b) ~> before dur
    where c = pure $ end - start
          t = timeAsPercentageOf dur
          b = pure start

-- | Varies 0.0 to 1.0 linearly for duration `t` and 1.0 after `t`.
timeAsPercentageOf :: (Monad m, Ord t, Num t, Fractional t) => t -> Yarn t m a t
timeAsPercentageOf t = liftA2 min 1 (time / pure t)
