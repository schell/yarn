{-# LANGUAGE FlexibleContexts #-}
module Yarn.Core where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Arrow
import Data.Time.Clock
import Debug.Trace

--------------------------------------------------------------------------------
-- Yarn instances
--------------------------------------------------------------------------------
instance Monad m => Arrow (Yarn m) where
    arr = yarn
    first y = Yarn $ \dt (b,d) -> do Output b' y' <- stepYarn y dt b
                                     return $ Output (b', d) (first y')

instance Monad m => Applicative (Yarn m a) where
    pure = pureYarn . const
    wf <*> wa = Yarn $ \dt a -> do Output f wf' <- stepYarn wf dt a
                                   Output b wa' <- stepYarn wa dt a
                                   return $ Output (f b) (wf' <*> wa')

instance (Monad m, Floating b) => Floating (Yarn m a b) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin; sinh = fmap sinh; asin = fmap asin; asinh = fmap asinh
    cos = fmap cos; cosh = fmap cosh; acos = fmap acos; acosh = fmap acosh
    atan = fmap atan; atanh = fmap atanh

instance (Monad m, Fractional b) => Fractional (Yarn m a b) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance (Monad m, Num b) => Num (Yarn m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Monad m => Category (Yarn m) where
    id  = pureYarn id
    (.) = (<~)

instance Monad m => Functor (Yarn m a) where
    fmap f (Yarn w) = Yarn $ \dt a -> liftM (fmap f) $ w dt a

instance Monad m => Functor (Output m a) where
    fmap f (Output val w) = Output (f val) (fmap f w)
--------------------------------------------------------------------------------
-- Composing Yarn
--------------------------------------------------------------------------------
-- | Plugs the output value of w2 into the input value of w1.
(<~) :: Monad m => Yarn m b c -> Yarn m a b -> Yarn m a c
(<~) = flip (~>)
infixl 1 <~

-- | Plugs the output value of w1 into the input value of w2.
(~>) :: Monad m => Yarn m a b -> Yarn m b c -> Yarn m a c
(~>) w1 w2 = Yarn $ \dt a -> do Output b w1' <- stepYarn w1 dt a
                                Output c w2' <- stepYarn w2 dt b
                                return $ Output c $ w1' ~> w2'
infixr 1 ~>
--------------------------------------------------------------------------------
-- Constructing pure Yarn
--------------------------------------------------------------------------------
yarn :: Monad m => (a -> b) -> Yarn m a b
yarn = pureYarn

pureYarn :: Monad m => (a -> b) -> Yarn m a b
pureYarn f = valYarn $ \a -> Output (f a) (pureYarn f)

valYarn :: Monad m => (a -> Output m a b) -> Yarn m a b
valYarn f = timeYarn $ \_ a -> f a

timeYarn :: Monad m => (Time -> a -> Output m a b) -> Yarn m a b
timeYarn f = Yarn $ \dt a -> return $ f dt a
--------------------------------------------------------------------------------
-- Constructing effectful Yarn
--------------------------------------------------------------------------------
yarnM :: Monad m => m b -> Yarn m a b
yarnM = valYarnM . const

valYarnM :: Monad m => (a -> m b) -> Yarn m a b
valYarnM = timeYarnM . const

timeYarnM :: Monad m => (Time -> a -> m b) -> Yarn m a b
timeYarnM f = Yarn $ \dt a -> do b <- f dt a
                                 return $ Output b $ timeYarnM f
--------------------------------------------------------------------------------
-- Help inspecting Yarn balls
--------------------------------------------------------------------------------
traceYarn :: (Show a, Monad m) => Yarn m a a
traceYarn = traceYarnWith ""

traceYarnWith :: (Show a, Monad m) => String -> Yarn m a a
traceYarnWith str = yarn $ \a -> trace (str ++ show a) a

testYarn :: Show b => Yarn IO () b -> IO b
testYarn = testYarnTill 1000000000

testYarnTill :: Show b => Time -> Yarn IO () b -> IO b
testYarnTill secs w = do
    t <- getCurrentTime
    loopYarn t 0 w
        where loopYarn t' tt w' = do t'' <- getCurrentTime
                                     let dt = realToFrac $ diffUTCTime t'' t'
                                     Output b w'' <- stepYarn w' dt ()
                                     print b
                                     if secs <= (tt + dt)
                                     then do putStrLn "Last value: "
                                             return b
                                     else loopYarn t'' (tt + dt) w''
--------------------------------------------------------------------------------
-- Running Yarn
--------------------------------------------------------------------------------
execYarn :: (Monad m, Functor m) => Yarn m a b -> Time -> a -> m b
execYarn w dt a = fmap outVal $ stepYarn w dt a

stepYarn :: Monad m => Yarn m a b -> Time -> a -> m (Output m a b)
stepYarn (Yarn w) dt a = w dt a
--------------------------------------------------------------------------------
-- Yarn basics
--------------------------------------------------------------------------------
time :: Monad m => Yarn m a Time
time = timeFrom 0

timeFrom :: Monad m => Double -> Yarn m a Time
timeFrom t = Yarn $ \dt _ ->
    let t' = t + dt in
    return $ Output t' (timeFrom t')

data Output m a b = Output { outVal  :: b
                           , outYarn :: (Yarn m a b)
                           }

data Yarn m a b = Yarn (Time -> a -> m (Output m a b))

type Time = Double
