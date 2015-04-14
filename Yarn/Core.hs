{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Yarn.Core where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Arrow
import Data.Time.Clock
import Data.Typeable
import Debug.Trace

--------------------------------------------------------------------------------
-- Yarn instances
--------------------------------------------------------------------------------
instance Monad m => Arrow (Yarn t m) where
    arr = Yarn
    first (Yarn y) = Yarn $ \(b,d) -> (y b, d)
    first y = Yarntaom $ \dt (b,d) ->
                  stepYarn y dt b >>= \(Output c y') ->
                      return $ Output (c, d) (first y')

instance Monad m => Applicative (Yarn t m a) where
    pure = Yarn . const
    wf <*> wa = Yarntaom $ \dt a ->
                    stepYarn wf dt a >>= \(Output f wf') ->
                        stepYarn wa dt a >>= \(Output b wa') ->
                                   return $ Output (f b) (wf' <*> wa')

instance (Monad m, Floating b) => Floating (Yarn t m a b) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin; sinh = fmap sinh; asin = fmap asin; asinh = fmap asinh
    cos = fmap cos; cosh = fmap cosh; acos = fmap acos; acosh = fmap acosh
    atan = fmap atan; atanh = fmap atanh

instance (Monad m, Fractional b) => Fractional (Yarn t m a b) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance (Monad m, Num b) => Num (Yarn t m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Monad m => Category (Yarn t m) where
    id  = Yarn id
    (.) = (<~)

instance Monad m => Functor (Yarn t m a) where
    fmap f (Yarn y) = Yarn $ f . y
    fmap f (Yarnt y) = Yarnt $ f . y
    fmap f (Yarnta y) = Yarnta $ \dt a -> f $ y dt a
    fmap f (Yarntao y) = Yarntao $ \dt a -> fmap f $ y dt a
    fmap f (Yarntaom y) = Yarntaom $ \dt a -> liftM (fmap f) $ y dt a

instance Monad m => Functor (Output t m a) where
    fmap f (Output val w) = Output (f val) (fmap f w)
--------------------------------------------------------------------------------
-- Feedback
--------------------------------------------------------------------------------
-- Delays the yarn by one step using the given value as the current step's
-- produced value.
delayWith :: Monad m => Yarn t m a b -> b -> Yarn t m a b
delayWith y start = Yarntao $ \dt a -> Output start $ delay dt a y

-- | Delays a yarn by one step using the given time and value as the first
-- inputs.
delay :: Monad m => t -> a -> Yarn t m a b -> Yarn t m a b
delay dt a y = Yarntaom $ \dt' a' -> {-# SCC "delay-1" #-}stepYarn y dt a >>=
    {-# SCC "delay-2" #-}\(Output b y') ->
        {-# SCC "delay-3" #-}return $ Output b $ delay dt' a' y'

--delayWith b y =
--    let go dt' a' y' = yarnM $ \dt'' a'' -> do
--                              Output b' y'' <- stepYarn y' dt' a'
--                              return $ Output b' $ go dt'' a'' y''
--    in yarnM $ \dt a -> return $ Output b $ go dt a y
--------------------------------------------------------------------------------
-- Composing Yarn
--------------------------------------------------------------------------------
-- | Plugs the output value of w2 into the input value of w1.
(<~) :: Monad m => Yarn t m b c -> Yarn t m a b -> Yarn t m a c
(<~) = flip (~>)
infixl 1 <~

-- | Plugs the output value of w1 into the input value of w2.
(~>) :: Monad m => Yarn t m a b -> Yarn t m b c -> Yarn t m a c
(~>) (Yarn y1) (Yarn y2) = {-# SCC "pure~>" #-} Yarn $ y2 . y1
(~>) y1 y2 = Yarntaom $ \dt a -> do
    Output b y1' <- stepYarn y1 dt a
    Output c y2' <- stepYarn y2 dt b
    return $ Output c $ y1' ~> y2'
infixr 1 ~>
--------------------------------------------------------------------------------
-- Help inspecting Yarn balls
--------------------------------------------------------------------------------
-- | A yarn that traces its input when stepped and outputs that value.
traceYarn :: (Show a, Monad m) => Yarn t m a a
traceYarn = traceYarnWith ""

-- | A yarn that traces its input along with an identifying string and
-- outputs the input value.
traceYarnWith :: (Show a, Monad m) => String -> Yarn t m a a
traceYarnWith str = Yarn $ \a -> trace (str ++ show a) a

-- | Tests a yarn in an IO loop for a long time.
testYarn :: Show b => Yarn Double IO () b -> IO b
testYarn = testYarnTill 1000000000

-- | Tests a yarn in an IO loop for the given number of seconds.
testYarnTill :: Show b => Double -> Yarn Double IO () b -> IO b
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

-- | Tests a yarn step by step in a user controlled IO loop.
stepTestYarn :: (Show b) => Yarn Double IO () b -> IO ()
stepTestYarn y = do
    putStr "\ntime delta: "
    t <- getLine
    let dt = read t :: Double
    Output b y' <- stepYarn y dt ()
    putStrLn $ show b
    stepTestYarn y'
--------------------------------------------------------------------------------
-- Running Yarn
--------------------------------------------------------------------------------
-- | Evaluate the given yarn once with the given time and input value.
-- Throws away the resulting output yarn and keeps the resulting output value.
evalYarn :: (Monad m, Functor m, RealFloat t) => Yarn t m a b -> t -> a -> m b
evalYarn w dt a = fmap outVal $ stepYarn w dt a

-- | Execute the given yarn once with the given time and input value.
-- Throws away the resulting output yarn and keeps the resulting output value.
execYarn :: (Monad m, Functor m, RealFloat t) => Yarn t m a b -> t -> a
         -> m (Yarn t m a b)
execYarn w dt a = fmap outYarn $ stepYarn w dt a

-- | Step the given yarn by a discreet time step and a given input value.
stepYarn :: Monad m => Yarn t m a b -> t -> a -> m (Output t m a b)
stepYarn (Yarn y) _ a = return $ Output (y a) $ Yarn y
stepYarn (Yarnt y) dt _ = return $ Output (y dt) $ Yarnt y
stepYarn (Yarnta y) dt a = return $ Output (y dt a) $ Yarnta y
stepYarn (Yarntao y) dt a = return $ y dt a
stepYarn (Yarntaom y) dt a = y dt a
--------------------------------------------------------------------------------
-- Yarn basics
--------------------------------------------------------------------------------
constant :: Monad m => b -> Yarn t m a b
constant = arr . const

-- | A yarn that outputs time elapsed 0.
time :: (Num t, Monad m) => Yarn t m a t
time = timeFrom 0

-- | A yarn that outputs time elapsed since the given time.
timeFrom :: (Num t, Monad m) => t -> Yarn t m a t
timeFrom t = Yarntao $ \dt _ ->
    let t' = t + dt in Output t' (timeFrom t')

-- | A result type of a discreet time step. A yarn produces this type which
-- encodes the value at that step as well as a yarn for the next step.
data Output t m a b = Output { outVal  :: !b
                             , outYarn :: (Yarn t m a b)
                             }

-- | Various yarn constructors.
data Yarn t m a b where
    Yarn     :: (a -> b) -> Yarn t m a b
    Yarnt    :: (t -> b) -> Yarn t m a b
    Yarnta   :: (t -> a -> b) -> Yarn t m a b
    Yarntao  :: (t -> a -> Output t m a b) -> Yarn t m a b
    Yarntaom :: (t -> a -> m (Output t m a b)) -> Yarn t m a b
    deriving (Typeable)
