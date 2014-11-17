module Yarn.Event (
    Event,
    -- * Combining events and values
    orE,
    -- * Generating events from values
    triggerTrue,
    triggerJust,
    triggerUnique,
    -- * Using event values
    collect,
    hold,
    holdWith,
    startingWith,
    startWith,
    -- * Time based events
    after,
    before,
    forever,
    never,
    -- * Switching events
    andThen,
    andThenWith,
    untilE,
    untilWithE
) where

import Yarn.Core
import Control.Applicative
--------------------------------------------------------------------------------
-- Combining value Yarn and event Yarn
--------------------------------------------------------------------------------
-- | Produces values from the first Yarn unless the second produces event
-- values and if so, produces the values of those events.
orE :: Monad m => Yarn m a b -> Yarn m a (Event b) -> Yarn m a b
orE y ye = Yarn $ \dt a -> do
    Output b y'  <- stepYarn y dt a
    Output e ye' <- stepYarn ye dt a
    return $ case e of
        NoEvent  -> Output b $ orE y' ye'
        Event b' -> Output b' $ orE y' ye'
--------------------------------------------------------------------------------
-- Generating events from values
--------------------------------------------------------------------------------
triggerTrue :: Monad m => Yarn m Bool (Event ())
triggerTrue = yarn $ \b -> if b then Event () else NoEvent

triggerJust :: Monad m => Yarn m (Maybe a) (Event a)
triggerJust = yarn $ \ma -> case ma of
                                 Nothing -> NoEvent
                                 Just a  -> Event a

triggerUnique :: (Monad m, Eq a) => Yarn m a (Event a)
triggerUnique = trigger NoEvent
    where trigger NoEvent   = valYarn $ \a -> Output (Event a) (trigger $ Event a)
          trigger (Event a) = valYarn $ \a' -> let e = if a == a'
                                                       then Event a
                                                       else Event a'
                                               in Output e (trigger e)
--------------------------------------------------------------------------------
-- Using event values
--------------------------------------------------------------------------------
collect :: Monad m => Yarn m (Event a) [a]
collect = valYarn $ collect' []
    where collect' es e = let es' = case e of
                                        NoEvent -> es
                                        Event a -> a:es
                          in Output es' (valYarn $ collect' es')

-- | Produces the given value until the input events produce a value, then
-- produce that value until a new input event produces. This always holds
-- the last produced value, starting with the given value.
-- @
-- after 3 ~> startWith 0
startingWith, startWith :: Monad m => a -> Yarn m (Event a) a
startingWith = startWith
startWith a = valYarn $ \ma ->
    case ma of
        NoEvent  -> Output a (startWith a)
        Event a' -> Output a' (startWith a')

holdWith :: Monad m => b -> Yarn m a (Event b) -> Yarn m a b
holdWith = flip hold

hold :: Monad m => Yarn m a (Event b) -> b -> Yarn m a b
hold w initial = Yarn $ \dt x -> do
    Output mb w' <- stepYarn w dt x
    return $ case mb of
        NoEvent -> Output initial $ hold w' initial
        Event e -> Output e $ hold w' e
--------------------------------------------------------------------------------
-- Time based events
--------------------------------------------------------------------------------
-- | Produces events of input values until t seconds have passed.
-- Note that as soon as the Yarn has run for a number of seconds >= t it stops
-- streaming events so the actual value of the Yarn at t is not guaranteed to
-- be produced.
before :: (RealFloat t, Monad m) => t -> Yarn m a (Event a)
before t = timeYarn $ \dt a ->
    if t - dt > 0
    then Output (Event a) (before $ t - dt)
    else Output NoEvent never

-- | Produces events of input values after t seconds.
-- Note that the Yarn is not guaranteed to start producing exactly at t,
-- only at some very small dt after t, so the value exactly at t most
-- likely will not be produced. You can see this by testing the Yarn:
-- @
-- time ~> after 2
-- @
-- which produces 2.000015999995771 as its first
-- value
after :: (RealFloat t, Monad m) => t -> Yarn m a (Event a)
after t = timeYarn $ \dt a ->
    if t - dt <= 0
    then Output (Event a) forever
    else Output NoEvent (after $ t - dt)

-- | Never produces any event values.
never :: Monad m => Yarn m a (Event a)
never = pure NoEvent

-- | Produces events of input values forever, always.
forever :: Monad m => Yarn m a (Event a)
forever = valYarn $ \a -> Output (Event a) forever
--------------------------------------------------------------------------------
-- Switching Yarn on events
--------------------------------------------------------------------------------
-- | Produces an Event's values until it stops producing, then
-- switches to the second Yarn.
andThen :: Monad m => Yarn m a (Event b) -> Yarn m a b -> Yarn m a b
andThen w1 w2 = w1 `andThenWith` const w2

-- | Produces an event stream of the first Yarn's values until the second
-- starts producing, then produces an event stream of the second.
untilE :: Monad m => Yarn m a b -> Yarn m a (Event b) -> Yarn m a (Event b)
untilE w1 w2 = Yarn $ \dt a -> do
    Output mb w2' <- stepYarn w2 dt a
    case mb of
        NoEvent -> do Output b w1' <- stepYarn w1 dt a
                      return $ Output (Event b) (w1' `untilE` w2')
        _  -> return $ Output mb w2'

untilWithE :: Monad m => Yarn m a b -> Yarn m a (Event c) -> (c -> Yarn m a b)
           -> Yarn m a b
untilWithE y ey f = Yarn $ \dt a -> do
    Output mb ey' <- stepYarn ey dt a
    case mb of
        NoEvent -> do Output b y' <- stepYarn y dt a
                      return $ Output b ((y' `untilWithE`) ey' f)
        Event b -> stepYarn (f b) dt a

andThenWith :: Monad m => Yarn m a (Event b) -> (Maybe b -> Yarn m a b) -> Yarn m a b
andThenWith = go Nothing
    where go mb w1 f = Yarn $ \dt a -> do
              Output e w1' <- stepYarn w1 dt a
              case e of
                  NoEvent -> stepYarn (f mb) dt a
                  Event b -> return $ Output b $ go (Just b) w1' f

instance Show a => Show (Event a) where
    show (Event a) = "Event " ++ show a
    show NoEvent   = "NoEvent"

instance (Floating a) => Floating (Event a) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin; sinh = fmap sinh; asin = fmap asin; asinh = fmap asinh
    cos = fmap cos; cosh = fmap cosh; acos = fmap acos; acosh = fmap acosh
    atan = fmap atan; atanh = fmap atanh

instance (Fractional a) => Fractional (Event a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Num a => Num (Event a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Alternative Event where
    empty = NoEvent
    (<|>) (Event e) _ = Event e
    (<|>) NoEvent e = e

instance Applicative Event where
    pure = Event
    (<*>) (Event f) (Event a) = Event $ f a
    (<*>) _ _ = NoEvent

instance Functor Event where
    fmap f (Event a) = Event $ f a
    fmap _ NoEvent = NoEvent

data Event a = Event a | NoEvent
