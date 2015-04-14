module Yarn.Event (
    Event,
    -- * Transforming event values.
    toMaybe,
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
    -- * Switching and chaining events
    andThen,
    andThenWith,
    andThenE,
    untilE,
    untilWithE
) where

import Yarn.Core
import Control.Applicative
--------------------------------------------------------------------------------
-- Transforming event values into usable values.
--------------------------------------------------------------------------------
toMaybe :: Event a -> Maybe a
toMaybe (Event a) = Just a
toMaybe _ = Nothing
--------------------------------------------------------------------------------
-- Combining value yarn and event yarn
--------------------------------------------------------------------------------
-- | Produces values from the first yarn unless the second produces event
-- values and if so, produces the values of those events.
orE :: Monad m => Yarn t m a b -> Yarn t m a (Event b) -> Yarn t m a b
orE y ye = Yarntaom $ \dt a -> do
    Output b y'  <- stepYarn y dt a
    Output e ye' <- stepYarn ye dt a
    return $ case e of
        NoEvent  -> Output b $ orE y' ye'
        Event b' -> Output b' $ orE y' ye'
--------------------------------------------------------------------------------
-- Generating events from values
--------------------------------------------------------------------------------
-- | Triggers an `Event ()` when the input value is True.
triggerTrue :: Monad m => Yarn t m Bool (Event ())
triggerTrue = Yarn $ \b -> if b then Event () else NoEvent

-- | Triggers an `Event a` when the input is `Just a`.
triggerJust :: Monad m => Yarn t m (Maybe a) (Event a)
triggerJust = Yarn $ \ ma -> case ma of
                                 Nothing -> NoEvent
                                 Just a  -> Event a

-- | Triggers an `Event a` when the input is a unique value.
triggerUnique :: (Monad m, Eq a) => Yarn t m a (Event a)
triggerUnique = trigger NoEvent
    where trigger NoEvent   = Yarntao $ \_ a -> Output (Event a) (trigger $ Event a)
          trigger (Event a) = Yarntao $ \_ a' -> let e = if a == a'
                                                         then Event a
                                                         else Event a'
                                                 in Output e (trigger e)
--------------------------------------------------------------------------------
-- Using event values
--------------------------------------------------------------------------------
-- | Collect all produced values into a monoidal structure using the given
-- insert function.
collectWith :: (Monoid b, Monad m) => (a -> b -> b) -> Yarn t m (Event a) b
collectWith f = Yarntao $ \_ a -> collect' mempty a
    where collect' b e = let b' = case e of
                                        NoEvent -> b
                                        Event a' -> f a' b
                          in Output b' (Yarntao $ \_ a' -> collect' b' a')

-- | Collect all produced values into a list.
collect :: Monad m => Yarn t m (Event a) [a]
collect = collectWith (:)

-- | Produces the given value until the input events produce a value, then
-- produce that value until a new input event produces. This always holds
-- the last produced value, starting with the given value.
-- @
-- after 3 ~> startWith 0
-- @
-- This is similar to `hold` except that it takes events from its input value
-- instead of another yarn.
startingWith, startWith :: Monad m => a -> Yarn t m (Event a) a
startingWith = startWith
startWith a = Yarntao $ \_ ma ->
    case ma of
        NoEvent  -> Output a (startWith a)
        Event a' -> Output a' (startWith a')

-- | Flipped version of `hold`.
holdWith :: Monad m => b -> Yarn t m a (Event b) -> Yarn t m a b
holdWith = flip hold

-- | Produces the `initial` value until the given yarn produces an event.
-- After an event is produced that event's value will be produced until the
-- next event produced by the given yarn.
hold :: Monad m => Yarn t m a (Event b) -> b -> Yarn t m a b
hold w initial = Yarntaom $ \dt x -> do
    Output mb w' <- stepYarn w dt x
    return $ case mb of
        NoEvent -> Output initial $ hold w' initial
        Event e -> Output e $ hold w' e
--------------------------------------------------------------------------------
-- Time based events
--------------------------------------------------------------------------------
-- | Produces events of input values until t seconds have passed.
-- Note that as soon as the yarn has run for a number of seconds >= t it stops
-- streaming events so the actual value of the yarn at t is not guaranteed to
-- be produced.
before :: (Num t, Ord t) => t -> Yarn t m a (Event a)
before t = Yarntao $ \dt a ->
    if t - dt > 0
    then Output (Event a) (before $ t - dt)
    else Output NoEvent never

-- | Produces events of input values after t seconds.
-- Note that the yarn is not guaranteed to start producing exactly at t,
-- only at some delta after t, so the value exactly at t most likely will not
-- be produced. You can see this by testing the yarn:
-- @
-- testYarn $ time ~> after 2
-- @
-- which produces ~2.000015999995771 as its first value
after :: (Num t, Ord t) => t -> Yarn t m a (Event a)
after t = Yarntao $ \dt a ->
    if t - dt <= 0
    then Output (Event a) forever
    else Output NoEvent (after $ t - dt)

-- | Never produces any event values.
never :: Yarn t m a (Event a)
never = Yarn $ const NoEvent

-- | Produces events of input values forever, always.
forever :: Yarn t m a (Event a)
forever = Yarn $ \a -> Event a
--------------------------------------------------------------------------------
-- Switching yarn on events
--------------------------------------------------------------------------------
-- | Produces the first yarn's Event values until that stops producing, then
-- switches to the second yarn.
andThen :: Monad m => Yarn t m a (Event b) -> Yarn t m a b -> Yarn t m a b
andThen w1 w2 = w1 `andThenWith` const w2

-- | Produces the first yarn's Event values until that stops producing,
-- then switches to the second yarn's Event values.
andThenE :: Monad m => Yarn t m a (Event b) -> Yarn t m a (Event b) -> Yarn t m a (Event b)
andThenE y1 y2 = Yarntaom $ \dt a -> do
    Output e y1' <- stepYarn y1 dt a
    case e of
        NoEvent -> stepYarn y2 dt a
        Event b -> return $ Output (Event b) (y1' `andThenE` y2)

-- | Produces a Event values of the first yarn's values until the second
-- yarn produces an Event value, then produces Event values of the second.
untilE :: Monad m => Yarn t m a b -> Yarn t m a (Event b) -> Yarn t m a (Event b)
untilE w1 w2 = Yarntaom $ \dt a -> do
    Output mb w2' <- stepYarn w2 dt a
    case mb of
        NoEvent -> do Output b w1' <- stepYarn w1 dt a
                      return $ Output (Event b) (w1' `untilE` w2')
        _  -> return $ Output mb w2'

untilWithE :: Monad m => Yarn t m a b -> Yarn t m a (Event c) -> (c -> Yarn t m a b)
           -> Yarn t m a b
untilWithE y ey f = Yarntaom $ \dt a -> do
    Output mb ey' <- stepYarn ey dt a
    case mb of
        NoEvent -> do Output b y' <- stepYarn y dt a
                      return $ Output b ((y' `untilWithE`) ey' f)
        Event b -> stepYarn (f b) dt a

andThenWith :: Monad m => Yarn t m a (Event b) -> (Maybe b -> Yarn t m a b) -> Yarn t m a b
andThenWith = go Nothing
    where go mb w1 f = Yarntaom $ \dt a -> do
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
