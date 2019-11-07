module NotThis where

import Control.Applicative

(>>>=) :: Maybe x -> (x -> Maybe y) -> Maybe y
Nothing >>>= _ = Nothing
Just a >>>= f = f a

infixl 1 >>>=

lift0Maybe :: a -> Maybe a
lift0Maybe = Just

exercise0 :: (a -> Maybe b) -> [a] -> Maybe [b]
exercise0 f list =
  case list of
    [] -> lift0Maybe []
    h:t ->
      f h >>>= \b ->
      exercise0 f t >>>= \bs ->
      lift0Maybe (b:bs)

-- Monad
class Hi k where
  (>>>>>>>>=) :: k x -> (x -> k y) -> k y
  lift0 :: a -> k a

(<<*>>) :: Hi k => k (x -> y) -> k x -> k y
(<<*>>) f x =
  f >>>>>>>>= \f' ->
  x >>>>>>>>= \x' ->
  lift0 (f' x')

(<<$>>) :: Hi k => (x -> y) -> k x -> k y
(<<$>>) f x =
  lift0 f <<*>> x

lawleftIdentity x = (>>>>>>>>=) x lift0 == x

instance Hi ((->) t) where
  (>>>>>>>>=) t2x x2t2y t = x2t2y (t2x t) t
  lift0 = const

instance Hi Maybe where
  Nothing >>>>>>>>= _ = Nothing
  Just a >>>>>>>>= f = f a
  lift0 = Just

instance Hi [] where
  list >>>>>>>>= f = foldr ((++) . f) [] list
  lift0 a = [a]

-- Applicative k => ((->) t (k a)) -> (->) [t] (k [a])
--                  (p t (k a)) -> p [t] (k [a])
--                  (p a (k b)) -> p s (k t)
--                  (p a (k b)) -> p s (k t)

exerciseN :: Applicative k => (t -> k a) -> [t] -> k [a]
exerciseN f list = 
  case list of
    [] -> pure []
    h:t -> 
      liftA2 (:) (f h) (exerciseN f t) 
      {-
      f h >>>>>>>>= \b ->
      exerciseN f t >>>>>>>>= \bs ->
      lift0 (b:bs)
      -}

(>>>>=) :: (t -> x) -> (x -> t -> y) -> t -> y
(>>>>=) t2x x2t2y t = x2t2y (t2x t) t

lift0Reader :: a -> t -> a
lift0Reader = const

exercise1 :: (a -> t -> b) -> [a] -> (t -> [b])
exercise1 f list = 
    case list of
      [] -> lift0Reader []
      h:t ->
        f h >>>>= \b ->
        exercise1 f t >>>>= \bs ->
        lift0Reader (b:bs)

(>>>>>=) :: [x] -> (x -> [y]) -> [y]
(>>>>>=) list f = foldr ((++) . f) [] list

lift0List :: a -> [a]
lift0List a = [a]

exercise2 :: (a -> [b]) -> [a] -> [[b]]
exercise2 f list = 
  case list of
    [] -> lift0List []
    h:t ->
      f h >>>>>= \b ->
      exercise2 f t >>>>>= \bs ->
      lift0List (b:bs)

-- Parser a ~ ((Int, Int, String) -> Maybe (Int, Int, String, a))
newtype FluffyThings k a = FluffyThings (Int -> k (Int, a))

runFluffyThings :: FluffyThings k a -> Int -> k (Int, a)
runFluffyThings (FluffyThings x) = x

instance Functor k => Functor (FluffyThings k) where
  fmap =
    \f -> \(FluffyThings x) -> FluffyThings (fmap (fmap f) . x)

instance Monad k => Applicative (FluffyThings k) where
  pure = \a -> FluffyThings (\n -> pure (n, a))
  (<*>) = \(FluffyThings f) -> \(FluffyThings a) ->
    FluffyThings (\n ->
      do  {
            (o, ff) <- f n;
            (p, aa) <- a o;
            return (p, ff aa);
          }
    )

instance Monad k => Monad (FluffyThings k) where
  return = pure
  (>>=) = \(FluffyThings x) -> \f ->
    FluffyThings (\n ->
      x n >>= \(o, a) ->
      runFluffyThings (f a) o
    )

updateCounter :: Applicative k => (Int -> Int) -> FluffyThings k ()
updateCounter k =
  FluffyThings (\n -> pure (k n, ()))

failFluffyThings :: FluffyThings Maybe a
failFluffyThings =
  FluffyThings (pure Nothing)

-- remove all newlines, but keep count of them
-- construct the string without the newlines
-- if a tab character is seen, stop counting and fail
countLinesNoTabs :: String -> FluffyThings Maybe String
countLinesNoTabs [] =
  pure []
countLinesNoTabs (h:t) =
  case h of
    '\t' ->
      failFluffyThings
    '\n' ->
      do  updateCounter (+1)
          countLinesNoTabs t
    _ ->
      do  r <- countLinesNoTabs t
          pure (h:r)




-- f :: Int -> Maybe (Int, a -> b)
-- a :: Int -> Maybe (Int, a)
-- n :: Int
-- _ :: Maybe (Int, b)

blah :: Maybe x -> (x -> Maybe y) -> Maybe y
blah = \m f ->
  case m of
    Nothing -> Nothing
    Just x -> f x

blah2 x =
  do  c <- readFile x
      length c `seq` writeFile x (reverse c) 

-- \x -> f (g x)
-- f . g

-- f :: a -> b
-- x :: Int -> Maybe (Int, a)
-- n :: Int
-- _ :: Maybe (Int, b)


-- b ::   b
-- bs :: [b]
-- _ ::  [b]



            -- h :: a
            -- t :: [a]
            -- f :: (a -> Maybe b)

-- f h           :: Maybe b
-- exercise0 f t :: Maybe [b]
-- _             :: Maybe [b]
