{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module Reflex.View.Base where

import Control.Lens
import Control.Monad.Reader
import Data.Coerce
import Data.Constraint
import Data.Constraint.Forall
import Data.Dependent.Sum (DSum (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Maybe
import Data.GADT.Compare (GCompare, GEq (..), (:~:) (..))
import Data.Semigroup
import Data.These
import Reflex.Dom
import Reflex.Class
import Reflex.Dynamic
import Reflex.EventWriter

-- | A Dynamic that is known to be ready to be sampled
newtype StrictDynamic x t a = StrictDynamic { unStrictDynamic :: Dynamic t a }

deriving instance Reflex t => Functor (StrictDynamic x t)
deriving instance Reflex t => Applicative (StrictDynamic x t)
deriving instance Reflex t => Monad (StrictDynamic x t)

newtype SampleStrictT x m a = SampleStrictT { unSampleStrictT :: m a }
  deriving (Functor, Applicative, Monad)

class SampleStrict x m where
  sampleStrict :: StrictDynamic x t a

withFoldDynMaybeM :: (Reflex t, MonadHold t m, MonadFix m) => (cmd -> v -> PushM t (Maybe v)) -> v -> Event t cmd -> (forall x. StrictDynamic x t v -> SampleStrictT x m a) -> m a
withFoldDynMaybeM f z e a = do
  d <- foldDynMaybeM f z e
  unSampleStrictT $ a $ StrictDynamic d

{-
class F a b where {}

instance F (Either a b) (Either (Dynamic t a) (Dynamic t b)) where {}

class where
  factorDynamic :: Dynamic (f a) -> Dynamic (f (Dynamic a))

myWidget :: (Show b) => Dynamic t (Either String (Either b c)) -> m (Dynamic t ())
myWidget v = factorView v $ \case
  Left (D da) -> dynText da
  Right (Left (D db)) -> display db
  Right (Right (D dc)) -> display dc
-}

-- | IMPORTANT: The state 'Dynamic' must always be sampleable
--TODO: Split the ReaderT part of this into a "strict dynamic" monad
--TODO: newtype this
type ViewT t s c m a = ReaderT (Dynamic t s) (EventWriterT t c m) a


--TODO: Probably shouldn't return Event
factorWidget :: (MonadWidget t m, GEq k) => Dynamic t (DSum k v) -> (DSum k (Compose (Dynamic t) v) -> m a) -> m (Event t a)
factorWidget d w = do
  d' <- factorDyn d
  dyn $ fmap w d'

askModel :: Monad m => ViewT t s c m (Dynamic t s)
askModel = ask

modelDyn :: (Reflex t, MonadSample t m, MonadHold t m, Adjustable t m, Semigroup c) => (s -> ViewT t s c m a) -> ViewT t s c m (Dynamic t a)
modelDyn f = do
  m <- askModel
  strictDynWidget m f

--TODO: Should these be patches?
data ViewMorphism s s' c c' = ViewMorphism
  { _viewMorphism_mapCommand :: c -> c'
  , _viewMorphism_mapState :: s' -> s
  }

viewEitherToDSum :: ViewMorphism (DSum (EitherTag a b) Identity) (Either a b) (Endo (DSum (EitherTag a b) Identity)) (Endo (Either a b))
viewEitherToDSum = ViewMorphism
  { _viewMorphism_mapCommand = \(Endo c) -> Endo $ dsumToEither . c . eitherToDSum
  , _viewMorphism_mapState = eitherToDSum
  }

commandMorphism :: (c -> c') -> ViewMorphism s s c c'
commandMorphism f = ViewMorphism
  { _viewMorphism_mapCommand = f
  , _viewMorphism_mapState = id
  }

composeViewMorphisms :: ViewMorphism s s' c' c'' -> ViewMorphism s' s'' c c' -> ViewMorphism s s'' c c''
composeViewMorphisms (ViewMorphism c' s) (ViewMorphism c s') = ViewMorphism (c' . c) (s . s')

dmapToEndo :: GCompare k => AppendDMap k Endo -> Endo (DSum k Identity)
dmapToEndo (AppendDMap m) = Endo $ \kv@(k :=> Identity v) -> case DMap.lookup k m of
  Nothing -> kv
  Just (Endo f) -> k :=> Identity (f v)

-- | IMPORTANT: The state 'Dynamic' must always be sampleable
runViewT :: (Reflex t, Monad m, Semigroup c) => ViewT t s c m a -> Dynamic t s -> m (a, Event t c)
runViewT v s = runEventWriterT $ runReaderT v s

execViewT :: (Reflex t, Monad m, Semigroup c) => ViewT t s c m a -> Dynamic t s -> m (Event t c)
execViewT v s = snd <$> runViewT v s

mapViewT :: (Reflex t, Semigroup c, Semigroup c', MonadHold t m, MonadFix m) => ViewMorphism s' s c' c -> ViewT t s' c' m a -> ViewT t s c m a
mapViewT (ViewMorphism g f) = withReaderT (fmap f) . mapReaderT (withEventWriterT g)

newtype AppendDMap k v = AppendDMap { unAppendDMap :: DMap k v }

instance (GCompare k, ForallF Semigroup v) => Semigroup (AppendDMap k v) where
  AppendDMap a <> AppendDMap b = AppendDMap $ DMap.unionWithKey f a b
    where f :: k a -> v a -> v a -> v a
          f (_ :: k a) = case instF @Semigroup @v @a of
            Sub Dict -> (<>)

instance (GCompare k, ForallF Semigroup v) => Monoid (AppendDMap k v) where
  mappend = (<>)
  mempty = AppendDMap mempty
  mconcat ms = AppendDMap $ DMap.unionsWithKey f $ coerce ms
    where f :: k a -> v a -> v a -> v a
          f (_ :: k a) = case instF @Semigroup @v @a of
            Sub Dict -> (<>)

-- | WARNING: The input 'Dynamic' must be fully constructed when this is run
strictDynWidget :: (Reflex t, MonadSample t m, MonadHold t m, Adjustable t m) => Dynamic t a -> (a -> m b) -> m (Dynamic t b)
strictDynWidget a f = do
  a0 <- sample $ current a
  (result0, result') <- runWithReplace (f a0) $ f <$> updated a
  holdDyn result0 result'

strictDynWidget_ :: (Reflex t, MonadSample t m, MonadHold t m, Adjustable t m) => Dynamic t a -> (a -> m ()) -> m ()
strictDynWidget_ a f = do
  a0 <- sample $ current a
  (result0, result') <- runWithReplace (f a0) $ f <$> updated a
  pure ()

--TODO: How do we capture that the command being sent back will always match the current state?
factorViewT
  :: forall t m k s c v.
     ( Reflex t
     , Monad m
     , MonadFix m
     , MonadHold t m
     , ForallF Semigroup c
     , Adjustable t m
     , GCompare k
     )
  => (forall a. k a -> ViewT t (s a) (c a) m (v a))
  -> ViewT t (DSum k s) (AppendDMap k c) m (Dynamic t (DSum k v))
factorViewT v = do
  s <- factorDyn =<< ask
  strictDynWidget s $ \((k :: k a) :=> Compose s) ->
    case instF @Semigroup @c @a of
      Sub Dict -> do
        (v, c) <- lift $ lift $ runViewT (v k) s
        tellEvent $ fmapCheap (AppendDMap . DMap.singleton k) c
        return $ k :=> v

-- Separate routing concerns:
--   Parsing/unparsing
--   StrictDynamic (being able to write Dynamic t (m a) -> m (Dynamic t a))
--   Jumping to links
--     Via EventWriter
--     Via clicked links
--   Reasonable case syntax for breaking down dynamics
--     factorDynPure
--       unsafeLiftPushM :: PushM t a -> PullM t a

unsafeLiftPushM :: Reflex t => PushM t a -> PullM t a
unsafeLiftPushM = undefined

f :: forall t k (n :: (whatever -> *) -> *) v. (Reflex t, n ~ DSum k, GEq k) => Dynamic t (n v) -> Dynamic t (n (Compose (Dynamic t) v))
f d =
  let inner :: k a -> v a -> PushM t (Dynamic t (v a))
      inner k v0 = holdDyn v0 . fmapMaybe id =<< takeWhileE isJust newVal
        where newVal = ffor (updated d) $ \(newK :=> newV) -> case newK `geq` k of
                Just Refl -> Just newV
                Nothing -> Nothing

      getInitial :: PullM t (n (Compose (Dynamic t) v))
      getInitial = do
        k0 :=> (v0 :: v a) <- sample $ current d
        i0 <- unsafeLiftPushM $ inner k0 v0
        return $ k0 :=> Compose i0

      update :: Event t (n (Compose (Dynamic t) v))
      update = flip push (updated d) $ \(newKey :=> newVal) -> do
        (oldKey :=> _) <- sample $ current d
        case newKey `geq` oldKey of
          Just Refl -> return Nothing
          Nothing -> do
            newInner <- inner newKey newVal
            return $ Just $ newKey :=> Compose newInner

      out :: Dynamic t (n (Compose (Dynamic t) v))
      out = unsafeBuildDynamic getInitial update
  in out
