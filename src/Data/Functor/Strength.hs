module Data.Functor.Strength where

costrengthL :: Monad m => Either (m a) b -> m (Either a b)
costrengthL e = case e of
  Left as -> fmap Left as
  Right b -> return (Right b)
{-# INLINE costrengthL #-}

costrengthR :: Monad m => Either a (m b) -> m (Either a b)
costrengthR e = case e of
  Left a -> return (Left a)
  Right bs -> fmap Right bs
{-# INLINE costrengthR #-}

strengthL :: Functor m => (m a,b) -> m (a,b)
strengthL (as,b) = fmap (\a -> (a,b)) as
{-# INLINE strengthL #-}

strengthR :: Functor m => (a,m b) -> m (a,b)
strengthR (a,bs) = fmap (\b -> (a,b)) bs
{-# INLINE strengthR #-}
