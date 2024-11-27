{-# LANGUAGE FunctionalDependencies, -- <- This one is so cool. <3
             MultiParamTypeClasses #-}

module Memory where

class Monad m => MemoryMonad m p i e | m -> p i e where
  memMalloc :: m p
  memRead :: p -> m (Maybe e)
  memWrite :: p -> e -> m ()
  memFree :: p -> m ()
  memRefCount :: p -> m Int
  memGetUsed :: e -> m [p]

  memIncRef :: p -> m ()
  memDecRef :: p -> m ()
  withSharedPtr :: (p -> m a) -> m a
  withSharedPtr f = do
    p <- memMalloc
    a <- f p
    memFree p
    return a

  memClean :: m ()
  memCopyRecord :: p -> m e
  memOverwrite :: p -> e -> m ()

  memRoot :: m p
  memChangeDirectory :: p -> [i] -> m (Maybe p)
  memMakeDirectory :: p -> [i] -> m p
  