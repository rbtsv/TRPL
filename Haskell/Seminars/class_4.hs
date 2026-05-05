import Control.Monad (liftM, ap, MonadPlus)
import Control.Applicative (Alternative (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.State (StateT (..))
--readNN :: Maybe (IO String)
--readNN = do
--    x <- Just getLine
--    case x of
--        "" -> Nothing
--        x -> Just x

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  fmap :: Monad m => (a -> b) -> MaybeT m a -> MaybeT m b
  fmap = liftM

instance Monad m => Applicative (MaybeT m) where
  pure :: Monad m => a -> MaybeT m a
  pure x = MaybeT $ pure $ Just x
  (<*>) :: Monad m => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (<*>) = ap

instance Monad m => Monad (MaybeT m) where 
  (>>=) :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
        Nothing -> return Nothing
        Just value -> runMaybeT $ f value

instance Monad m => Alternative (MaybeT m) where
  empty :: Monad m => MaybeT m a
  empty = MaybeT $ return Nothing
  (<|>) :: Monad m => MaybeT m a -> MaybeT m a -> MaybeT m a
  x <|> y = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
        Nothing -> runMaybeT y
        Just _ -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where

instance MonadTrans MaybeT where
    lift :: Monad m => m a -> MaybeT m a
    lift x = MaybeT $ fmap Just x
    -- fmap Just :: m a -> m (Maybe a)

readNN :: MaybeT IO String
readNN = do
    x <- lift getLine
    case x of
        "" -> empty
        x -> pure x

-- StateT s m a = StateT { runStateT :: (s -> m (a, s)) }
newtype OldParser a = OldParser {runOldParse :: String -> Maybe (a, String)}

type Parser a = StateT String Maybe a
item :: Parser Char
item = StateT $ \s -> case s of
    [] -> Nothing
    (c : cs) -> Just (c, cs)

eps :: Parser String
eps = pure ""
char :: Char -> Parser String
char c = item >>= \x -> if c==x then pure [x] else empty
choice :: Parser String -> Parser String -> Parser String
choice a b = a <|> b
try :: Parser String -> Parser String
try a = StateT $ \s ->
    case runStateT a s of
        Nothing -> Nothing
        _ -> Just ("", s)
neg :: Parser String -> Parser String
neg a = StateT $ \s ->
    case runStateT a s of
        Nothing -> Just ("", s)
        _ -> Nothing



