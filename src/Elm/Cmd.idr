module Elm.Cmd

import Data.IORef
import public Control.Monad.State

%default total
%access public export


data Step a = More a | Done

data Command : Type -> Type where
  Never : Command a
  Pure : a -> Command a
  Map : (a -> b) -> Command a -> Command b
  Batch : Command a -> Command a -> Command a
  Concat : Command a -> Command a -> Command a
  Subscribe : ((Step a -> JS_IO ()) -> JS_IO (JS_IO ())) -> Command a

Cmd : Type -> Type
Cmd a = Command (Maybe a)

implementation Semigroup (Command a) where
  Never <+> b = b
  a <+> Never = a
  a <+> b = Batch a b

(++) : Command a -> Command a -> Command a 
Never ++ b = b
a ++ Never = a
a ++ b = Concat a b

implementation Monoid (Command a) where
  neutral = Never

implementation Functor Command where
  map = Map
  
pure : a -> Command a
pure =
  Pure

eval : Command msg -> (msg -> JS_IO ()) -> JS_IO () -> JS_IO ()
eval Never onMessage onComplete = onComplete
eval (Pure x) onMessage onComplete = do onMessage x; onComplete
eval (Map f x) onMessage onComplete = eval x (\a => onMessage (f a)) onComplete
eval (Batch x y) onMessage onComplete = do
  xCompleted <- newIORef' False
  yCompleted <- newIORef' False
  let onBothCompleted = if !(readIORef' xCompleted) && !(readIORef' yCompleted) then onComplete else pure ()
  eval x onMessage (do writeIORef' xCompleted True; onBothCompleted)
  eval y onMessage (do writeIORef' yCompleted True; onBothCompleted)
eval (Concat x y) onMessage onComplete = do
  eval x onMessage (eval y onMessage onComplete)
eval (Subscribe f) onMessage onComplete = do
  unsubscribe <- f $ \step => case step of
    (More x) => onMessage x
    Done => onComplete
  pure ()
  
record UpdateState (modelTy : Type) (message : Type) where
  constructor MkUpdateState
  model : modelTy
  command : Cmd message
  
%used MkUpdateState model
%used MkUpdateState command

Update : (model : Type) -> (message : Type) -> Type -> Type
Update model message =
  StateT (UpdateState model message) JS_IO

interface (Monad m, MonadState (UpdateState model msg) m) => MonadUpdate model msg (m : Type -> Type) | m where

MonadUpdate model msg (Update model msg) where

getModel : (Applicative m, MonadUpdate model msg m) => m model
getModel = do
  state <- get
  pure $ model state

modifyModel : MonadUpdate model msg m => (model -> model) -> m ()
modifyModel f = do
  (MkUpdateState model command) <- get
  put (MkUpdateState (f model) command)

putModel : Applicative m => MonadUpdate model msg m => model -> m ()
putModel model = do
  modifyModel $ const model

batchCommand : MonadUpdate model msg m => Cmd msg -> m ()
batchCommand c =
  modify $ record { command $= flip (<+>) c }


runUpdate : Update model msg () -> model -> JS_IO (model, Cmd msg)
runUpdate m mod = do
  (_, MkUpdateState model' command') <- runStateT m (MkUpdateState mod neutral)
  pure $ (model', command')
