module Soundness where

import Classes
import Interpreter
import qualified ConcreteSemantics as C
import qualified HoleSemantics as H
import Data.Sequence (Seq)

alpha :: C.Interp a b -> H.Interp a b
alpha f = H.Interp $ \(a,e) ->
  H.join (H.Success mempty) $ do
    e' <- gammaEnv e
    return $ case C.runInterp f (a,e') of
      C.Success (b,e'') -> H.Success (return (b,alphaEnv e''))
      C.Fail -> H.Fail

{-
   alpha C.id
   =
   alpha (Interp Success)
   =
   H.Interp $ \(a,e) ->
     [ case C.runInterp (Interp Success) (a,e') of
         C.Success (b,e'') -> H.Success (return (b,alphaEnv e''))
         C.Fail -> H.Fail
       | e' <- gammaEnv e ]
   =
   H.Interp $ \(a,e) ->
     [ case Success (a,e') of
         C.Success (b,e'') -> H.Success (return (b,alphaEnv e''))
         C.Fail -> H.Fail
       | e' <- gammaEnv e ]
   =
   H.Interp $ \(a,e) -> [ H.Success (return (a,alphaEnv e')) | e' <- gammaEnv e ]
   <=
   H.Interp $ \(a,e) -> H.Success (return (a,e))
   =
   H.id

   alpha (f.g)
   =
   alpha (Interp $ runInterp g >=> runInterp f)
   =
   H.Interp $ \(a,e) ->
     [ case C.runInterp (Interp $ runInterp g >=> runInterp f) (a,e') of
         C.Success (b,e'') -> H.Success (return (b,alphaEnv e''))
         C.Fail -> H.Fail
       | e' <- gammaEnv e ]
   =
   H.Interp $ \(a,e) ->
     [ case (runInterp g >=> runInterp f) (a,e') of
         C.Success (b,e'') -> H.Success (return (b,alphaEnv e''))
         C.Fail -> H.Fail
       | e' <- gammaEnv e ]
   =
   H.Interp $ \(a,e) ->
     [ case (case C.runInterp g (a,e') of
               C.Success (b,e'') -> C.runInterp f (b,e'')
               C.Fail -> C.Fail) of
         C.Success (b,e'') -> H.Success (return (b,alphaEnv e''))
         C.Fail -> H.Fail
       | e' <- gammaEnv e ]
   =
   H.Interp $ \(a,e) ->
     [ case C.runInterp g (a,e') of
         C.Success (b,e'') -> case C.runInterp f (b,e'') of
           C.Success (c,e''') -> H.Success (return (c,alphaEnv e'''))
           C.Fail -> H.Fail
         C.Fail -> H.Fail
       | e' <- gammaEnv e ]
   =
   ???
   =
   Interp $ \(a,e) -> 
     case runInterp (alpha g) (a,e) of
        H.Success bs -> join (Success mempty) (fmap (runInterp (alpha f)) bs)
        H.SuccessOrFail bs -> join (SuccessOrFail mempty) (fmap (runInterp (alpha f)) bs)
        H.Fail -> Fail 
   =
   Interp $ \(a,e) -> 
     (runInterp (alpha g) >>=>> runInterp (alpha f)) (a,e)
   =
   Interp (runInterp (alpha g) >>=>> runInterp (alpha f))
   =
   alpha f . alpha g
   
-}

gammaEnv :: H.Environment -> Seq C.Environment
gammaEnv = undefined

alphaEnv :: C.Environment -> H.Environment
alphaEnv = undefined
