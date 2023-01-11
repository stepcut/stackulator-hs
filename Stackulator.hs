{-# language GADTs #-}
{-# language OverloadedStrings #-}
-- http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/
-- https://www2.ccs.neu.edu/racket/pubs/dissertation-kleffner.pdf
-- https://github.com/caotic123/Kei
module Stackulator where

import Control.Monad.State (State, evalState, get, put)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Exception (try)
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

type InferM = StateT Int (Except Text)

data Var where
  Name    :: Text -> Var
  GenName :: Text -> Int -> Var
  Dummy   :: Var

instance Eq Var where
  (Name n1) == (Name n2) = n1 == n2
  (GenName n1 i1) == (GenName n2 i2) = (n1 == n2) && (i1 == i2)
  Dummy == Dummy = True
  _ == _ = False

instance Ord Var where
    compare x y =
        if x == y then EQ else
          case (x, y) of
               (Name n1, Name n2) -> compare n1 n2
               (Name n1, _)       -> LT
               (GenName _ _, Name _) -> GT
               (GenName n1 i1, GenName n2 i2) -> compare (n1, i1) (n2, i2)
               (GenName _ _, Dummy) -> LT
               (Dummy, Dummy) -> EQ
               (Dummy, _)     -> GT

ppVar :: Var -> Text
ppVar (Name s)      = s
ppVar (GenName s i) = s <> Text.pack (show i)
ppVar (Dummy)        = "_"

instance Show Var where
  show = Text.unpack . ppVar

data Literal
     = LDouble Double

instance Show Literal where
  show = Text.unpack . ppLiteral


ppLiteral :: Literal -> Text
ppLiteral (LDouble d) = Text.pack (show d)

data Primitive
  = Plus
  | Minus
  | Times
  | Divide

ppPrimitive :: Primitive -> Text
ppPrimitive Plus   = "+"
ppPrimitive Minus  = "-"
ppPrimitive Times  = "*"
ppPrimitive Divide = "/"

instance Show Primitive where
  show = Text.unpack . ppPrimitive

ppCon0 :: Text -> Text
ppCon0 n = n

type Nat = Int

data Term where
  TVar     :: Var -> Term
  Universe :: Nat -> Term
--   Lit    : Literal -> Term
  Pi       :: Var -> Term -> Term -> Term
  Lambda   :: Var -> Term -> Term -> Term
  App      :: Term -> Term -> Term
  Lit      :: Literal -> Term
  Prim     :: Primitive -> Term
  Con0     :: Text -> Term

ppTerm :: Term -> Text
ppTerm  (TVar v)      = ppVar v
ppTerm (Universe 0)   = "Type"
ppTerm (Universe n)   = "Type " <> (Text.pack $ show n)
ppTerm (Pi v a b)     = "forall " <> ppVar v <> " : " <> ppTerm a <> ". " <> ppTerm b
ppTerm (Lambda v x y) = "(\\" <> ppVar v <> " : " <> ppTerm x <> " => " <> ppTerm y <> ")"
-- ppTerm (App f x)      = ppTerm f <> " " <> ppTerm x
ppTerm (App f x)      = ppTerm x <> " " <> ppTerm f
ppTerm (Lit lit)      = ppLiteral lit
ppTerm (Prim p)       = ppPrimitive p
ppTerm (Con0 n)       = ppCon0 n

instance Show Term where
  show = Text.unpack . ppTerm

data Statement where
     Let     :: Var  -> Term -> Statement
     Assume  :: Var  -> Term -> Statement
     Forget  :: Var  -> Statement
     STerm   :: Term -> Statement

ppStatement :: Statement -> Text
ppStatement (Let v t)    = "let "    <> ppVar v <> " = " <> ppTerm t
ppStatement (Assume v t) = "assume " <> ppVar v <> " : " <> ppTerm t
ppStatement (Forget v)   = "forget " <> ppVar v
ppStatement (STerm t)    = ppTerm t

instance Show Statement where
  show = Text.unpack . ppStatement

inc :: InferM Int
inc =
  do n <- get
     let n' = n + 1
     put n'
     pure n'

fresh :: Var -> InferM Var
fresh v =
  do k <- inc
     case v of
       (Name n)      -> pure (GenName n k)
       (GenName n _) -> pure (GenName n k)
       Dummy         -> pure (GenName "_" k)

data Context where
  Ctx :: Map Var (Term, Maybe Term) -> Context

instance Show Context where
  show (Ctx map) = show map

emptyContext :: Context
emptyContext = Ctx Map.empty

preludeContext :: Context
preludeContext = Ctx $ Map.fromList
 [ (Name "Nat", (Universe 0, Nothing))
 , (Name "z", (TVar (Name "Nat"), Nothing))
 , (Name "s", (Pi Dummy (TVar (Name "Nat")) (TVar (Name "Nat")), Nothing))
 , (Name "three", (TVar (Name "Nat"), Just (App (TVar (Name "s")) (App (TVar (Name "s")) (App (TVar (Name "s")) (TVar (Name "z")))))))
 , (Name "Double", (Universe 0, Nothing))
 , (Name "id", (Pi (Name "a") (Universe 0) (Pi (Name "x") (TVar (Name "a")) (TVar (Name "a"))), Just (Lambda (Name "a") (Universe 0) (Lambda (Name "x") (TVar (Name "a")) (TVar (Name "x"))))))
 , (Name "Maybe", (Pi (Name "a") (Universe 0) (Universe 0), Nothing))
-- , (Name "Nothing", (Pi (Name "a") (Universe 0) (App (TVar (Name "Maybe")) (TVar (Name "a"))), Just (Lambda (Name "a") (Universe 0) (Con0 "Nothing"))))
 , (Name "Nothing", (Pi (Name "a") (Universe 0) (App (TVar (Name "Maybe")) (TVar (Name "a"))), Just (Con0 "Nothing")))
 ]

lookupTy :: Var -> Context -> Maybe Term
lookupTy v (Ctx ctx) =
  case Map.lookup v ctx of
    (Just (ty, _)) -> Just ty
    _              -> Nothing

lookupValue :: Var -> Context -> Maybe (Maybe Term)
lookupValue v (Ctx ctx) =
  case Map.lookup v ctx of
    (Just (_, val)) -> Just val
    Nothing         -> Nothing

extend :: Var -> Term -> Maybe Term -> Context -> Context
extend x t val (Ctx ctx) =
  Ctx (Map.insert x (t, val) ctx)

subst :: Map Var Term -> Term -> InferM Term
subst vars e =
  case e of
    (TVar v) ->
      case Map.lookup v vars of
        Nothing   -> return e
        (Just e') -> return e'
    (Universe _) -> return e
    Pi v e1 e2 ->
      do (v', e1', e2') <- subst' vars v e1 e2
         return $ Pi v' e1' e2'
    Lambda v e1 e2 ->
      do (v', e1', e2') <- subst' vars v e1 e2
         return $ Lambda v' e1' e2'
    App e1 e2 ->
      do e1' <- subst vars e1
         e2' <- subst vars e2
         return $ App e1' e2'
    Lit _ -> pure e -- probably right?
    Prim _ -> pure e -- probably right?
    (Con0 _) -> pure e -- probably right?

subst' :: Map Var Term -> Var -> Term -> Term -> InferM (Var, Term, Term)
subst' vars x t e =
  do x' <- fresh x
     t' <- subst vars t
     e' <- subst (Map.insert x (TVar x') vars) e
     return (x', t', e')


tDouble :: Term
tDouble = TVar (Name "Double")

inferLiteral :: Literal -> InferM Term
inferLiteral (LDouble _) = pure tDouble

inferPrimitive :: Primitive -> InferM Term
inferPrimitive Plus   = pure $ Pi Dummy tDouble (Pi Dummy tDouble tDouble)
inferPrimitive Minus  = pure $ Pi Dummy tDouble (Pi Dummy tDouble tDouble)
inferPrimitive Times  = pure $ Pi Dummy tDouble (Pi Dummy tDouble tDouble)
inferPrimitive Divide = pure $ Pi Dummy tDouble (Pi Dummy tDouble tDouble)

inferType :: Context -> Term -> InferM Term
inferType ctx e =
  case e of
    (TVar x) ->
      case lookupTy x ctx of
        Nothing  -> throwError ("inferType: unknown identifier: " <> (ppVar x))
        (Just t) -> return t

    (Universe k) -> return (Universe (k + 1))

    Pi x t1 t2 ->
      do k1 <- inferUniverse ctx t1
         k2 <- inferUniverse (extend x t1 Nothing ctx) t2
         return (Universe (max k1 k2))

    Lambda x t e ->
      do u <- inferUniverse ctx t
         te <- inferType (extend x t Nothing ctx) e
         return (Pi x t te)

    App e1 e2 ->
      do (x, s, t) <- inferPi ctx e1
         te <- inferType ctx e2
         b <- equal ctx s te
         if b
           then subst (Map.fromList [(x, e2)]) t
           else throwError ("inferType:\nexpected type: " <> ppTerm s <> "\ninferred type: " <> ppTerm te)

    Lit l -> inferLiteral l

    Prim p -> inferPrimitive p

    (Con0 n) ->
      case lookupTy (Name n) ctx of
        Nothing -> throwError ("inferType: unknown identifier: " <> ppVar (Name n))
        (Just t) -> pure t

inferPi :: Context -> Term -> InferM (Var, Term, Term)
inferPi ctx e =
  do t <- inferType ctx e
     t' <- normalize ctx t
     case t' of
       (Pi x s t) -> return (x, s, t)
       _          -> throwError $ "inferPi: function expected. Got: " <> ppTerm t'

inferUniverse :: Context -> Term -> InferM Nat
inferUniverse ctx t =
  do u  <- inferType ctx t
     u' <- normalize ctx u
     case u' of
       (Universe k) -> return k
       _ -> throwError $ "inferUniverse: type expected. Got: t=" <> ppTerm t <> ", u=" <> ppTerm u <> ", u'=" <> ppTerm u'

normalize :: Context -> Term -> InferM Term
normalize ctx e =
  case e of
    (TVar x) ->
      case lookupValue x ctx of
        Nothing -> throwError $ "normalize: Unknown identifier: " <> ppVar x
        (Just Nothing) -> return (TVar x)
        (Just (Just e)) -> normalize ctx e

    (Universe k) -> return (Universe k)

    Pi x t e ->
       do (x', t', e') <- normalize' ctx x t e
          return $ Pi x' t' e'

    Lambda x t e ->
       do (x', t', e') <- normalize' ctx x t e
          return $ Lambda x' t' e'
    (App (App (Prim Plus) e1) e2) ->
      do e1' <- normalize ctx e1
         e2' <- normalize ctx e2
         case (e1', e2') of
           (Lit (LDouble d1), Lit (LDouble d2)) ->
              return $ Lit $ LDouble (d2 + d1)
           _ -> return $ App (App (Prim Plus) e1') e2'
    (App (App (Prim Minus) e1) e2) ->
      do e1' <- normalize ctx e1
         e2' <- normalize ctx e2
         case (e1', e2') of
           (Lit (LDouble d1), Lit (LDouble d2)) ->
              return $ Lit $ LDouble (d2 - d1)
           _ -> return $ App (App (Prim Minus) e1') e2'
    (App (App (Prim Times) e1) e2) ->
      do e1' <- normalize ctx e1
         e2' <- normalize ctx e2
         case (e1', e2') of
           (Lit (LDouble d1), Lit (LDouble d2)) ->
              return $ Lit $ LDouble (d2 * d1)
           _ -> return $ App (App (Prim Times) e1') e2'

    (App (App (Prim Divide) e1) e2) ->
      do e1' <- normalize ctx e1
         e2' <- normalize ctx e2
         case (e1', e2') of
           (Lit (LDouble d1), Lit (LDouble d2)) ->
              return $ Lit $ LDouble (d2 / d1)
           _ -> return $ App (App (Prim Divide) e1') e2'

    (App e1 e2) ->
      do e2' <- normalize ctx e2
         e1' <- normalize ctx e1
         case e1' of
           (Lambda x _ e1'') ->
             do e1''' <- subst (Map.fromList [(x, e2')]) e1''
                normalize ctx e1'''
           e1'' -> return $ App e1'' e2'

    (Lit l) -> return (Lit l)
    (Prim p) -> pure (Prim p)
    (Con0 n) -> pure (Con0 n)

normalize' :: Context -> Var  -> Term -> Term -> InferM (Var, Term, Term)
normalize' ctx x t e =
  do t' <- normalize ctx t
     e' <- normalize (extend x t' Nothing ctx) e
     return (x, t', e')

equal ctx e1 e2 =
  do e1' <- normalize ctx e1
     e2' <- normalize ctx e2
     equal' e1' e2'
  where
    equal' :: Term -> Term -> InferM Bool
    equal'' :: Var -> Term -> Term -> Var -> Term -> Term -> InferM Bool
    equal' e1 e2 =
      case (e1, e2) of
        (TVar x1, TVar x2) -> pure (x1 == x2)
        (App e11 e12, App e21 e22) ->
          do b1 <- equal' e11 e21
             b2 <- equal' e12 e22
             pure (b1 && b2)
        (Universe k1, Universe k2) -> pure (k1 == k2)
        (Pi x1 t1 e1, Pi x2 t2 e2) ->
          equal'' x1 t1 e1 x2 t2 e2
        (Lambda x1 t1 e1, Lambda x2 t2 e2) ->
          equal'' x1 t1 e1 x2 t2 e2
        _ -> pure False

    equal'' x t1 e1 y t2 e2 =
      do b1  <- equal' t1 t2
         e2' <- subst (Map.fromList [(y, (TVar x))]) e2
         b2  <- equal' e1 e2'
         pure (b1 && b2)

runInfer :: Context -> Term -> Either Text Term
runInfer ctx term = runExcept $ evalStateT (inferType ctx term) 0

runEval :: Context -> Term -> Either Text Term
runEval ctx term = runExcept $ evalStateT (normalize ctx term) 0

evalIO :: Context -> Term -> IO ()
evalIO ctx term =
    case runEval ctx term of
      (Left err)   -> putStrLn $ "error: " ++ Text.unpack err
      (Right term) -> putStrLn $ show $ ppTerm term

typeCheck :: Context -> Term -> IO ()
typeCheck ctx term =
  case runInfer ctx term of
    (Left err)   -> putStrLn $ "error: " ++ Text.unpack err
    (Right term) -> putStrLn $ show $ ppTerm term
