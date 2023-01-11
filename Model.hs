{-# language OverloadedStrings #-}
module Model where

import Control.Monad.State (StateT, execStateT, evalStateT, runStateT, put, get)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec (parse)

import Stackulator
import Parser

type Stack = [(Term, Term)]

type ModelM = StateT Model (Except Text)


data Model = Model
  { context  :: Context
  , stack    :: Stack
  }
  deriving Show

initialModel :: Model
initialModel = Model preludeContext []

{-
withModel :: Model -> ModelM () -> Either Text Model
withModel model action =
  runExcept $ execStateT action model
-}

evalModelM :: ModelM a -> Model -> Either Text a
evalModelM modelM model = runExcept $ evalStateT modelM model

doLet :: Var -> Term -> ModelM ()
doLet v term =
  do (Model ctx terms) <- get
     case runInfer ctx term of
       (Left err) -> throwError err
       (Right ty) -> do put (Model (extend v ty (Just term) ctx) terms)
                        pure ()

doAssume :: Var -> Term -> ModelM ()
doAssume v term =
  do (Model ctx terms) <- get
     put (Model (extend v term Nothing ctx) terms)

push :: Term -> ModelM ()
push term =
  do (Model ctx terms) <- get
     case runInfer ctx term of
          (Left err) -> throwError err
          (Right ty) -> put (Model ctx ((term, ty) : terms))

drop :: ModelM ()
drop =
  do (Model ctx terms) <- get
     case terms of
       (t:ts) -> put (Model ctx ts)
       _      -> throwError "empty stack"

swap :: ModelM ()
swap =
  do (Model ctx terms) <- get
     case terms of
       (t1:t2:ts) -> put (Model ctx (t2:t1:ts))
       _          -> throwError "not enough elements on the stack"


dup :: ModelM ()
dup =
  do (Model ctx terms) <- get
     case terms of
      (t:terms') -> put (Model ctx (t:t:terms'))
      []         -> throwError "empty stack"


pushString :: String -> ModelM ()
pushString s =
  if s == ""
    then dup
    else case parse pStatement "" s of
           (Left err)   -> throwError (Text.pack $ show err)
           (Right statement) ->
             case statement of
               (Let v term) -> doLet v term
               (Assume v term) -> doAssume v term
               (STerm term) -> push term

apply :: ModelM ()
apply =
  do (Model ctx terms) <- get
     case terms of
      ((f, _):(x, _):xs) ->
         do let t = App f x
            case runInfer ctx t of
              (Left err) -> throwError err
              (Right ty) -> put (Model ctx ((t, ty) : xs))
      _ -> throwError "Not enough terms on the stack"

eval :: ModelM ()
eval =
  do (Model ctx terms) <- get
     case terms of
      ((t,_):terms) ->
        case runEval ctx t of
          (Left err) -> throwError err
          (Right t') -> do put (Model ctx terms)
                           push t'
      _ -> throwError "Not enough terms on the stack"

