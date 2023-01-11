{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language ExistentialQuantification #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Chili.Types (KeyboardEvent(KeyDown, KeyUp, KeyPress), KeyboardEventObject, MouseEvent(Click), altKey, currentDocument, getElementById, getValue, setValue, toJSNode, keyCode, charCode, preventDefault)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, readMVar, modifyMVar_)
import Control.Monad.State (get)
import Data.Char (chr)
import Data.Maybe (isNothing)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.JSString as JS
import Dominator
import Dominator.Types (Attr(..), Html(..), DHandle)
import Dominator.HSX
import Dominator.Patch (updateView)
import Dominator.Diff  ()
import Language.Haskell.HSX.QQ (hsx)

import Dominator
import Dominator.Types (Attr(..), Html(..), DHandle)
import Model
import Stackulator
import Parser

import Prelude hiding (drop)

data Web = Web
 { model    :: Model
 , errorMsg :: Text
 }

viewStackElem :: (Term, Term) -> Html
viewStackElem (e, ty) =
  [hsx|
    <div class="row">
      -- expression
      <div class="col-md-6">
       <% ppTerm e %>
      </div>
      -- term
      <div class="col-md-6">
       <% ppTerm ty %>
      </div>
    </div>
  |]

viewContextElem :: (Var, (Term, Maybe Term)) -> Html
viewContextElem (v, (ty, me)) =
  [hsx|
    <div class="row">
      -- var
      <div class="col-md-4">
        <% ppVar v %>
      </div>
       -- expr
      <div class="col-md-4">
       <% case me of
            Nothing  -> Text.empty
            (Just e) -> ppTerm e
       %>
      </div>
       -- type
      <div class="col-md-4">
        <% ppTerm ty %>
      </div>
    </div>
    |]

view :: DHandle -> MVar Web -> IO Html
view domH mWeb = do
  (Web (Model (Ctx ctx) stack) errorMsg) <- readMVar mWeb
  pure $
   [hsx|
    <div class="container" >
      <div class="row">
        <div class="col-md-12">
          <h1>Stackulator</h1>
        </div>
      </div>
      <h3>Context</h3>
      <div class="row">
        <div class="col-md-4"><b>Var</b></div>
        <div class="col-md-4"><b>Expression</b></div>
        <div class="col-md-4"><b>Type</b></div>
      </div>
      <div id="context">
        <% map viewContextElem (reverse $ Map.toList ctx) %>
      </div>
      <h3>Stack</h3>
      <div class="row">
        <div class="col-md-6"><b>Expression</b></div>
        <div class="col-md-6"><b>Type</b></div>
      </div>
      <div id="stack">
       <% map viewStackElem (reverse stack) %>
      </div>
      <pre id="error"><% errorMsg %></pre>
      <div class="row">

        <input type="text" id="prompt" [EL KeyDown (onKeyDown domH mWeb)
                                      ] />


--        <input type="submit" id="prompt" value="" />
        <input type="submit" id="enter" value="<enter>" [EL Click (const $ enter domH mWeb) ] />
        <input type="submit" id="apply" value="apply"   [EL Click (simple $ cmd domH mWeb apply) ] />
        <input type="submit" id="eval"  value="eval"    [EL Click (simple $ cmd domH mWeb eval) ]/>
        <input type="submit" id="swap"  value="swap"    [EL Click (simple $ cmd domH mWeb swap) ]/>
        <input type="submit" id="drop"  value="drop"    [EL Click (simple $ cmd domH mWeb drop) ]/>
      </div>
    </div>
   |]
  where
    simple :: IO a -> (e -> IO ())
    simple a e = a >> pure ()

pushNget :: String -> ModelM Model
pushNget str = do pushString str
                  get

cmd :: DHandle -> MVar Web -> ModelM () -> IO Bool
cmd domH mWeb thecmd =
  do (Web model _) <- takeMVar mWeb
     case evalModelM (thecmd >> get) model of
       Left err ->
         do putMVar mWeb (Web model err)
            updateView domH =<< (view domH mWeb)
            pure False

       (Right model') ->
         do putMVar mWeb (Web model' "OK")
            updateView domH =<< (view domH mWeb)
            pure True

enter :: DHandle -> MVar Web -> IO ()
enter domH mWeb =
  do (Web model errorMsg) <- takeMVar mWeb
     (Just doc)  <- currentDocument
     (Just prompt) <- getElementById doc "prompt"
     (Just str) <- getValue prompt
     let eModel = evalModelM (pushNget (JS.unpack str)) model
     case eModel of
       (Left err) -> do putMVar mWeb (Web model err)
                        updateView domH =<< (view domH mWeb)
                        pure ()
       (Right model') ->
         do setValue prompt ""
            putMVar mWeb (Web model' "OK")
            updateView domH =<< (view domH mWeb)
     pure ()

onKeyPress :: DHandle -> MVar Web -> KeyboardEventObject -> IO ()
onKeyPress domH mWeb e =
  do let k = keyCode e
         c = chr (charCode e)
         alt = altKey e
     putStrLn $  "KeyPress: k = " ++ show k ++ " c = " ++ show c ++ " alt = " ++ show alt

onKeyUp :: DHandle -> MVar Web -> KeyboardEventObject -> IO ()
onKeyUp domH mWeb e =
  do -- preventDefault e
     let k = keyCode e
         c = chr (charCode e)
         alt = altKey e
     putStrLn $  "KeyUp: k = " ++ show k ++ " c = " ++ show c ++ " alt = " ++ show alt

onKeyDown2 :: DHandle -> MVar Web -> KeyboardEventObject -> IO ()
onKeyDown2 domH mWeb e =
  do -- preventDefault e
     let k = keyCode e
         c = chr (charCode e)
         alt = altKey e
{-
     (Just doc)  <- currentDocument
     (Just prompt) <- getElementById doc "prompt"
     (Just str) <- getValue prompt
     putStrLn $  "KeyDown 2: k = " ++ show k ++ " c = " ++ show c ++ " alt = " ++ show alt ++ " str=" ++ (JS.unpack str)
-}
     pure ()

onKeyDown :: DHandle -> MVar Web -> KeyboardEventObject -> IO ()
onKeyDown domH mWeb e =
  do let k = keyCode e
         c = chr (charCode e)
         alt = altKey e
     (Just doc)  <- currentDocument
     (Just prompt) <- getElementById doc "prompt"
     (Just str) <- getValue prompt
--     putStrLn $  "KeyDown: k = " ++ show k ++ " c = " ++ show c ++ " alt = " ++ show alt ++ " str=" ++ (JS.unpack str)
     case () of
       () | k == 13 ->
            enter domH mWeb
          | alt -> do case () of
                        () | k == 65 -> -- A
                               do (Just doc)  <- currentDocument
                                  (Just prompt) <- getElementById doc "prompt"
                                  (Just str) <- getValue prompt
                                  if str /= ""
                                    then do s <- cmd domH mWeb $ pushString (JS.unpack str)
                                            if s
                                              then do setValue prompt ""
                                                      cmd domH mWeb $ apply
                                                      pure ()
                                              else pure ()
                                    else do cmd domH mWeb $ apply
                                            pure ()
                           | k == 68 -> -- D
                               cmd domH mWeb drop >> pure ()
                           | k == 86 -> -- V
                               cmd domH mWeb eval >> pure ()
                           | k == 83 -> -- S
                               cmd domH mWeb swap >> pure ()
                           | otherwise -> pure ()
                      preventDefault e
          | otherwise -> pure ()
     pure ()

main :: IO ()
main =
  do mWeb <- newMVar (Web initialModel "")
     mdomH <- attachById "root"
     case mdomH of
         Nothing -> error "could not attach"
         (Just domH) -> initView domH =<< view domH mWeb
     pure ()
