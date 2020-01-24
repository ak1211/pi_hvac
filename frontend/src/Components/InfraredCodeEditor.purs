{-
 PiHVAC <https://github.com/ak1211/pi_hvac>
 Copyright 2019 Akihiro Yamamoto

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-}

module Components.InfraredCodeEditor
  ( Action(..)
  , Output(..)
  , Query(..)
  , component
  ) where

import Prelude

import Components.InfraredCodeEditor.Form as Form
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH

-- |
data Query a

-- |
data Action
  = HandleInput String
  | HandleEditingForm Form.Output

-- |
data Output
  = TextChanged String
  | Reset

type ChildSlot =
  ( formless :: Formless.Slot Form.IRCodeEditForm (Const Void) () Form.Output Unit )

type State = String

-- |
component :: forall m. MonadAff m => H.Component HH.HTML Query String Output m
component =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< HandleInput  
      }
    }
  where
  initialState i = i

-- |
render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlot m
render state =
  HH.slot Formless._formless unit Form.component state (Just <<< HandleEditingForm)

-- |
handleAction
  :: forall i m
   . MonadAff m
  => Action
  -> H.HalogenM State Action i Output m Unit
handleAction = case _ of
  HandleInput inputText -> do
    logShow ("Input" :: String)
    pure mempty

  HandleEditingForm (Form.Text t) -> do
    logShow ("IRCodeEditForm---" :: String)
---    let hexstr = unwrap infraredCodeText.irCodeText
---    log hexstr
---    logShow ("---IRCodeEditForm" :: String)
    H.raise $ TextChanged t.infraredCodeText
    pure mempty

  HandleEditingForm Form.Reset -> do
    H.raise Reset
    pure mempty
