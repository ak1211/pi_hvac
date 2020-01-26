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
  , Input
  , Output(..)
  , Query(..)
  , component
  ) where

import Prelude
import Components.InfraredCodeEditor.Form as Form
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH

-- |
data Query a

-- |
data Action
  = HandleInput Input
  | HandleEditingForm Form.Output

-- |
type Input
  = Maybe String

-- |
data Output
  = TextChanged String
  | Reset

type ChildSlot
  = ( formless :: Formless.Slot Form.IRCodeEditForm (Const Void) () Form.Output Unit )

type State
  = String

-- |
component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< HandleInput
            }
    }

-- |
initialState :: Input -> State
initialState i = fromMaybe "" i

-- |
render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlot m
render state = HH.slot Formless._formless unit Form.component state (Just <<< HandleEditingForm)

-- |
handleAction ::
  forall i m.
  MonadAff m =>
  Action ->
  H.HalogenM State Action i Output m Unit
handleAction = case _ of
  HandleInput inputText -> H.put $ initialState inputText
  HandleEditingForm (Form.Text t) -> H.raise $ TextChanged t.infraredCodeText
  HandleEditingForm Form.Reset -> do
    H.put $ initialState Nothing
    H.raise Reset
