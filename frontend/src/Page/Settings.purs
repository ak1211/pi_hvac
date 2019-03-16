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

module Page.Settings
  ( Query(..)
  , component
  ) where

import AppM (class Navigate, navigate)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Page.Commons as Commons
import Prelude
import Route (Route)
import Route as Route

type State =
  { session :: String
  }

data Query a
  = Initialize a
  | NavigateTo Route a

-- | child component
component
  :: forall m
   . MonadAff m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState =
    { session: "Setting"
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize next -> do
      pure next

    NavigateTo route next -> do
      navigate route
      pure next


render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ Commons.navbar NavigateTo Route.Settings
    , HH.div
      [ HP.class_ (HC.ClassName "container")
      ]
      [ HH.text state.session ]
    ]
