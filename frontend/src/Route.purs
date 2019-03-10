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

module Route
  ( Route(..)
  , routing
  , routeToString
  , routeToPathQuery
  , href
  , locationReplace
  , redirectTo
  , redirectToRoot
  , RecordsLimit(..)
  , defRecordsLimit
  ) where

import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Halogen.HTML.Properties as HP
import Prelude
import Routing.Match (Match, lit, param, root, end)
import Web.HTML as DOM
import Web.HTML.Location as WHL
import Web.HTML.Window as Window

data Route
  = Home
  | Plotdata (Maybe RecordsLimit)
  | Infrared
  | Settings
  | About

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where
  show = genericShow

-- |
newtype RecordsLimit = RecordsLimit Int

derive instance genericRecordsLimit :: Generic RecordsLimit _
derive instance newtypeRecordsLimit :: Newtype RecordsLimit _
derive instance eqRecordsLimit :: Eq RecordsLimit
derive instance ordRecordsLimit :: Ord RecordsLimit
instance showRecordsLimit :: Show RecordsLimit where
  show = genericShow

-- |
fromString :: String -> Maybe RecordsLimit
fromString original =
  RecordsLimit <$> Int.fromString original

-- |
defRecordsLimit :: RecordsLimit
defRecordsLimit = RecordsLimit 30

-- | 
routing :: Match Route
routing = oneOf
  [ Home <$ (root *> end) 
  , Plotdata Nothing <$ (root *> lit "plotdata" *> end)
  , Plotdata <<< fromString <$ (root *> lit "plotdata") <*> param "limits"
  , Infrared <$ (root *> lit "infra-red" *> end)
  , Settings <$ (root *> lit "settings" *> end)
  , About <$ (root *> lit "about" *> end)
  ]

-- | navbarに表示するページ名
routeToString :: Route -> String
routeToString = case _ of
  Home -> "Home"
  Plotdata _ -> "Plotdata"
  Infrared -> "Infra-red"
  Settings -> "Settings"
  About -> "About"

-- |
routeToPathQuery :: Route -> String
routeToPathQuery route =
  "/" <> case route of
    Home -> ""
    Plotdata Nothing -> "plotdata"
    Plotdata (Just n) -> "plotdata?limits=" <> Int.toStringAs Int.decimal (unwrap n)
    Infrared -> "infra-red"
    Settings -> "settings"
    About -> "about"

-- PUBLIC HELPERS

href :: forall r i. Route -> HP.IProp (href :: String | r) i
href targetRoute =
  HP.href (routeToPathQuery targetRoute)

locationReplace :: Route -> Effect Unit
locationReplace route =
  DOM.window
  >>= Window.location
  >>= WHL.replace (routeToPathQuery route)

redirectTo :: Route -> Effect Unit
redirectTo route =
  DOM.window
  >>= Window.location
  >>= \loc -> do
      org <- WHL.origin loc
      WHL.setHref (org <> routeToPathQuery route) loc

redirectToRoot :: Effect Unit
redirectToRoot =
  DOM.window
  >>= Window.location
  >>= \loc -> do
      org <- WHL.origin loc
      WHL.setHref (org <> "/") loc
