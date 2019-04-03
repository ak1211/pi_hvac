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
  ( InfraredQueryParams
  , PlotdataQueryParams
  , Route(..)
  , href
  , locationReplace
  , redirectTo
  , redirectToRoot
  , routeToPathQuery
  , routeToString
  , routing
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Map as M
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.HTML.Properties as HP
import Routing.Match (Match, end, lit, param, params, root)
import Web.HTML as DOM
import Web.HTML.Location as WHL
import Web.HTML.Window as Window

data Route
  = Home
  | Plotdata (Maybe PlotdataQueryParams)
  | Infrared (Maybe InfraredQueryParams)
  | Settings
  | About
derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where
  show = genericShow

-- |
type PlotdataQueryParams =
  { limits :: Maybe Int
  }

-- |
type InfraredQueryParams =
  { tab   :: Maybe Int
  , manuf :: Maybe Int
  , limits:: Maybe Int
  , page  :: Maybe Int
  }

-- | 
routing :: Match Route
routing = oneOf
  [ Home <$ (root *> end) 
  , Plotdata <<< toPlotdataParams <$ (root *> lit "plotdata") <*> param "limits"
  , Plotdata Nothing <$ (root *> lit "plotdata" *> end)
  , Infrared <<< toIrParams <$ (root *> lit "infra-red") <*> params
  , Infrared Nothing <$ (root *> lit "infra-red" *> end)
  , Settings <$ (root *> lit "settings" *> end)
  , About <$ (root *> lit "about" *> end)
  ]
  where

  toPlotdataParams :: String -> Maybe PlotdataQueryParams
  toPlotdataParams str =
    Just
    { limits: Int.fromString str
    }

  toIrParams :: M.Map String String -> Maybe InfraredQueryParams
  toIrParams kvsets =
    Just
    { tab:    Int.fromString =<< M.lookup "tab" kvsets
    , manuf:  Int.fromString =<< M.lookup "manuf" kvsets
    , limits: Int.fromString =<< M.lookup "limits" kvsets
    , page:   Int.fromString =<< M.lookup "page" kvsets
    }

-- | navbarに表示するページ名
routeToString :: Route -> String
routeToString = case _ of
  Home -> "Home"
  Plotdata _ -> "Plotdata"
  Infrared _ -> "Infra-red"
  Settings -> "Settings"
  About -> "About"

-- |
routeToPathQuery :: Route -> String
routeToPathQuery route =
  "/" <> case route of
    Home -> ""
    Plotdata Nothing -> "plotdata"
    Plotdata (Just qryparam) ->
      "plotdata" <> (
        [ ("limits=" <> _) <<< Int.toStringAs Int.decimal <$> qryparam.limits
        ]
        # Array.catMaybes
        # Array.intercalate "&"
        # case _ of
          "" -> ""
          str -> "?" <> str
      )
    Infrared Nothing -> "infra-red"
    Infrared (Just qryparam) -> 
      "infra-red" <> (
        [ ("tab=" <> _) <<< Int.toStringAs Int.decimal <$> qryparam.tab
        , ("manuf=" <> _) <<< Int.toStringAs Int.decimal <$> qryparam.manuf
        , ("limits=" <> _) <<< Int.toStringAs Int.decimal <$> qryparam.limits
        , ("page=" <> _) <<< Int.toStringAs Int.decimal <$> qryparam.page
        ]
        # Array.catMaybes
        # Array.intercalate "&"
        # case _ of
          "" -> ""
          str -> "?" <> str
      )
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
