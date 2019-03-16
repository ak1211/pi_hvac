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

module Page.Commons
  ( showToast
  , navbar
  , icon
  , getContext2dById
  , toast
  , toastItem
  , snackbarItem
  ) where

import Prelude

import CSS (marginLeft, px)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Halogen.Themes.Bootstrap4 as BS
import Halogen.Themes.Bootstrap4 as HB
import Route (Route)
import Route as Route

-- |
foreign import showToastJs :: Effect Unit

-- |
showToast :: Effect Unit
showToast = showToastJs

-- | page navbar
navbar :: forall p f. (Route -> HQ.Action f) -> Route -> H.HTML p f
navbar navigateAction current =
  HH.nav
    [ HP.classes
      [ BS.navbar
      --, BS.navbarExpandSm
      , HC.ClassName "navbar-expand-sm"
      , BS.fixedTop
      , BS.navbarDark
      , BS.bgDark
      ]
    ]
    -- navbar brand
    [ HH.div [ HP.class_ BS.navbarBrand ] [ HH.text "PiHVAC" ]
    -- navbar toggler
    , HH.button
      [ HP.class_ BS.navbarToggler
      , HP.type_ HP.ButtonButton
      , HP.attr (HC.AttrName "data-toggle") "collapse"
      , HP.attr (HC.AttrName "data-target") "#navbarNav"
      , HP.attr (HC.AttrName "aria-controls") "navbarNav"
      , HP.attr (HC.AttrName "aria-expanded") "false"
      , HP.attr (HC.AttrName "aria-label") "Toggle navigation"
      ]
      [ HH.span [ HP.class_ BS.navbarTogglerIcon ] []
      ]
    -- navbar item
    , HH.div
      [ HP.classes
        [ BS.collapse
        , BS.navbarCollapse
        ]
      , HP.id_ "navbarNav"
      ]
      [ HH.ul
        [ HP.class_ BS.navbarNav
        ]
        [ navItem Route.Home
        , navItem (Route.Plotdata Nothing)
        , navItem Route.Infrared
        , navItem Route.Settings
        , navItem Route.About
        ] 
      ]
    -- navbar right side settings button
    {-}
    , HH.button
      [ HP.classes
        [ BS.btn
        , BS.btnOutlineWarning
        , BS.justifyContentEnd
        ]
      , HE.onClick $ HE.input_ $ navigateAction Route.Settings
      ]
      [ icon "fas fa-wrench" ]
      -}
    ]
  where

    navItem route =
      let clss =  if route == current
                  then [ BS.navItem, BS.active ]
                  else [ BS.navItem ]
      in
      HH.li
        [ HP.classes clss
        ]
        [ HH.button
          [ HP.classes [ BS.btn, BS.navLink ]
          , HE.onClick $ HE.input_ $ navigateAction route
          ]
          [ HH.text $ Route.routeToString route
          ]
        ]

-- | icon font
icon :: forall p i. String -> H.HTML p i
icon iconName =
  HH.span
    [ HP.class_ $ HC.ClassName "icon" ]
    [ HH.i [ HP.class_ $ HC.ClassName iconName ] [] ]

-- |
getContext2dById :: String -> Effect (Maybe Canvas.Context2D)
getContext2dById id_ = do
  maybeElem <- Canvas.getCanvasElementById id_
  maybe (pure Nothing) (\x -> pure =<< maybeC2d x) maybeElem
  where
    maybeC2d :: Canvas.CanvasElement -> Effect (Maybe Canvas.Context2D)
    maybeC2d elem = Just <$> Canvas.getContext2D elem

-- |
toast :: forall p i. Array (H.HTML p i) -> H.HTML p i
toast xs =
  HH.div
    [ HP.attr (HC.AttrName "aria-live") "polite"
    , HP.attr (HC.AttrName "aria-atomic") "true"
    ]
    xs

-- |
toastItem :: forall p i. String -> String -> String -> H.HTML p i
toastItem head subhead text =
  HH.div
    [ HP.classes [ HC.ClassName "toast", HB.bgInfo, HB.mxAuto ]
    , HP.attr (HC.AttrName "role") "alert"
    , HP.attr (HC.AttrName "aria-live") "assertive"
    , HP.attr (HC.AttrName "aria-atomic") "true"
    ]
    [ HH.div
      [ HP.class_ $ HC.ClassName "toast-header" ]
      [ HH.strong [ HP.class_ $ HB.mrAuto ] [ HH.text head ]
      , HH.small
        [ style $ do
          marginLeft $ px 40.0
        ]
        [ HH.text subhead
        ]
      , HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.ml2, HB.mb1, HB.close ]
        , HP.attr (HC.AttrName "data-dismiss") "toast"
        , HP.attr (HC.AttrName "area-label") "Close"
        ]
        [ HH.span [ HP.attr (HC.AttrName "aria-hidden") "true" ] [ icon "fas fa-times-circle" ]
        ]
      ]
    , HH.div [ HP.classes [ HC.ClassName "toast-body", HB.textWhite ] ] [ HH.text text ]
    ]

-- |
snackbarItem :: forall p i. String -> H.HTML p i
snackbarItem text =
  HH.div
    [ HP.classes [ HC.ClassName "toast", HB.bgInfo, HB.mxAuto ]
    , HP.attr (HC.AttrName "role") "alert"
    , HP.attr (HC.AttrName "aria-live") "assertive"
    , HP.attr (HC.AttrName "aria-atomic") "true"
    ]
    [ HH.div
      [ HP.classes [ HC.ClassName "toast-body", HB.textCenter ] ]
      [ HH.span [ HP.class_ HB.textWhite ] [ HH.text text ] ]
    ]
