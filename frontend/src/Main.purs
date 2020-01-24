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

module Main (main) where

import Prelude

import Api as Api
import AppM (class HasApiAccessible, class Navigate, runAppM)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Page.About as PgAbout
import Page.Home as PgHome
import Page.Infrared as PgInfrared
import Page.Plotdata as PgPlotdata
import Page.Settings as PgSettings
import Route (Route)
import Route as Route
import Routing.PushState (PushStateInterface)
import Routing.PushState as PushState
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

type ChildSlots =
  ( pgHome      :: H.Slot PgHome.Query Void Unit
  , pgPlotdata  :: H.Slot PgPlotdata.Query Void Unit
  , pgInfrared  :: H.Slot PgInfrared.Query Void Unit
  , pgSettings  :: H.Slot PgSettings.Query Void Unit
  , pgAbout     :: H.Slot PgAbout.Query Void Unit
  )

_pgHome     = SProxy :: SProxy "pgHome"
_pgPlotdata = SProxy :: SProxy "pgPlotdata"
_pgInfrared = SProxy :: SProxy "pgInfrared"
_pgSettings = SProxy :: SProxy "pgSettings"
_pgAbout    = SProxy :: SProxy "pgAbout"

type State =
  { route :: Route
  }

data Query a
  = Goto Route a

--| root component
rootComponent
  :: forall m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => H.Component HH.HTML Query Unit Void m
rootComponent =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      }
    }

-- |
initialState :: forall i. i -> State
initialState _ =
  { route: Route.Home
  }

-- |
render
  :: forall m
   . MonadAff m
  => Navigate m
  => HasApiAccessible m
  => State
  -> H.ComponentHTML Unit ChildSlots m
render state = case state.route of
    Route.Home ->
      HH.slot _pgHome unit PgHome.component unit absurd
    Route.Plotdata _ ->
      HH.slot _pgPlotdata unit PgPlotdata.component state.route absurd
    Route.Infrared _ ->
      HH.slot _pgInfrared unit PgInfrared.component state.route absurd
    Route.Settings ->
      HH.slot _pgSettings unit PgSettings.component unit absurd
    Route.About ->
      HH.slot _pgAbout unit PgAbout.component unit absurd

-- |
handleQuery :: forall action slots output m a. Query a -> H.HalogenM State action slots output m (Maybe a)
handleQuery = case _ of
  Goto newRoute a -> do
    { route } <- H.get
    when (route /= newRoute) do
      H.modify_ \state -> state { route = newRoute }
    pure (Just a)

-- | entry point
main :: Effect Unit
main = HA.runHalogenAff do
  awBody <- HA.awaitBody
  psInterface <- H.liftEffect PushState.makeInterface
  baseurl <- H.liftEffect (origin =<< location =<< window)
  --
  let env = { psInterface: psInterface
            , apiBaseURL: Api.BaseURL baseurl
            , apiTimeout: Milliseconds 3600.0
            }
  --
  halogenIO <- runUI
                (H.hoist (flip runAppM env) rootComponent)
                unit awBody
  forkAff (routeSignal halogenIO psInterface)

-- |
routeSignal :: H.HalogenIO Query Void Aff -> PushStateInterface -> Aff (Effect Unit)
routeSignal hio interface =
  H.liftEffect $ PushState.matches Route.routing pathChanged interface
  where

  pathChanged _ newRoute = do
    void $ launchAff $ hio.query $ H.tell (Goto newRoute)
    pure unit
