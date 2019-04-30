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
import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as ChildPath
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

type ChildQuery = Coproduct5 PgHome.Query PgPlotdata.Query PgInfrared.Query PgSettings.Query PgAbout.Query
type ChildSlot = Either5 Unit Unit Unit Unit Unit

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
rootComponent = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where

  init =
    { route: Route.Home
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state = case state.route of
    Route.Home ->
      HH.slot' ChildPath.cp1 unit PgHome.component unit absurd

    Route.Plotdata _ ->
      HH.slot' ChildPath.cp2 unit PgPlotdata.component state.route absurd
      
    Route.Infrared _ ->
      HH.slot' ChildPath.cp3 unit PgInfrared.component state.route absurd

    Route.Settings ->
      HH.slot' ChildPath.cp4 unit PgSettings.component unit absurd

    Route.About ->
      HH.slot' ChildPath.cp5 unit PgAbout.component unit absurd

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of

    Goto newRoute next -> do
      { route } <- H.get
      when (route /= newRoute) do
        H.modify_ \state -> state { route = newRoute }
      pure next

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
    void $ launchAff $ hio.query $ H.action $ Goto newRoute
    pure unit
