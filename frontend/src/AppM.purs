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
module AppM
  ( Env(..)
  , AppM(..)
  , class HasApiAccessible
  , class Navigate
  , getApiBaseURL
  , getApiTimeout
  , navigate
  , runAppM
  ) where

import Prelude
import Api as Api
import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Time.Duration (Milliseconds)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Route (Route)
import Route as Route
import Routing.PushState (PushStateInterface)
import Type.Equality (class TypeEquals, from)

type Env
  = { psInterface :: PushStateInterface
    , apiBaseURL :: Api.BaseURL
    , apiTimeout :: Milliseconds
    }

-- | ReaderT monad pattern
newtype AppM a
  = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

-- | ReaderT helper
runAppM :: forall a. AppM a -> Env -> Aff a
runAppM (AppM m) env = runReaderT m env

-- | MonadAsk
instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

-- | Navigate
class
  Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateAppM :: Navigate AppM where
  navigate newRoute = do
    psInterface <- AppM ask <#> \env -> env.psInterface
    locState <- liftEffect psInterface.locationState
    let
      currentPathQuery = locState.path <> locState.search

      nextPathQuery = Route.routeToPathQuery newRoute
    when (nextPathQuery /= currentPathQuery) do
      liftEffect $ psInterface.pushState locState.state nextPathQuery

instance navigateHalogenM :: Navigate m => Navigate (H.HalogenM state action slots output m) where
  navigate = H.lift <<< navigate

-- | HasApiAccessible
class
  Monad m <= HasApiAccessible m where
  getApiBaseURL :: m Api.BaseURL
  getApiTimeout :: m Milliseconds

instance hasApiAccessibleAppM :: HasApiAccessible AppM where
  getApiBaseURL = AppM ask <#> \env -> env.apiBaseURL
  getApiTimeout = AppM ask <#> \env -> env.apiTimeout

instance hasApiAccessibleHalogenM ::
  HasApiAccessible m =>
  HasApiAccessible (H.HalogenM state action slots outout m) where
  getApiBaseURL = H.lift getApiBaseURL
  getApiTimeout = H.lift getApiTimeout
