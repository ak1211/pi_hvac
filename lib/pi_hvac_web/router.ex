#
# PiHVAC <https://github.com/ak1211/pi_hvac>
# Copyright 2019 Akihiro Yamamoto
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# module: バックエンドルーター

defmodule PiHvacWeb.Router do
  use PiHvacWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", PiHvacWeb do
    pipe_through :browser

    get "/", PageController, :index
    get "/:page", PageController, :index
  end

  # Other scopes may use custom stacks.
  scope "/api", PiHvacWeb do
    pipe_through :api

    scope "/v1", V1, as: :v1 do
      resources "/measurements", EnvMeasuredController, except: [:new, :edit, :create, :update, :delete]
      resources "/i2c-devices", I2cDevicesController, except: [:new, :show, :edit, :create, :update, :delete]
      resources "/infra-red", InfraRedController, except: [:index, :new, :edit, :create, :delete]
      resources "/trans-ir", InfraRedController, except: [:index, :new, :show, :edit, :update, :delete]
    end
  end
end
