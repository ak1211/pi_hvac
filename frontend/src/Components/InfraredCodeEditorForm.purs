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

module Components.InfraredCodeEditor.Form
  ( Action(..)
  , FieldError
  , IRCodeEditForm
  , IRCodeEditFormRow
  , InfraredCodeText
  , Output(..)
  , component
  ) where

import Prelude

import Data.Bifunctor as Bifunctor
import Data.Const (Const)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Formless (FormFieldResult(..), Validation)
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB
import InfraredRemote.Code (infraredCodeTextParser)
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))
import Utils as Utils

-- |
data Action
  = OnClickReset
  | OnClickSeparate32bits (Formless.PublicState IRCodeEditForm ())

-- |
data Output
  = Text InfraredCodeText 
  | Reset

type InfraredCodeText = { | IRCodeEditFormRow Formless.OutputType }

newtype IRCodeEditForm r f = IRCodeEditForm (r (IRCodeEditFormRow f))
derive instance newtypeIRCodeEditForm' :: Newtype (IRCodeEditForm r f) _

type IRCodeEditFormRow f =
  ( infraredCodeText :: f FieldError String String
  )

_infraredCodeText = SProxy :: SProxy "infraredCodeText"

data FieldError
  = EmptyField
  | InvalidInfraredCodeText ParseError

-- |
validators :: forall m. MonadAff m => IRCodeEditForm Record (Formless.Validation IRCodeEditForm m)
validators =
  IRCodeEditForm {infraredCodeText: validateInfraredCode}

-- |
validateInfraredCode :: forall form m. MonadAff m => Validation form m FieldError String String 
validateInfraredCode =
  Formless.hoistFnE_ go
  where
  go input =
    let ok = const input
    in
    Bifunctor.bimap InvalidInfraredCodeText ok $ runParser input infraredCodeTextParser

-- |
initialInputs :: IRCodeEditForm Record Formless.InputField
initialInputs =
  Formless.wrapInputFields {infraredCodeText: ""}

-- |
component :: forall m. MonadAff m => Formless.Component IRCodeEditForm (Const Void) () Unit Output m
component =
  Formless.component
    initialState
    $ Formless.defaultSpec
      { render = renderFormless
      , handleEvent = handleEvent
      , handleAction = handleAction
      }
  where
  -- |
  initialState _ =
    { validators: validators
    , initialInputs: Just initialInputs
    }

  -- |
  renderFormless state =
    HH.div_
      [ resetButton true
      , separate32bitsButton state true
      , HH.div
        [ HP.class_ HB.formGroup
        , HE.onKeyUp (\_ -> Just Formless.submit)
  ---      , HE.onPaste (\_ -> Just Formless.submit)
        ]
        [ HH.label_ [ HH.text "on-off counts (count is based on 38kHz carrier)" ]
        , textarea state
        , help $ Formless.getResult _infraredCodeText state.form
        ]
      ]

  -- |
  separate32bitsButton state isActive =
    HH.button
      [ HP.classes
        [ HB.m1
        , HB.btn
        , HB.btnLight
        , HB.justifyContentCenter
        ]
      , HE.onClick (\_ -> Just $ Formless.injAction (OnClickSeparate32bits state))
      , if isActive
        then HP.attr (HC.AttrName "active") "active"
        else HP.attr (HC.AttrName "disabled") "disabled"
      ]
      [ HH.text "separate to 32bits" ]

  -- |
  resetButton isActive =
    HH.button
      [ HP.classes
        [ HB.m1
        , HB.btn
        , HB.btnLight
        , HB.justifyContentCenter
        ]
      , HE.onClick (\_ -> Just $ Formless.injAction OnClickReset)
      , if isActive
        then HP.attr (HC.AttrName "active") "active"
        else HP.attr (HC.AttrName "disabled") "disabled"
      ]
      [ HH.text "Reset" ]

  -- |
  textarea state =
    HH.textarea
      [ HP.classes [ HB.formControl, HB.textMonospace ]
      , HP.rows 5
      , HP.placeholder "Write an on-off pair count (32-bit little endianness) hexadecimal number or json made with 'pigpio irrp.py' file or Click download button."
      , HP.value $ Formless.getInput _infraredCodeText state.form
      , HE.onValueInput (Just <<< Formless.setValidate _infraredCodeText)
      , HE.onValueChange (\_ -> Just Formless.submit)
      ]

  -- |
  handleEvent = case _ of
    Formless.Submitted outputs ->
      H.raise (Text $ Formless.unwrapOutputFields outputs)

    Formless.Changed formState ->
      pure mempty

  -- |
  handleAction = case _ of
    OnClickReset -> do
      eval Formless.resetAll
      H.raise Reset

    OnClickSeparate32bits state ->
      let
        txt = Formless.getInput _infraredCodeText state.form
        new = formatTo32bits txt
      in
      eval $ Formless.setValidate _infraredCodeText new

    where
    eval act = Formless.handleAction handleAction handleEvent act

-- |
help :: forall o p i. Formless.FormFieldResult FieldError o -> HH.HTML p i
help = case _ of
  NotValidated                        -> initial
  Validating                          -> good "validating..."
  Error EmptyField                    -> initial
  Error (InvalidInfraredCodeText err) -> bad err
  Success baseband                    -> good "good"
  where
  -- |
  initial =
    good "write a infrared codes"
  -- |
  good str =
    HH.p [] [ HH.text str ]
  -- |
  bad err =
    let
      (Position p) = parseErrorPosition err
      line = Int.toStringAs Int.decimal p.line
      col = Int.toStringAs Int.decimal p.column
      msg = parseErrorMessage err
      pos = "line: " <> line <> " column: " <> col
    in
    HH.p_
      [ HH.span [ HP.class_ HB.textDanger ] [ HH.text msg ]
      , HH.text (" at " <> pos)
      ]

-- |
toBinaries :: String -> String
toBinaries =
  String.joinWith "" <<< Utils.lines <<< Utils.removeAllSpaces <<< String.toUpper

-- |
formatTo32bits :: String -> String
formatTo32bits =
  Utils.unlines <<< map go <<< Utils.lines
  where

  go :: String -> String
  go in_ =
    let text = Utils.removeAllSpaces in_
        arr = String.toCodePointArray text
        arrarr = Utils.toArrayArray 8 arr
        strArrArr = map String.fromCodePointArray arrarr
    in
    String.joinWith " " strArrArr
