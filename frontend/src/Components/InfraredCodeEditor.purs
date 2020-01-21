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
  where
---  ( IRInputForm
---  , Input
---  , Output(..)
---  , Query
---  , component
---  ) where

import Prelude

import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Int as Int
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Formless (FormFieldResult(..), Validation)
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query as HQ
import Halogen.Themes.Bootstrap4 as HB
import InfraredRemote.Code (InfraredHexString, infraredHexStringParser)
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))
import Utils as Utils

-- |
type State =
  { text        :: String
  , formErrors  :: Int
  , formDirty   :: Boolean
  }

-- |
data Query a
  = HandleInput Input a

-- |
data Action
---  = HandleFormless (Formless.Message' IRInputForm) a
  = HandleIRInputForm InfraredInput
  | OnClickReset
  | OnClickSeparate32bits

-- |
type Input = String

-- |
data Output
  = TextChanged String
  | Reset

---type ChildQuery m = Formless.Query' IRInputForm m
type ChildQuery = Unit
type ChildSlots = Unit

-- | component
component
  :: forall m
   . MonadAff m
  => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
---      , handleEvent = handleEvent
      }
    }

---  H.parentComponent
---    { initialState: const initialState
---    , render
---    , eval
---    , receiver: HE.input HandleInput
---    }

-- |
initialState :: forall i. i -> State
initialState _ = 
  { text: ""
  , formErrors: 0
  , formDirty: false
  }

-- |
---render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
---  let ini = { initialInputs: initialInputs state.text
---            , validators
---            , render: renderFormless
---            }
---  in
  HH.div_
---    [ resetButton OnClickReset state.formDirty
---    , separate32bitsButton OnClickSeparate32bits state.formDirty
    ---, HH.slot unit Formless.component ini (HE.input Formless)
    [ HH.slot Formless._formless unit formComponent state.text (Just <<< HandleIRInputForm)
    ]

-- |
---handleEvent = case _ of
---  Formless.Submitted outputs -> H.raise (Formless.unwrapOutputFields outputs)
---  Formless.Changed formState -> pure mempty --- logShow $ delete (SProxy :: _ "form") formState

-- |
handleAction
  :: forall i m
   . MonadAff m
  => Action
  -> H.HalogenM State Action i Output m Unit
handleAction = case _ of
  HandleIRInputForm infraredtext ->
    pure mempty

  OnClickReset -> do
---    eval Formless.resetAll
---    H.put initialState
    H.raise Reset
    pure mempty

  OnClickSeparate32bits -> do
    {text} <- H.get
---     void $ H.query unit $ Formless.setAll_ {ircode: formatTo32bits text} 
    pure mempty

---  HandleInput input -> do
---    {text} <- H.get
---    when (text /= input) do
---      H.modify_ \st -> st {text = input}
------      void $ H.query unit $ Formless.setAll_ {ircode: input} 
---    pure mempty

---  where
  -- you will usually want to define this pre-applied function if you
  -- are recursively evaluating Formless actions.
---  eval act = Formless.handleAction handleAction handleEvent act

---  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Output m
---  eval = case _ of
---
---    Formless (Formless.Emit _) next -> do
---      pure next
---
---    Formless (Formless.Submitted formOutputs) next -> do
---      let irForm :: InfraredInput
---          irForm = Formless.unwrapOutputFields formOutputs
---          hexstr = toBinaries irForm.ircode
---      H.modify_ _ {text = hexstr}
---      H.raise $ TextChanged hexstr
---      pure next
---
---    Formless (Formless.Changed fstate) next -> do
---      H.modify_ _ { formErrors = fstate.errors
---                  , formDirty = fstate.dirty
---                  }
---      pure next
---
---    OnClickReset next -> do
---      _ <- H.query unit $ Formless.resetAll_
---      H.put initialState
---      H.raise Reset
---      pure next
---
---    OnClickSeparate32bits next -> do
---      {text} <- H.get
---      void $ H.query unit $ Formless.setAll_ {ircode: formatTo32bits text} 
---      pure next
---
---    HandleInput input next -> do
---      {text} <- H.get
---      when (text /= input) do
---        H.modify_ \st -> st {text = input}
---        void $ H.query unit $ Formless.setAll_ {ircode: input} 
---      pure next

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

-- |
separate32bitsButton :: forall p f. f -> Boolean -> HH.HTML p f
separate32bitsButton action isActive =
  HH.button
    [ HP.classes
      [ HB.m1
      , HB.btn
      , HB.btnLight
      , HB.justifyContentCenter
      ]
    , HE.onClick (\_ -> Just action)
    , appendix isActive
    ]
    [ HH.text "separate to 32bits" ]
  where

  appendix = case _ of
    true -> HP.attr (HC.AttrName "active") "active"
    false -> HP.attr (HC.AttrName "disabled") "disabled"

-- |
resetButton :: forall p f. f -> Boolean -> HH.HTML p f
resetButton action isActive =
  HH.button
    [ HP.classes
      [ HB.m1
      , HB.btn
      , HB.btnLight
      , HB.justifyContentCenter
      ]
    , HE.onClick (\_ -> Just action)
    , appendix isActive
    ]
    [ HH.text "Reset" ]
  where

  appendix = case _ of
    true -> HP.attr (HC.AttrName "active") "active"
    false -> HP.attr (HC.AttrName "disabled") "disabled"

--
-- Formless
--
type InfraredInput = {ircode :: InfraredHexString}

data FieldError
  = EmptyField
  | InvalidInfraredCode ParseError

newtype IRInputForm r f = IRInputForm (r
  ( ircode :: f FieldError String String
  ))

derive instance newtypeIRInputForm :: Newtype (IRInputForm r f) _

-- |
initialInputs :: String -> IRInputForm Record Formless.InputField
initialInputs str =
  Formless.wrapInputFields {ircode: str}

-- |
formComponent :: forall m. MonadAff m => Formless.Component IRInputForm (Const Void) () String InfraredInput m
formComponent =
  Formless.component
    (\in_ -> { validators: validators, initialInputs: Just $ initialInputs in_})
    $ Formless.defaultSpec
      { render = renderFormless
      , handleEvent = Formless.raiseResult
      }
  where

  -- |
  validators :: IRInputForm Record (Formless.Validation IRInputForm m)
  validators =
    IRInputForm {ircode: validateInfraredCode}

  -- |
  validateInfraredCode :: forall form. Validation form m FieldError String String
  validateInfraredCode =
    Formless.hoistFnE_ go
    where

    go str = case runParser str infraredHexStringParser of
      Right _ -> Right str
      Left err -> Left (InvalidInfraredCode err)

  -- |
  ---renderFormless :: Formless.State IRInputForm m -> Formless.HTML' IRInputForm Aff
  renderFormless state =
    HH.div
      [ HP.class_ HB.formGroup
      , HE.onKeyUp (\_ -> Just Formless.submit)
  --    , HE.onPaste (\_ -> Just Formless.submit)
      ]
      [ HH.label_ [ HH.text "on-off counts (count is based on 38kHz carrier)" ]
      , textarea
      , help $ Formless.getResult _ircode state.form
      ]
    where

    _ircode = SProxy :: SProxy "ircode"

    textarea =
      HH.textarea
        [ HP.classes [ HB.formControl, HB.textMonospace ]
        , HP.rows 5
        , HP.placeholder "Write an on-off pair count (32-bit little endianness) hexadecimal number or json made with 'pigpio irrp.py' file or Click download button."
        , HP.value $ Formless.getInput _ircode state.form
        , HE.onValueInput (Just <<< Formless.setValidate _ircode)
        , HE.onValueChange (\_ -> Just Formless.submit)
        ]

    help = case _ of
      NotValidated                    -> initial
      Validating                      -> good "validating..."
      Error EmptyField                -> initial
      Error (InvalidInfraredCode err) -> bad err
      Success baseband                -> good "good"
      where

      initial =
        good "write a infrared codes"

      good str =
        HH.p [] [ HH.text str ]

      bad err =
        let (Position p) = parseErrorPosition err
            line = Int.toStringAs Int.decimal p.line
            col = Int.toStringAs Int.decimal p.column
            msg = parseErrorMessage err
            pos = "line: " <> line <> " column: " <> col
        in
        HH.p_
          [ HH.span [ HP.class_ HB.textDanger ] [ HH.text msg ]
          , HH.text (" at " <> pos)
          ]
