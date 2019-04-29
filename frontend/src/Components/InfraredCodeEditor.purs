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
  ( IRInputForm
  , Input
  , Output(..)
  , Query(..)
  , component
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
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
import Halogen.Query as HQ
import Halogen.Themes.Bootstrap4 as HB
import InfraredCode (InfraredHexString, infraredHexStringParser)
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))
import Utils (removeAllSpaces, toArrayArray)

-- |
type State =
  { text        :: String
  , formErrors  :: Int
  , formDirty   :: Boolean
  }

-- |
data Query a
  = Formless (Formless.Message' IRInputForm) a
  | OnClickReset a
  | OnClickSeparate32bits a
  | HandleInput Input a

-- |
type Input = String

-- |
data Output
  = TextChanged String
  | Reset

type ChildQuery m = Formless.Query' IRInputForm m
type ChildSlot = Unit

-- | component
component
  :: forall m
   . MonadAff m
  => H.Component HH.HTML Query Input Output m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState = 
    { text: ""
    , formErrors: 0
    , formDirty: false
    }

  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Output m
  eval = case _ of

    Formless (Formless.Emit _) next -> do
      pure next

    Formless (Formless.Submitted formOutputs) next -> do
      let irForm :: InfraredInput
          irForm = Formless.unwrapOutputFields formOutputs
          hexstr = String.toUpper $ removeAllSpaces irForm.ircode
      H.modify_ _ {text = hexstr}
      H.raise $ TextChanged hexstr
      pure next

    Formless (Formless.Changed fstate) next -> do
      H.modify_ _ { formErrors = fstate.errors
                  , formDirty = fstate.dirty
                  }
      pure next

    OnClickReset next -> do
      _ <- H.query unit $ Formless.resetAll_
      H.put initialState
      H.raise Reset
      pure next

    OnClickSeparate32bits next -> do
      {text} <- H.get
      let arr = String.toCodePointArray $ removeAllSpaces text
          twoDimArr = toArrayArray 8 arr
          choped = String.joinWith " " $ map String.fromCodePointArray twoDimArr
      void $ H.query unit $ Formless.setAll_ {ircode: choped} 
      pure next

    HandleInput input next -> do
      {text} <- H.get
      when (text /= input) do
        H.modify_ \st -> st {text = input}
        void $ H.query unit $ Formless.setAll_ {ircode: input} 
      pure next

  render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  render state =
    let ini = { initialInputs: initialInputs state.text
              , validators
              , render: renderFormless
              }
    in
    HH.div_
      [ resetButton OnClickReset state.formDirty
      , separate32bitsButton OnClickSeparate32bits state.formDirty
      , HH.slot unit Formless.component ini (HE.input Formless)
      ]

-- |
separate32bitsButton :: forall p f. HQ.Action f -> Boolean -> H.HTML p f
separate32bitsButton action isActive =
  HH.button
    [ HP.classes
      [ HB.m1
      , HB.btn
      , HB.btnLight
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ action
    , appendix isActive
    ]
    [ HH.text "separate to 32bits" ]
  where

  appendix = case _ of
    true -> HP.attr (HC.AttrName "active") "active"
    false -> HP.attr (HC.AttrName "disabled") "disabled"

-- |
resetButton :: forall p f. HQ.Action f -> Boolean -> H.HTML p f
resetButton action isActive =
  HH.button
    [ HP.classes
      [ HB.m1
      , HB.btn
      , HB.btnLight
      , HB.justifyContentCenter
      ]
    , HE.onClick $ HE.input_ action
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
  ( ircode :: f FieldError String InfraredHexString
  ))

derive instance newtypeIRInputForm :: Newtype (IRInputForm r f) _

-- |
initialInputs :: String -> IRInputForm Record Formless.InputField
initialInputs str =
  Formless.wrapInputFields {ircode: str}

-- |
validators :: forall m. MonadAff m => IRInputForm Record (Formless.Validation IRInputForm m)
validators =
  IRInputForm {ircode: validateInfraredCode}

-- |
validateInfraredCode :: forall form m. MonadAff m => Validation form m FieldError String InfraredHexString
validateInfraredCode =
  Formless.hoistFnE_ go
  where

  go str = case runParser str infraredHexStringParser of
    Right _ -> Right str
    Left err -> Left (InvalidInfraredCode err)

-- |
renderFormless :: forall m. MonadAff m => Formless.State IRInputForm m -> Formless.HTML' IRInputForm m
renderFormless state =
  HH.div
    [ HP.class_ HB.formGroup
    , HE.onKeyUp $ HE.input_ Formless.submit
    , HE.onPaste $ HE.input_ Formless.submit
    ]
    [ HH.label_ [ HH.text "on-off counts (count is based on 38kHz carrier)" ]
    , textarea
    , help $ Formless.getResult _textarea state.form
    ]
  where

  _textarea = SProxy :: SProxy "ircode"

  textarea =
    HH.textarea
      [ HP.classes [ HB.formControl, HB.textMonospace ]
      , HP.rows 5
      , HP.placeholder "Write an on-off pair count (32-bit little endianness) hexadecimal number or Click download button."
      , HP.value $ Formless.getInput _textarea state.form
      , HE.onValueInput $ HE.input $ Formless.setValidate _textarea
      , HE.onChange $ HE.input_ Formless.submit
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
