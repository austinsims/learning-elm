import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

main = Html.beginnerProgram {
  model=model
  ,view=view
  ,update=update}


-- Model

type alias Model = {
  name: String
  ,password: String
  ,passwordAgain: String
  ,didTrySubmit: Bool
  ,didSucceedSubmit: Bool}

validate: Model -> {isValid: Bool, message: String}
validate model =
  let
    (isValid, message) =
      if model.password == model.passwordAgain then
        (True, "OK")
      else
        (False, "Passwords do not match!")
  in
    {isValid=isValid, message=message}

model: Model
model = {
  name=""
  ,password=""
  ,passwordAgain=""
  ,didTrySubmit=False
  ,didSucceedSubmit=False}

-- Update
type Msg =
  Name String
  | Password String
  | PasswordAgain String
  | TrySubmit
update: Msg -> Model -> Model
update msg model = case msg of
  Name name -> {model | name=name}
  Password password -> {model | password=password}
  PasswordAgain password -> {model | passwordAgain = password}
  TrySubmit -> {model | didTrySubmit=True, didSucceedSubmit=validate model |> .isValid}


-- View

type alias View = Model -> Html Msg
view: View
view model =
  let
    isValid = validate model |> .isValid
    isButtonDisabled = if model.didTrySubmit then not isValid else False
  in div [] [
    input [type_ "text", placeholder "Name", onInput Name][]
    ,input [type_ "text", placeholder "Password", onInput Password] []
    ,input [type_ "text", placeholder "Re-enter Password", onInput PasswordAgain] []
    ,button [type_ "button", disabled isButtonDisabled, onClick TrySubmit] [text "Submit"]
    ,validationView model
    ,submissionView model
  ]

validationView: View
validationView model =
  let
    color = if (validate model |> .isValid) then "green" else "red"
    message = .message (validate model)
    view = div [style [("color", color)]] [text message] 
  in
    if model.didTrySubmit then view else text ""

submissionView: View
submissionView model =
  let
    view = div [] [
      hr [] []
      ,p [] [text "You submitted:"]
      ,p [] [text ("Name: " ++ model.name)]
      ,p [] [text ("Password: " ++ model.password)]
    ]
  in
    if model.didSucceedSubmit then view else text ""

