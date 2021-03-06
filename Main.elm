module Main exposing (Model, Msg, update, view, subscriptions, init)

import Time
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Random
import Random.Extra
import Keyboard
import Json.Encode as JE
import Json.Decode as JD
import Http


encodeOutput : Model -> JE.Value
encodeOutput model =
    JE.object
        [ ( "gamer", JE.bool model.gamer )
        , ( "trials1", JE.list <| List.map encodeTrial <| List.reverse model.trials1 )
        , ( "trials2", JE.list <| List.map encodeTrial <| List.reverse model.trials2 )
        ]


encodeTrial : Trial -> JE.Value
encodeTrial trial =
    JE.object
        [ ( "direction", JE.string <| toString trial.direction )
        , ( "position", JE.string <| toString trial.position )
        , ( "tries", JE.list <| List.map (\x -> JE.string <| toString x) <| List.reverse trial.tries )
        , ( "startedAt", JE.float trial.startedAt )
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Phase
    = UserInfo
    | Instructions1
    | Testing
    | Instructions2
    | WrapUp


type ArrowPosition
    = Top
    | Bottom
    | Middle


type ArrowDirection
    = Up
    | Down


type TestType
    = Interference
    | NonInterference


type alias Trial =
    { direction : ArrowDirection
    , position : ArrowPosition
    , tries : List ArrowDirection
    , startedAt : Time.Time
    }


type alias Model =
    { phase : Phase
    , gamer : Bool
    , test : TestType
    , trials1 : List Trial
    , trials2 : List Trial
    , readyToEnd : Bool
    , pasteUrl : Maybe String
    , emailSent : Bool
    }


type Msg
    = GotKey Keyboard.KeyCode
    | NextScreen
    | CreateNextTrial ( ArrowDirection, ArrowPosition ) Time.Time
    | IsGamer Bool
    | WhichTestFirst TestType
    | NewArrowConfig ( ArrowDirection, ArrowPosition )
    | GotPasteUrl (Result Http.Error String)
    | EmailSent (Result Http.Error ())



-- body{margin:40px
-- auto;max-width:650px;line-height:1.6;font-size:18px;color:#444;padding:0
-- 10px}h1,h2,h3{line-height:1.2}


motherfuckingStyle : Attribute msg
motherfuckingStyle =
    style
        [ ( "margin", "40px auto" )
        , ( "max-width", "650px" )
        , ( "line-height", "1.6" )
        , ( "font-size", "18px" )
        , ( "color", "#444" )
        , ( "padding", "0 10px" )
        ]


mfStyleHx : Attribute msg
mfStyleHx =
    style
        [ ( "line-height", "1.2" )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        configGenerator =
            if model.test == NonInterference then
                arrowConfigGeneratorNonInterference
            else
                arrowConfigGeneratorInterference
    in
        case msg of
            GotKey kc ->
                if model.phase == Testing then
                    let
                        tried_ : Maybe ArrowDirection
                        tried_ =
                            case kc of
                                40 ->
                                    Just Down

                                38 ->
                                    Just Up

                                _ ->
                                    Nothing

                        newTest =
                            if model.test == NonInterference then
                                Interference
                            else
                                NonInterference

                        trials =
                            if model.test == NonInterference then
                                model.trials1
                            else
                                model.trials2

                        trial_ =
                            List.head trials
                    in
                        case tried_ of
                            Just tried ->
                                case trial_ of
                                    Just trial ->
                                        if List.length trials < 30 then
                                            let
                                                matched =
                                                    if tried == trial.direction then
                                                        Random.generate NewArrowConfig configGenerator
                                                    else
                                                        Cmd.none

                                                tries_ =
                                                    trial.tries

                                                newTrial =
                                                    { trial | tries = (tried :: tries_) }

                                                newTrials =
                                                    case List.tail trials of
                                                        Just tt ->
                                                            newTrial :: tt

                                                        Nothing ->
                                                            [ newTrial ]
                                            in
                                                if model.test == NonInterference then
                                                    ( { model | trials1 = newTrials }, matched )
                                                else
                                                    ( { model | trials2 = newTrials }, matched )
                                        else
                                            let
                                                newPhase =
                                                    if model.readyToEnd then
                                                        WrapUp
                                                    else
                                                        Instructions2
                                            in
                                                ( { model
                                                    | test = newTest
                                                    , phase = newPhase
                                                    , readyToEnd = not model.readyToEnd
                                                  }
                                                , if newPhase == WrapUp then
                                                    (Http.send EmailSent (makeEmailRequest (JE.encode 0 (encodeOutput model))))
                                                  else
                                                    Cmd.none
                                                )

                                    Nothing ->
                                        ( model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )
                else
                    model ! []

            IsGamer yn ->
                ( { model
                    | gamer = yn
                    , phase = Instructions1
                  }
                , Cmd.none
                )

            NextScreen ->
                case model.phase of
                    Instructions1 ->
                        ( { model | phase = Testing }
                        , Random.generate NewArrowConfig configGenerator
                        )

                    Instructions2 ->
                        ( { model | phase = Testing }
                        , Random.generate NewArrowConfig configGenerator
                        )

                    _ ->
                        model ! []

            CreateNextTrial ( ap, ad ) time ->
                let
                    newModel =
                        case model.test of
                            Interference ->
                                { model
                                    | trials2 = (Trial ap ad [] time) :: model.trials2
                                }

                            NonInterference ->
                                { model
                                    | trials1 = (Trial ap ad [] time) :: model.trials1
                                }
                in
                    ( newModel
                    , Cmd.none
                    )

            WhichTestFirst tt ->
                { model | test = tt } ! []

            NewArrowConfig config ->
                ( model, Task.perform (CreateNextTrial config) Time.now )

            GotPasteUrl res ->
                case res of
                    Ok url ->
                        ( { model | pasteUrl = Just url }, Cmd.none )

                    --Err err ->
                    --({model | pasteUrl = Just <| toString err}, Cmd.none)
                    _ ->
                        model ! []

            EmailSent res ->
                case res of
                    Ok () ->
                        {model| emailSent = True} ! []
                    Err _ ->
                        (model, (Http.send GotPasteUrl (makeRequest (JE.encode 0 (encodeOutput model)))))



--well, whatever.
--({model | pasteUrl = Just "Failed to load paste url..."}, Cmd.none)


view : Model -> Html Msg
view model =
    div [ motherfuckingStyle ]
        [ h1 [ mfStyleHx ] [ text "Spatial Stroop Arrow Test Program" ]
        , case model.phase of
            UserInfo ->
                div []
                    [ text "Hi, thanks for taking this test!"
                    , br [] []
                    , text """This study is a factorial study on the Spatial Stroop Effect that separates the two groups on whether or not the participant frequently plays video games (more than 0 hours per week)"""
                    , br [] []
                    , br [] []
                    , text "Do you play video games at least semi-frequently?"
                    , br [] []
                    , br [] []
                    , span []
                        [ button [ onClick <| IsGamer True ] [ Html.text "Yes" ]
                        , text "    "
                        , button [ onClick <| IsGamer False ] [ Html.text "No" ]
                        ]
                    ]

            Instructions1 ->
                div []
                    [ p [] [ text """The test will consist of a series of screens with arrows pointing up or down.
                        Your task is to press the up/down keys on your keyboard that correspond to the direction of each arrow as quickly and accurately as you can.""" ]
                    , p [] [ text "Get ready, then press start to begin." ]
                    , button [ onClick NextScreen ] [ text "Start" ]
                    ]

            Instructions2 ->
                div []
                    [ p [] [ text """The next test is the exact same rules as the last one-- Press the up/down keys on your keyboard corresponding to the
                    direction of each arrow you see. Good luck!""" ]
                    , p [] [ text "Get ready, then press start to begin." ]
                    , button [ onClick NextScreen ] [ text "Start" ]
                    ]

            Testing ->
                let
                    trials =
                        if model.test == NonInterference then
                            model.trials1
                        else
                            model.trials2

                    trial_ : Maybe Trial
                    trial_ =
                        case trials of
                            hd :: _ ->
                                Just hd

                            [] ->
                                Nothing
                in
                    div []
                        [ h3 [] [ text <| "Trial " ++ (toString <| List.length trials) ]
                        , case trial_ of
                            Just trial ->
                                let
                                    imgSrc =
                                        if trial.direction == Up && trial.position == Top then
                                            "images/top-up.png"
                                        else if trial.direction == Down && trial.position == Top then
                                            "images/top-down.png"
                                        else if trial.direction == Up && trial.position == Bottom then
                                            "images/bottom-up.png"
                                        else if trial.direction == Down && trial.position == Bottom then
                                            "images/bottom-down.png"
                                        else if trial.direction == Up && trial.position == Middle then
                                            "images/middle-up.png"
                                        else if trial.direction == Down && trial.position == Middle then
                                            "images/middle-down.png"
                                        else
                                            ""
                                in
                                    div [ style [ ( "text-align", "center" ) ] ]
                                        [ img [ src imgSrc, style [ ( "border-style", "dotted" ) ] ] []
                                        ]

                            Nothing ->
                                Html.text "Yo we got no trial this shit fucked"
                        ]

            WrapUp ->
                div []
                    [ p [] [ text """Thank you for completing this test!""" ]
                    , if model.emailSent then
                        p [] [ text "Your testing data has been submitted. Thanks!" ]
                      else
                        case model.pasteUrl of
                            Just url ->
                                div []
                                    [ p [] [ text "Please post this URL into the comments section of the Reddit thread. Thanks!" ]
                                    , p [] [ text url ]
                                    ]

                            Nothing ->
                                div []
                                    [ p [] [ text "It seems the data submission procedure has failed... If you'd be so kind as to copy and paste this into Pastebin, and then comment the link in the thread, that would be amazing!" ]
                                    , textarea [ rows 25, cols 80 ] [ text (JE.encode 0 (encodeOutput model)) ]
                                    ]
                    , p [] [ text "Again, thank you so much for doing this test!" ]
                    ]

        -- _ ->
        --     Html.text "invalid state"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs GotKey


whichTestGenerator : Random.Generator TestType
whichTestGenerator =
    Random.map
        (\b ->
            if b then
                Interference
            else
                NonInterference
        )
        Random.bool


arrowPositionGenerator : Random.Generator ArrowPosition
arrowPositionGenerator =
    Random.map
        (\b ->
            if b then
                Top
            else
                Bottom
        )
        Random.bool


arrowDirectionGenerator : Random.Generator ArrowDirection
arrowDirectionGenerator =
    Random.map
        (\b ->
            if b then
                Up
            else
                Down
        )
        Random.bool


arrowConfigGeneratorInterference : Random.Generator ( ArrowDirection, ArrowPosition )
arrowConfigGeneratorInterference =
    Random.pair arrowDirectionGenerator arrowPositionGenerator


arrowConfigGeneratorNonInterference : Random.Generator ( ArrowDirection, ArrowPosition )
arrowConfigGeneratorNonInterference =
    Random.pair arrowDirectionGenerator <| Random.Extra.constant Middle

makeBody paste =
    Http.multipartBody
        [ Http.stringPart "content" paste
        ]

-- [ Http.stringPart "api_dev_key" apiKey
-- , Http.stringPart "api_option" "paste"
-- , Http.stringPart "api_paste_code" paste
-- ]


makeRequest : String -> Http.Request String
makeRequest paste =
    Http.request
        { method = "POST"
        , headers = []
        , url = "https://cors-anywhere.herokuapp.com/http://dpaste.com/api/v2/"
        , body = makeBody paste
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


makeEmailRequest : String -> Http.Request ()
makeEmailRequest paste =
    Http.request
        { method = "POST"
        , headers = [ Http.header "accept" "json" ]
        , url = "https://formspree.io/yux60000@gmail.com"
        , body = makeBody paste
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


init : ( Model, Cmd Msg )
init =
    ( { phase = UserInfo
      , gamer = False
      , test = NonInterference
      , trials1 = []
      , trials2 = []
      , readyToEnd = False
      , pasteUrl = Nothing
      , emailSent = False
      }
    , Random.generate (WhichTestFirst) whichTestGenerator
    )
