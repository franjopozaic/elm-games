module TypistQuotes exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http
import Debug
import Json.Decode as Decode
import Json.Decode.Pipeline as JD exposing (decode, required)
import Time exposing (Time, millisecond, second)
import RansomLetters exposing (..)
import Keyboard
import Char
import String


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Quote =
    { quote : String
    , author : String
    }


type alias Model =
    { quote : Quote
    , state : State
    , lettersRendered : Int
    , lettersMatched : Int
    , lettersTyped : Int
    , time : Float
    }


type State
    = QuotePending
    | QuoteReceived
    | QuoteRendered
    | TypingStarted
    | TypingFinished


type Msg
    = GetQuote
    | Reset
    | OnFetchQuote (Result Http.Error (List Quote))
    | Tick Time
    | KeyPressed Keyboard.KeyCode
    | MeasureTime Time


init : ( Model, Cmd Msg )
init =
    ( Model (Quote "" "") QuotePending 0 0 0 0, fetchQuotes )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MeasureTime t ->
            let
                gameOver =
                    (model.lettersMatched == String.length model.quote.quote)
            in
                if (gameOver) then
                    ( { model | state = TypingFinished }, Cmd.none )
                else
                    ( { model | time = model.time + 0.01 }, Cmd.none )

        OnFetchQuote res ->
            case res of
                Err msg ->
                    ( model, Cmd.none )

                Ok value ->
                    let
                        firstQuote =
                            List.head value
                    in
                        case firstQuote of
                            Just value ->
                                ( { model
                                    | quote = Quote (String.filter isLetterOrSpace value.quote) value.author
                                    , state = QuoteReceived
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( model, Cmd.none )

        Reset ->
            ( { model
                | state = QuoteReceived
                , lettersRendered = 0
                , lettersMatched = 0
                , lettersTyped = 0
                , time = 0
              }
            , Cmd.none
            )

        GetQuote ->
            ( { model
                | quote = Quote "" ""
                , state = QuotePending
                , lettersRendered = 0
                , lettersMatched = 0
                , lettersTyped = 0
                , time = 0
              }
            , fetchQuotes
            )

        Tick t ->
            let
                lettersLeft =
                    String.length model.quote.quote - model.lettersRendered
            in
                if (lettersLeft > 0) then
                    ( { model | lettersRendered = model.lettersRendered + 1 }, Cmd.none )
                else
                    ( { model | state = QuoteRendered }, Cmd.none )

        KeyPressed code ->
            let
                keyPressed =
                    code
                        |> Char.fromCode
                        |> String.fromChar
                        |> String.toUpper
                        |> Debug.log "pressed"

                nextChar =
                    model.quote.quote
                        |> String.slice model.lettersMatched (model.lettersMatched + 1)
                        |> String.toUpper
                        |> Debug.log "next"
            in
                if (keyPressed == nextChar) then
                    ( { model
                        | lettersMatched = model.lettersMatched + 1
                        , lettersTyped = model.lettersTyped + 1
                        , state = TypingStarted
                      }
                    , Cmd.none
                    )
                else
                    ( { model
                        | lettersTyped = model.lettersTyped + 1
                        , state = TypingStarted
                      }
                    , Cmd.none
                    )


isLetterOrSpace : Char -> Bool
isLetterOrSpace char =
    let
        code =
            Char.toCode char
    in
        code == 32 || (Char.isUpper char || Char.isLower char)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        QuotePending ->
            Sub.none

        QuoteReceived ->
            Time.every (millisecond * 30) Tick

        QuoteRendered ->
            Keyboard.downs KeyPressed

        TypingStarted ->
            Sub.batch
                [ Keyboard.downs KeyPressed
                , Time.every (millisecond * 10) MeasureTime
                ]

        TypingFinished ->
            Sub.none


view : Model -> Html Msg
view model =
    let
        lettersRendered =
            model.lettersRendered
    in
        div [ style [ ( "margin-left", "100px" ), ( "margin-top", "80px" ) ] ]
            [ (text (String.left 5 (toString model.time)))
            , (renderLetters model.lettersRendered model.lettersMatched model.quote.quote)
            , (div [] [ button [ (onClick GetQuote) ] [ text "New quote" ] ])
            , (div [] [ button [ (onClick Reset) ] [ text "Reset" ] ])
            , div [] [ text (toString model.lettersMatched ++ "/" ++ toString model.lettersTyped) ]
            ]


fetchQuotes : Cmd Msg
fetchQuotes =
    let
        headers =
            [ Http.header "X-Mashape-Key" "tLFb3xaW1wmshmHtNuxA1Y7ZtGvdp1JulYtjsneJN1gqso8of3"
            , Http.header "Content-Type"
                "application/x-www-form-urlencoded"
            ]

        request =
            Http.request
                { method = "GET"
                , headers = headers
                , url = fetchQuotesUrl
                , body = Http.emptyBody
                , expect = Http.expectJson quotesDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send OnFetchQuote request


fetchQuotesUrl : String
fetchQuotesUrl =
    "https://andruxnet-random-famous-quotes.p.mashape.com/?cat=famous&count=1"


quotesDecoder : Decode.Decoder (List Quote)
quotesDecoder =
    Decode.list quoteDecoder


quoteDecoder : Decode.Decoder Quote
quoteDecoder =
    decode Quote
        |> JD.required "quote" Decode.string
        |> JD.required "author" Decode.string
