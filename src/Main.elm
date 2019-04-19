module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, h6, img, input, label, span, text)
import Html.Attributes exposing (attribute, class, disabled, id, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { balanceAsset : Int
    , balanceBase : Int
    , buyPrice : Int
    , buyAmount : Int
    , buyTotal : Int
    , buyPriceError : String
    , isBuyDisabled : Bool
    , sellPrice : Int
    , sellAmount : Int
    , sellTotal : Int
    , sellPriceError : String
    , isSellDisabled : Bool
    }


type Percent
    = Hundred
    | SeventyFive
    | Fifty
    | TwentyFive


init : ( Model, Cmd Msg )
init =
    ( { balanceAsset = 100, balanceBase = 1000, buyPrice = 1, buyAmount = 0, buyPriceError = "", buyTotal = 0, sellAmount = 0, sellPrice = 1, sellTotal = 0, isBuyDisabled = True, sellPriceError = "", isSellDisabled = True }, Cmd.none )



---- UPDATE ----


type Msg
    = Buy
    | Buy25
    | Buy50
    | Buy75
    | Buy100
    | ChangeBuyPrice String
    | ChangeBuyAmount String
    | Sell
    | Sell25
    | Sell50
    | Sell75
    | Sell100
    | ChangeSellPrice String
    | ChangeSellAmount String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Buy ->
            ( { model | balanceBase = model.balanceBase - model.buyTotal, isBuyDisabled = validateBuyButton (model.balanceBase - model.buyTotal) model.buyAmount }, Cmd.none )

        Buy25 ->
            ( buyPercent model TwentyFive, Cmd.none )

        Buy50 ->
            ( buyPercent model Fifty, Cmd.none )

        Buy75 ->
            ( buyPercent model SeventyFive, Cmd.none )

        Buy100 ->
            ( buyPercent model Hundred, Cmd.none )

        ChangeBuyPrice price ->
            ( { model | buyPrice = parseInt price, buyTotal = countTotalBuy (parseInt price) model.buyAmount, isBuyDisabled = validateBuyButton model.balanceBase (countTotalBuy (parseInt price) model.buyAmount) }, Cmd.none )

        ChangeBuyAmount amount ->
            ( { model | buyAmount = parseInt amount, buyTotal = countTotalBuy model.buyPrice (parseInt amount), isBuyDisabled = validateBuyButton model.balanceBase (countTotalBuy model.buyPrice (parseInt amount)) }, Cmd.none )

        Sell ->
            ( model, Cmd.none )

        Sell25 ->
            ( sellPercent model TwentyFive, Cmd.none )

        Sell50 ->
            ( sellPercent model Fifty, Cmd.none )

        Sell75 ->
            ( sellPercent model SeventyFive, Cmd.none )

        Sell100 ->
            ( sellPercent model Hundred, Cmd.none )

        ChangeSellPrice price ->
            ( { model | sellPrice = parseInt price, sellTotal = countTotalSell (parseInt price) model.sellAmount, isSellDisabled = validateSellButton model.balanceAsset (countTotalSell (parseInt price) model.sellAmount) }, Cmd.none )

        ChangeSellAmount amount ->
            ( { model | sellAmount = parseInt amount, sellTotal = countTotalSell model.sellPrice (parseInt amount), isSellDisabled = validateSellButton model.balanceAsset (countTotalSell model.sellPrice (parseInt amount)) }, Cmd.none )


buyPercent : Model -> Percent -> Model
buyPercent model percent =
    if model.buyPrice == 0 then
        model

    else
        case percent of
            TwentyFive ->
                { model | isBuyDisabled = validateBuyButton model.balanceBase ((model.balanceBase // 4) // model.buyPrice), buyTotal = ((model.balanceBase // 2) // model.buyPrice) * model.buyPrice, buyAmount = (model.balanceBase // 4) // model.buyPrice }

            Fifty ->
                { model | isBuyDisabled = validateBuyButton model.balanceBase ((model.balanceBase // 2) // model.buyPrice), buyTotal = ((model.balanceBase // 2) // model.buyPrice) * model.buyPrice, buyAmount = (model.balanceBase // 2) // model.buyPrice }

            SeventyFive ->
                { model | isBuyDisabled = validateBuyButton model.balanceBase (((model.balanceBase // 4) * 3) // model.buyPrice), buyTotal = (((model.balanceBase // 4) * 3) // model.buyPrice) * model.buyPrice, buyAmount = ((model.balanceBase // 4) * 3) // model.buyPrice }

            Hundred ->
                { model | isBuyDisabled = validateBuyButton model.balanceBase (model.balanceBase // model.buyPrice), buyTotal = model.balanceBase // model.buyPrice * model.buyPrice, buyAmount = model.balanceBase // model.buyPrice }


sellPercent : Model -> Percent -> Model
sellPercent model percent =
    if model.sellPrice == 0 then
        model

    else
        case percent of
            TwentyFive ->
                { model | isSellDisabled = validateSellButton model.balanceAsset (model.balanceAsset // 4), sellTotal = (model.balanceAsset // 4) * model.sellPrice, sellAmount = model.balanceAsset // 4 }

            Fifty ->
                { model | isSellDisabled = validateSellButton model.balanceAsset (model.balanceAsset // 2), sellTotal = (model.balanceAsset // 2) * model.sellPrice, sellAmount = model.balanceAsset // 2 }

            SeventyFive ->
                { model | isSellDisabled = validateSellButton model.balanceAsset ((model.balanceAsset // 4) * 3), sellTotal = ((model.balanceAsset // 4) * 3) * model.sellPrice, sellAmount = (model.balanceAsset // 4) * 3 }

            Hundred ->
                { model | isSellDisabled = validateSellButton model.balanceAsset (model.balanceAsset // 1), sellTotal = (model.balanceAsset // 1) * model.sellPrice, sellAmount = model.balanceAsset // 1 }


countTotalBuy : Int -> Int -> Int
countTotalBuy price amount =
    if amount == 0 then
        0

    else
        price * amount


countTotalSell : Int -> Int -> Int
countTotalSell price amount =
    if amount == 0 then
        0

    else
        price * amount


validateSellButton : Int -> Int -> Bool
validateSellButton balance buyTotal =
    not ((balance >= buyTotal) && (buyTotal > 0) && (balance > 0))


validateBuyButton : Int -> Int -> Bool
validateBuyButton balance amount =
    not ((balance >= amount) && (amount > 0) && (balance > 0))


printSellTotal : Model -> String
printSellTotal model =
    if model.sellTotal == 0 then
        ""

    else
        String.fromInt model.sellTotal


printSellAmount : Model -> String
printSellAmount model =
    if model.sellAmount == 0 then
        ""

    else
        String.fromInt model.sellAmount


printBuyTotal : Model -> String
printBuyTotal model =
    if model.buyTotal == 0 then
        ""

    else
        String.fromInt model.buyTotal


printBuyAmount : Model -> String
printBuyAmount model =
    if model.buyAmount == 0 then
        ""

    else
        String.fromInt model.buyAmount


parseInt : String -> Int
parseInt string =
    case String.toInt string of
        Just number ->
            number

        Nothing ->
            0



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-lg-6" ]
                [ div [ class "card card-body pd-15 mg-t-20" ]
                    [ h6 [ class "slim-card-title mg-b-10" ]
                        [ text "Buy ETH" ]
                    , h6 [ class "text-right mg-b-10" ]
                        [ text "Balance: "
                        , span [ id "base_balance" ]
                            [ text (String.fromInt model.balanceBase) ]
                        , text " BTC"
                        ]
                    , div [ class "text-danger form-text pull-left" ] [ text model.buyPriceError ]
                    , div [ class "input-group form-group" ]
                        [ input [ class "form-control", id "buy_price", placeholder "Price", type_ "number", value (String.fromInt model.buyPrice), onInput ChangeBuyPrice ]
                            []
                        , div [ class "input-group-append" ]
                            [ span [ class "input-group-text" ]
                                [ text "BTC" ]
                            ]
                        ]
                    , div [ class "input-group form-group" ]
                        [ input [ class "form-control", id "buy_amount", placeholder "Amount", type_ "number", value (printBuyAmount model), onInput ChangeBuyAmount ]
                            []
                        , div [ class "input-group-append" ]
                            [ span [ class "input-group-text" ]
                                [ text "ETH" ]
                            ]
                        ]
                    , div [ class "form-group" ]
                        [ div [ class "text-right" ]
                            [ button [ class "btn btn-outline-primary btn-sm mg-b-10", onClick Buy25 ]
                                [ text "25%" ]
                            , button [ class "btn btn-outline-primary btn-sm mg-b-10", onClick Buy50 ]
                                [ text "50%" ]
                            , button [ class "btn btn-outline-primary btn-sm mg-b-10", onClick Buy75 ]
                                [ text "75%" ]
                            , button [ class "btn btn-outline-primary btn-sm mg-b-10", onClick Buy100 ]
                                [ text "100%" ]
                            ]
                        ]
                    , div [ class "input-group form-group" ]
                        [ input [ class "form-control", id "total_buy", placeholder "Total", attribute "readonly" "", type_ "number", value (printBuyTotal model) ]
                            []
                        , div [ class "input-group-append" ]
                            [ span [ class "input-group-text" ]
                                [ text "BTC" ]
                            ]
                        ]
                    , button [ class "btn btn-success btn-block", disabled model.isBuyDisabled, onClick Buy ]
                        [ text "BUY ETH" ]
                    ]
                ]
            , div [ class "col-lg-6" ]
                [ div [ class "card card-body pd-15 mg-t-20" ]
                    [ h6 [ class "slim-card-title mg-b-10" ]
                        [ text "Sell ETH" ]
                    , h6 [ class "text-right mg-b-10" ]
                        [ text "Balance: "
                        , span [ id "asset_balance" ]
                            [ text (String.fromInt model.balanceAsset) ]
                        , text " ETH"
                        ]
                    , div [ class "input-group form-group" ]
                        [ input [ class "form-control", id "sell_price", placeholder "Price", type_ "number", value (String.fromInt model.sellPrice), onInput ChangeSellPrice ]
                            []
                        , div [ class "input-group-append" ]
                            [ span [ class "input-group-text" ]
                                [ text "BTC" ]
                            ]
                        ]
                    , div [ class "input-group form-group" ]
                        [ input [ class "form-control", id "sell_amount", placeholder "Amount", type_ "number", value (printSellAmount model), onInput ChangeSellAmount ]
                            []
                        , div [ class "input-group-append" ]
                            [ span [ class "input-group-text" ]
                                [ text "ETH" ]
                            ]
                        ]
                    , div [ class "form-group" ]
                        [ div [ class "text-right" ]
                            [ button [ class "btn btn-outline-primary btn-sm mg-b-10", onClick Sell25 ]
                                [ text "25%" ]
                            , button [ class "btn btn-outline-primary btn-sm mg-b-10", onClick Sell50 ]
                                [ text "50%" ]
                            , button [ class "btn btn-outline-primary btn-sm mg-b-10", onClick Sell75 ]
                                [ text "75%" ]
                            , button [ class "btn btn-outline-primary btn-sm mg-b-10", onClick Sell100 ]
                                [ text "100%" ]
                            ]
                        ]
                    , div [ class "input-group form-group" ]
                        [ input [ class "form-control", id "total_sell", placeholder "Total", attribute "readonly" "", type_ "number", value (printSellTotal model) ]
                            []
                        , div [ class "input-group-append" ]
                            [ span [ class "input-group-text" ]
                                [ text "BTC" ]
                            ]
                        ]
                    , button [ class "btn btn-danger btn-block", disabled model.isSellDisabled, onClick Sell ]
                        [ text "SELL ETH" ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
