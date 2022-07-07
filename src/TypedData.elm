module TypedData exposing (TypedData, clearCompletedWord, initial, updateData)

import Keyboard.Event exposing (KeyboardEvent)


type alias TypedData =
    { typedSoFar : String
    , completedWord : Maybe String
    , completedWordQueue : List String
    , lastChar : String
    }


initial =
    TypedData "" Nothing [] ""


updateData : KeyboardEvent -> TypedData -> TypedData
updateData e td =
    if e.ctrlKey then
        case e.key of
            Just "Backspace" ->
                { td | typedSoFar = "" }

            _ ->
                td

    else
        case e.key of
            Just c ->
                receivedKeyPress c td

            _ ->
                td


receivedKeyPress : String -> TypedData -> TypedData
receivedKeyPress c typedData =
    case ( String.length c, c ) of
        ( _, "Enter" ) ->
            pushWord typedData

        ( _, " " ) ->
            pushWord typedData

        ( _, "Backspace" ) ->
            { typedData | typedSoFar = String.dropRight 1 typedData.typedSoFar }

        ( 1, _ ) ->
            { typedData | typedSoFar = typedData.typedSoFar ++ c, lastChar = c }

        _ ->
            { typedData | lastChar = c }


pushWord : TypedData -> TypedData
pushWord td =
    case td.completedWord of
        Just _ ->
            { td
                | typedSoFar = ""
                , completedWordQueue = td.typedSoFar :: td.completedWordQueue
            }

        Nothing ->
            { td
                | typedSoFar = ""
                , completedWord = Just td.typedSoFar
            }


clearCompletedWord : TypedData -> TypedData
clearCompletedWord typedData =
    { typedData
        | completedWord = List.head typedData.completedWordQueue
        , completedWordQueue = List.drop 1 typedData.completedWordQueue
    }
