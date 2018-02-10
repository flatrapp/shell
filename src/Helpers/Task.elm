module Helpers.Task exposing (..)

import Dict exposing (Dict)
import Globals.Types exposing (Authentication)
import Helpers.Authentication exposing (authenticationHeaders)
import Helpers.Functions exposing (..)
import Helpers.User exposing (UserInfo)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required)
import Json.Encode as Encode
import Set exposing (Set)
import Time
import Time.DateTime as DateTime exposing (DateTime)


requestTimeout : Float
requestTimeout =
    5 * Time.second



-- ////////// GENERIC //////////


type alias Task =
    { id : Int
    , title : String
    , description : String
    , frequency : Int
    , completionTime : Int
    , users : Set Int
    , currentTurn : Maybe Turn
    , upcomingTurns : List Turn
    }


type alias TaskUser =
    { id : Int
    , title : String
    , description : String
    , frequency : Int
    , completionTime : Int
    , users : Dict Int UserInfo
    , currentTurn : Maybe TurnUser
    , upcomingTurns : List TurnUser
    }


type alias RequestTask =
    { title : String
    , description : String
    , frequency : Int
    , completionTime : Int
    , users : Set Int
    }


type alias Turn =
    { userId : Int
    , start : DateTime
    }


type alias TurnUser =
    { user : UserInfo
    , start : DateTime
    }


requestTaskEncode : Maybe Int -> RequestTask -> Encode.Value
requestTaskEncode maybeId task =
    Encode.object <|
        (case maybeId of
            Just id ->
                [ ( "id", Encode.int id ) ]

            Nothing ->
                []
        )
            ++ [ ( "title", Encode.string task.title )
               , ( "description", Encode.string task.description )
               , ( "frequency", Encode.int task.frequency )
               , ( "completionTime", Encode.int task.completionTime )
               , ( "users", Encode.list <| List.map Encode.int <| Set.toList task.users )
               ]


taskDecoder : Decode.Decoder Task
taskDecoder =
    DecodePipeline.decode
        (\id title description frequency completionTime users currentTurn upcomingTurns ->
            { id = id
            , title = title
            , description = description
            , frequency = frequency
            , completionTime = completionTime
            , users = Set.fromList users
            , currentTurn = currentTurn
            , upcomingTurns = upcomingTurns
            }
        )
        |> DecodePipeline.required "id" Decode.int
        |> DecodePipeline.required "title" Decode.string
        |> DecodePipeline.required "description" Decode.string
        |> DecodePipeline.required "frequency" Decode.int
        |> DecodePipeline.required "completionTime" Decode.int
        |> DecodePipeline.required "users" (Decode.list Decode.int)
        |> DecodePipeline.required "currentTurn" (Decode.nullable turnDecoder)
        |> DecodePipeline.required "upcomingTurns" (Decode.list turnDecoder)


turnDecoder : Decode.Decoder Turn
turnDecoder =
    DecodePipeline.decode
        (\id start -> { userId = id, start = start })
        |> DecodePipeline.required "userId" Decode.int
        |> DecodePipeline.required "startDate" dateDecoder


dateDecoder : Decode.Decoder DateTime
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case DateTime.fromISO8601 raw of
                    Ok date ->
                        Decode.succeed date

                    Err error ->
                        Decode.fail error
            )


tasksMapUser : Dict Int UserInfo -> List Task -> Maybe (List TaskUser)
tasksMapUser users tasks =
    maybeList <| List.map (taskMapUser users) tasks


taskMapUser : Dict Int UserInfo -> Task -> Maybe TaskUser
taskMapUser users task =
    let
        maybeCurrentTurn =
            case task.currentTurn of
                Nothing ->
                    Nothing

                Just currentTurn ->
                    Just <| turnMapUser users currentTurn

        maybeUpcomingTurns =
            List.map (\t -> turnMapUser users t) task.upcomingTurns
                |> maybeList
    in
    case
        maybe2
            ( userInfoFromIds users <| Set.toList task.users
            , maybeUpcomingTurns
            )
    of
        Nothing ->
            Nothing

        Just ( taskUsers, upcomingTurns ) ->
            case maybeCurrentTurn of
                Nothing ->
                    -- No user matching error, there was no turn to begin with
                    Just <| toTaskUser taskUsers Nothing upcomingTurns task

                Just Nothing ->
                    -- User matching error
                    Nothing

                Just (Just currentTurn) ->
                    Just <| toTaskUser taskUsers (Just currentTurn) upcomingTurns task


toTaskUser : Dict Int UserInfo -> Maybe TurnUser -> List TurnUser -> Task -> TaskUser
toTaskUser users currentTurn upcomingTurns task =
    { task
        | users = users
        , currentTurn = currentTurn
        , upcomingTurns = upcomingTurns
    }


turnMapUser : Dict Int UserInfo -> Turn -> Maybe TurnUser
turnMapUser users turn =
    case Dict.get turn.userId users of
        Nothing ->
            Nothing

        Just user ->
            Just { user = user, start = turn.start }


userInfoFromIds : Dict Int UserInfo -> List Int -> Maybe (Dict Int UserInfo)
userInfoFromIds userDict userList =
    List.foldl
        (\id maybeDst ->
            let
                maybeUser =
                    Dict.get id userDict
            in
            case maybe2 ( maybeDst, maybeUser ) of
                Nothing ->
                    Nothing

                Just ( dst, user ) ->
                    Just <| Dict.insert user.id user dst
        )
        (Just Dict.empty)
        userList



-- ////////// CREATE INVITATION //////////


type CreateTaskResponse
    = CreateTaskSuccessResponse Task
    | CreateTaskErrorResponse { error : CreateTaskError, message : String }
    | CreateTaskInvalidResponse
    | CreateTaskHttpError Http.Error


type CreateTaskError
    = UnknownCreateTaskError String


createTaskRequest : Authentication -> RequestTask -> Http.Request CreateTaskResponse
createTaskRequest auth requestTask =
    Http.request
        { body = requestTaskEncode Nothing requestTask |> Http.jsonBody
        , expect =
            Http.expectJson <|
                Decode.map (\t -> CreateTaskSuccessResponse t) taskDecoder
        , headers = authenticationHeaders auth
        , method = "POST"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/tasks"
        , withCredentials = False
        }


createTaskResponseDecode : Result Http.Error CreateTaskResponse -> CreateTaskResponse
createTaskResponseDecode res =
    responseDecode
        createTaskErrorDecoder
        CreateTaskInvalidResponse
        CreateTaskHttpError
        res


createTaskErrorDecoder : Decode.Decoder CreateTaskResponse
createTaskErrorDecoder =
    errorDecoder
        (\code message ->
            CreateTaskErrorResponse
                { error =
                    case code of
                        -- TODO: Parse errors for real
                        _ ->
                            UnknownCreateTaskError code
                , message = message
                }
        )



-- ////////// LIST TASKS //////////


type ListTasksResponse
    = ListTasksSuccessResponse (Dict Int Task)
    | ListTasksErrorResponse { error : ListTasksError, message : String }
    | ListTasksInvalidResponse
    | ListTasksHttpError Http.Error


type ListTasksError
    = UnknownListTasksError String


listTasksRequest : Authentication -> Http.Request ListTasksResponse
listTasksRequest auth =
    Http.request
        { body = Http.emptyBody
        , expect =
            Http.expectJson <|
                Decode.map
                    (\tasks ->
                        tasks
                            |> List.foldl (\task dict -> Dict.insert task.id task dict) Dict.empty
                            |> ListTasksSuccessResponse
                    )
                    (Decode.list taskDecoder)
        , headers = authenticationHeaders auth
        , method = "GET"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/tasks"
        , withCredentials = False
        }


listTasksResponseDecode : Result Http.Error ListTasksResponse -> ListTasksResponse
listTasksResponseDecode res =
    responseDecode
        listTasksErrorDecoder
        ListTasksInvalidResponse
        ListTasksHttpError
        res


listTasksErrorDecoder : Decode.Decoder ListTasksResponse
listTasksErrorDecoder =
    errorDecoder <|
        \code message ->
            ListTasksErrorResponse
                { error =
                    case code of
                        _ ->
                            UnknownListTasksError code
                , message = message
                }



-- ////////// UPDATE TASK //////////


type UpdateTaskResponse
    = UpdateTaskSuccessResponse Task
    | UpdateTaskErrorResponse { error : UpdateTaskError, message : String }
    | UpdateTaskInvalidResponse
    | UpdateTaskHttpError Http.Error


type UpdateTaskError
    = UnknownUpdateTaskError String


updateTaskRequest : Authentication -> Int -> RequestTask -> Http.Request UpdateTaskResponse
updateTaskRequest auth id task =
    Http.request
        { body = Http.jsonBody <| requestTaskEncode Nothing task
        , expect = Http.expectJson <| Decode.map UpdateTaskSuccessResponse taskDecoder
        , headers = authenticationHeaders auth
        , method = "PUT"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/tasks/" ++ toString id
        , withCredentials = False
        }


updateTaskResponseDecode : Result Http.Error UpdateTaskResponse -> UpdateTaskResponse
updateTaskResponseDecode res =
    responseDecode
        updateTaskErrorDecoder
        UpdateTaskInvalidResponse
        UpdateTaskHttpError
        res


updateTaskErrorDecoder : Decode.Decoder UpdateTaskResponse
updateTaskErrorDecoder =
    errorDecoder <|
        \code message ->
            UpdateTaskErrorResponse
                { error =
                    case code of
                        _ ->
                            UnknownUpdateTaskError code
                , message = message
                }



-- -- ////////// DELETE INVITATION //////////
--
--
-- type DeleteTaskResponse
--     = DeleteTaskSuccessResponse
--     | DeleteTaskErrorResponse { error : DeleteTaskError, message : String }
--     | DeleteTaskInvalidResponse
--     | DeleteTaskHttpError Http.Error
--
--
-- type DeleteTaskError
--     = UnknownDeleteTaskError String
--
--
-- deleteTaskRequest : String -> Authentication -> Int -> Http.Request String
-- deleteTaskRequest serverUrl auth invitationId =
--     Http.request
--         { body = Http.emptyBody
--         , expect = Http.expectString
--         , headers = authenticationHeaders auth
--         , method = "DELETE"
--         , timeout = Just requestTimeout
--         , url = serverUrl ++ "/invitations/" ++ toString invitationId
--         , withCredentials = False
--         }
--
--
-- deleteTaskResponseDecode : Result Http.Error DeleteTaskResponse -> DeleteTaskResponse
-- deleteTaskResponseDecode res =
--     flexibleResponseDecode
--         -- Ignore response string, as long as it's in the 200-300 range
--         (\_ -> DeleteTaskSuccessResponse)
--         deleteTaskErrorDecoder
--         DeleteTaskInvalidResponse
--         DeleteTaskHttpError
--         res
--
--
-- deleteTaskErrorDecoder : Decode.Decoder DeleteTaskResponse
-- deleteTaskErrorDecoder =
--     errorDecoder <|
--         \code message ->
--             DeleteTaskErrorResponse
--                 { error =
--                     case code of
--                         _ ->
--                             UnknownDeleteTaskError code
--                 , message = message
--                 }
