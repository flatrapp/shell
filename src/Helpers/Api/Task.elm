module Helpers.Api.Task exposing (..)

import Dict exposing (Dict)
import Globals.Types exposing (Authentication)
import Helpers.Api.User exposing (UserInfo)
import Helpers.Authentication exposing (authenticationHeaders)
import Helpers.Functions exposing (..)
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



-- ////////// CREATE TASK //////////


type CreateTaskResponse
    = CreateTaskSuccessResponse Task
    | CreateTaskErrorResponse { error : CreateTaskError, message : String }
    | CreateTaskInvalidResponse
    | CreateTaskHttpError Http.Error


type CreateTaskError
    = CreateTaskUnauthorizedError
    | UnknownCreateTaskError String


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
                        "unauthorized" ->
                            CreateTaskUnauthorizedError

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
    = ListTasksUnauthorizedError
    | UnknownListTasksError String


listTasksRequest : Authentication -> Http.Request ListTasksResponse
listTasksRequest auth =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson listTasksSuccessDecoder
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


listTasksSuccessDecoder : Decode.Decoder ListTasksResponse
listTasksSuccessDecoder =
    Decode.map
        (\tasks ->
            tasks
                |> List.foldl (\task dict -> Dict.insert task.id task dict) Dict.empty
                |> ListTasksSuccessResponse
        )
        (Decode.list taskDecoder)


listTasksErrorDecoder : Decode.Decoder ListTasksResponse
listTasksErrorDecoder =
    errorDecoder <|
        \code message ->
            ListTasksErrorResponse
                { error =
                    case code of
                        "unauthorized" ->
                            ListTasksUnauthorizedError

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
    = UpdateTaskNotFoundError
    | UpdateTaskUnauthorizedError
    | UnknownUpdateTaskError String


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
                        "not_found" ->
                            UpdateTaskNotFoundError

                        "unauthorized" ->
                            UpdateTaskUnauthorizedError

                        _ ->
                            UnknownUpdateTaskError code
                , message = message
                }



-- ////////// DELETE TASK //////////


type DeleteTaskResponse
    = DeleteTaskSuccessResponse
    | DeleteTaskErrorResponse { error : DeleteTaskError, message : String }
    | DeleteTaskInvalidResponse
    | DeleteTaskHttpError Http.Error


type DeleteTaskError
    = DeleteTaskNotFoundError
    | DeleteTaskUnauthorizedError
    | UnknownDeleteTaskError String


deleteTaskRequest : Authentication -> Int -> Http.Request DeleteTaskResponse
deleteTaskRequest auth taskId =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok DeleteTaskSuccessResponse)
        , headers = authenticationHeaders auth
        , method = "DELETE"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/tasks/" ++ toString taskId
        , withCredentials = False
        }


deleteTaskResponseDecode : Result Http.Error DeleteTaskResponse -> DeleteTaskResponse
deleteTaskResponseDecode res =
    responseDecode
        deleteTaskErrorDecoder
        DeleteTaskInvalidResponse
        DeleteTaskHttpError
        res


deleteTaskErrorDecoder : Decode.Decoder DeleteTaskResponse
deleteTaskErrorDecoder =
    errorDecoder <|
        \code message ->
            DeleteTaskErrorResponse
                { error =
                    case code of
                        "not_found" ->
                            DeleteTaskNotFoundError

                        "unauthorized" ->
                            DeleteTaskUnauthorizedError

                        _ ->
                            UnknownDeleteTaskError code
                , message = message
                }



-- ////////// FINISH TASK TURN //////////


type FinishTurnResponse
    = FinishTurnSuccessResponse
    | FinishTurnErrorResponse { error : FinishTurnError, message : String }
    | FinishTurnInvalidResponse
    | FinishTurnHttpError Http.Error


type FinishTurnError
    = FinishTurnUnauthorizedError
    | FinishTurnNotFoundError
    | UnknownFinishTurnError String


finishTurnRequest : Authentication -> Int -> Http.Request FinishTurnResponse
finishTurnRequest auth taskId =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok FinishTurnSuccessResponse)
        , headers = authenticationHeaders auth
        , method = "POST"
        , timeout = Just requestTimeout
        , url = auth.serverUrl ++ "/tasks/" ++ toString taskId ++ "/finish"
        , withCredentials = False
        }


finishTurnErrorDecoder : Decode.Decoder FinishTurnResponse
finishTurnErrorDecoder =
    errorDecoder
        (\code message ->
            FinishTurnErrorResponse
                { error =
                    case code of
                        "unauthorized" ->
                            FinishTurnUnauthorizedError

                        "task_not_found" ->
                            FinishTurnNotFoundError

                        _ ->
                            UnknownFinishTurnError code
                , message = message
                }
        )


finishTurnResponseDecode : Result Http.Error FinishTurnResponse -> FinishTurnResponse
finishTurnResponseDecode res =
    responseDecode
        finishTurnErrorDecoder
        FinishTurnInvalidResponse
        FinishTurnHttpError
        res
