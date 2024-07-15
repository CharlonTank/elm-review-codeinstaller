module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Install.ClauseInCase
import Install.FieldInTypeAlias
import Install.Function.InsertFunction
import Install.Import
import Install.Initializer
import Install.TypeVariant
import Review.Rule exposing (Rule)


config : List Rule
config =
    List.concat
        [ typesRules
        , backendRules
        , frontendRules
        , envRules
        ]


typesRules : List Rule
typesRules =
    [ -- Imports
      [ Install.Import.init "Types"
            [ { moduleToImport = "Dict", alias_ = Nothing, exposedValues = Just [ "Dict" ] }
            , { moduleToImport = "Auth.Common", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Lamdera", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Url", alias_ = Nothing, exposedValues = Just [ "Url" ] }
            ]
            |> Install.Import.makeRule
      ]

    -- FrontendModel =
    , List.map (Install.FieldInTypeAlias.makeRule "Types" "FrontendModel")
        [ "authFlow : Auth.Common.Flow"
        , "authRedirectBaseUrl : Url"
        , "login : LoginState"
        , "currentUser : Maybe UserFrontend"
        ]

    -- BackendModel =
    , List.map (Install.FieldInTypeAlias.makeRule "Types" "BackendModel")
        [ "pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth"
        , "sessions : Dict Lamdera.SessionId Auth.Common.UserInfo"
        , "users : Dict Email User"
        ]

    -- FrontendMsg =
    , List.map (Install.TypeVariant.makeRule "Types" "FrontendMsg")
        [ "GoogleSigninRequested"
        , "Logout"
        ]

    -- ToBackend =
    , List.map (Install.TypeVariant.makeRule "Types" "ToBackend")
        [ "AuthToBackend Auth.Common.ToBackend"
        , "GetUserToBackend"
        , "LoggedOut"
        ]

    -- BackendMsg =
    , List.map (Install.TypeVariant.makeRule "Types" "BackendMsg")
        [ "AuthBackendMsg Auth.Common.BackendMsg"
        ]

    -- ToFrontend =
    , List.map (Install.TypeVariant.makeRule "Types" "ToFrontend")
        [ "AuthToFrontend Auth.Common.ToFrontend"
        , "AuthSuccess Auth.Common.UserInfo"
        , "UserInfoMsg (Maybe Auth.Common.UserInfo)"
        , "UserDataToFrontend UserFrontend"
        ]

    -- Other types
    , [ Install.Function.InsertFunction.init "Types"
            "LoginState"
            """type LoginState = JustArrived | NotLogged Bool | LoginTokenSent | LoggedIn Auth.Common.UserInfo"""
      , Install.Function.InsertFunction.init "Types"
            "UserFrontend"
            """type alias UserFrontend = { email : Email }"""
      , Install.Function.InsertFunction.init
            "Types"
            "User"
            """type alias User =
    { email : Email }"""
      , Install.Function.InsertFunction.init
            "Types"
            "Email"
            """type alias Email = String"""
      ]
        |> List.map Install.Function.InsertFunction.makeRule
    ]
        |> List.concat


backendRules : List Rule
backendRules =
    [ -- Imports
      [ Install.Import.init "Backend"
            [ { moduleToImport = "Auth.Method.EmailMagicLink", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Auth.Method.OAuthGithub", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Auth.Method.OAuthGoogle", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Auth.Flow", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Auth.Common", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Lamdera", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Env", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Dict", alias_ = Nothing, exposedValues = Just [ "Dict" ] }
            , { moduleToImport = "Time", alias_ = Nothing, exposedValues = Just [ "Posix" ] }
            ]
            |> Install.Import.makeRule
      ]

    -- init :
    , [ Install.Initializer.makeRule "Backend" "init" "pendingAuths" "Dict.empty"
      , Install.Initializer.makeRule "Backend" "init" "sessions" "Dict.empty"
      , Install.Initializer.makeRule "Backend" "init" "users" "Dict.empty"
      ]
    , [ -- update :
        Install.ClauseInCase.init "Backend"
            "update"
            "AuthBackendMsg authMsg"
            "Auth.Flow.backendUpdate (backendConfig model) authMsg"

      -- updateFromFrontend :
      , Install.ClauseInCase.init "Backend"
            "updateFromFrontend"
            "AuthToBackend authToBackend"
            "Auth.Flow.updateFromFrontend (backendConfig model) clientId sessionId authToBackend model"
      , Install.ClauseInCase.init "Backend"
            "updateFromFrontend"
            "GetUserToBackend"
            """case Dict.get sessionId model.sessions of
            Just userInfo ->
                case Dict.get userInfo.email model.users of
                    Just user ->
                        ( model, Cmd.batch [ Lamdera.sendToFrontend clientId <| UserInfoMsg <| Just userInfo, Lamdera.sendToFrontend clientId <| UserDataToFrontend <| userToFrontend user ])
                    Nothing ->
                        let
                            user = createUser userInfo
                            newModel = insertUser userInfo.email user model
                        in
                        ( newModel, Cmd.batch [ Lamdera.sendToFrontend clientId <| UserInfoMsg <| Just userInfo, Lamdera.sendToFrontend clientId <| UserDataToFrontend <| userToFrontend user ])
            Nothing ->
                ( model, Lamdera.sendToFrontend clientId <| UserInfoMsg Nothing )"""
      , Install.ClauseInCase.init "Backend"
            "updateFromFrontend"
            "LoggedOut"
            """( { model | sessions = Dict.remove sessionId model.sessions }, Cmd.none )"""
      ]
        |> List.map Install.ClauseInCase.makeRule

    -- Other Backend functions
    , [ Install.Function.InsertFunction.init "Backend" "userToFrontend" """userToFrontend : User -> UserFrontend
userToFrontend user =
    { email = user.email }"""
      , Install.Function.InsertFunction.init "Backend" "insertUser" """insertUser : Email -> User -> BackendModel -> BackendModel
insertUser email newUser model =
    { model | users = Dict.insert email newUser model.users }"""
      , Install.Function.InsertFunction.init "Backend" "createUser" """createUser : Auth.Common.UserInfo -> User
createUser userInfo =
    { email = userInfo.email }"""
      , Install.Function.InsertFunction.init "Backend" "config" """config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { toBackend = AuthToBackend
    , toFrontend = AuthToFrontend
    , backendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , renewSession = renewSession
    , methods = [ Auth.Method.OAuthGoogle.configuration Env.googleAppClientId Env.googleAppClientSecret ] }"""
      , Install.Function.InsertFunction.init "Backend" "backendConfig" """backendConfig : BackendModel -> Auth.Flow.BackendUpdateConfig FrontendMsg BackendMsg ToFrontend FrontendModel BackendModel
backendConfig model =
    { asToFrontend = AuthToFrontend
    , asBackendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , backendModel = model
    , loadMethod = Auth.Flow.methodLoader config.methods
    , handleAuthSuccess = handleAuthSuccess model
    , isDev = True
    , renewSession = renewSession
    , logout = logout }"""
      , Install.Function.InsertFunction.init "Backend" "logout" """logout : Lamdera.SessionId -> Lamdera.ClientId -> BackendModel -> ( BackendModel, Cmd msg )
logout sessionId _ model =
    ( { model | sessions = model.sessions |> Dict.remove sessionId }, Cmd.none )"""
      , Install.Function.InsertFunction.init "Backend" "handleAuthSuccess" """handleAuthSuccess : BackendModel -> Lamdera.SessionId -> Lamdera.ClientId -> Auth.Common.UserInfo -> Auth.Common.MethodId -> Maybe Auth.Common.Token -> Time.Posix -> ( BackendModel, Cmd BackendMsg )
handleAuthSuccess backendModel sessionId clientId userInfo _ _ _ =
    let
        sessionsWithOutThisOne : Dict Lamdera.SessionId Auth.Common.UserInfo
        sessionsWithOutThisOne =
            Dict.filter (\\_ { email } -> email /= userInfo.email) backendModel.sessions
        newSessions =
            Dict.insert sessionId userInfo sessionsWithOutThisOne
        response =
            AuthSuccess userInfo
    in
    ( { backendModel | sessions = newSessions }, Cmd.batch [ Lamdera.sendToFrontend clientId response ] )"""

      -- renewSession :
      , Install.Function.InsertFunction.init "Backend" "renewSession" """renewSession : Lamdera.SessionId -> Lamdera.ClientId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
renewSession _ _ model =
    ( model, Cmd.none )"""
      ]
        |> List.map Install.Function.InsertFunction.makeRule
    ]
        |> List.concat


frontendRules : List Rule
frontendRules =
    [ [ Install.Import.init "Frontend"
            [ { moduleToImport = "Auth.Method.OAuthGoogle", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Auth.Flow", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Auth.Common", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Html", alias_ = Nothing, exposedValues = Nothing }
            , { moduleToImport = "Html.Events", alias_ = Just "HE", exposedValues = Nothing }
            , { moduleToImport = "Browser", alias_ = Nothing, exposedValues = Nothing }
            ]
            |> Install.Import.makeRule
      ]
    , [ Install.Initializer.makeRule "Frontend" "init" "authFlow" "Auth.Common.Idle"
      , Install.Initializer.makeRule "Frontend" "init" "authRedirectBaseUrl" "{ url | query = Nothing, fragment = Nothing }"
      , Install.Initializer.makeRule "Frontend" "init" "login" "JustArrived"
      , Install.Initializer.makeRule "Frontend" "init" "currentUser" "Nothing"
      ]
    , [ -- update :
        Install.ClauseInCase.init "Frontend"
            "update"
            "GoogleSigninRequested"
            """Auth.Flow.signInRequested "OAuthGoogle" { model | login = NotLogged True } Nothing
    |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend)"""
      , Install.ClauseInCase.init "Frontend"
            "update"
            "Logout"
            """( { model | login = NotLogged False }, Lamdera.sendToBackend LoggedOut )"""

      -- updateFromBackend :
      , Install.ClauseInCase.init "Frontend"
            "updateFromBackend"
            "AuthToFrontend authToFrontendMsg"
            "authUpdateFromBackend authToFrontendMsg model"
      , Install.ClauseInCase.init "Frontend"
            "updateFromBackend"
            "AuthSuccess userInfo"
            """( { model | login = LoggedIn userInfo }, Cmd.batch [ Nav.pushUrl model.key "/", Lamdera.sendToBackend GetUserToBackend ] )"""
      , Install.ClauseInCase.init "Frontend"
            "updateFromBackend"
            "UserInfoMsg mUserinfo"
            """case mUserinfo of
            Just userInfo ->
                ( { model | login = LoggedIn userInfo }, Cmd.none )

            Nothing ->
                ( { model | login = NotLogged False }, Cmd.none )"""
      , Install.ClauseInCase.init "Frontend"
            "updateFromBackend"
            "UserDataToFrontend currentUser"
            """( { model | currentUser = Just currentUser }, Cmd.none )"""
      ]
        |> List.map Install.ClauseInCase.makeRule

    -- Other Frontend functions
    , [ Install.Function.InsertFunction.init "Frontend"
            "authUpdateFromBackend"
            """authUpdateFromBackend : Auth.Common.ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
authUpdateFromBackend authToFrontendMsg model =
    case authToFrontendMsg of
        Auth.Common.AuthInitiateSignin url ->
            Auth.Flow.startProviderSignin url model

        Auth.Common.AuthError err ->
            Auth.Flow.setError model err

        Auth.Common.AuthSessionChallenge _ ->
            ( model, Cmd.none )"""
      , Install.Function.InsertFunction.init "Frontend"
            "viewWithAuth"
            """viewWithAuth : Model -> Browser.Document FrontendMsg
viewWithAuth model =
    { title = "View Auth Test"
    , body =
        [ Html.button
            [ HE.onClick GoogleSigninRequested ]
            [ Html.text "Sign in with Google" ]
        ]
    }"""
      , Install.Function.InsertFunction.init "Frontend"
            "appWithAuth"
            """{-| replace with your app function to try it out -}
appWithAuth =
    Lamdera.frontend
        { init = initWithAuth
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = always Sub.none
        , view = viewWithAuth
        }"""
            |> Install.Function.InsertFunction.withInsertAfter "app"
      , Install.Function.InsertFunction.init "Frontend"
            "initWithAuth"
            """initWithAuth : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
initWithAuth url key =
    let
        ( model, cmds ) =
            init url key
    in
    authCallbackCmd model url key
        |> Tuple.mapSecond (\\cmd -> Cmd.batch [ cmds, cmd, Lamdera.sendToBackend GetUserToBackend ])"""
      , Install.Function.InsertFunction.init "Frontend"
            "authCallbackCmd"
            """authCallbackCmd : FrontendModel -> Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
authCallbackCmd model url key =
    let
        { path } =
            url
    in
    case path of
        "/login/OAuthGoogle/callback" ->
            callbackForGoogleAuth model url key

        _ ->
            ( model, Cmd.none )"""
      , Install.Function.InsertFunction.init "Frontend"
            "callbackForGoogleAuth"
            """callbackForGoogleAuth : FrontendModel -> Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
callbackForGoogleAuth model url key =
    let
        ( authM, authCmd ) =
            Auth.Flow.init model
                "OAuthGoogle"
                url
                key
                (\\msg -> Lamdera.sendToBackend (AuthToBackend msg))
    in
    ( authM, authCmd )"""
      ]
        |> List.map Install.Function.InsertFunction.makeRule
    ]
        |> List.concat


envRules : List Rule
envRules =
    [ Install.Function.InsertFunction.init "Env"
        "githubAppClientId"
        """githubAppClientId : String
githubAppClientId =
    \"\""""
    , Install.Function.InsertFunction.init "Env"
        "githubAppClientSecret"
        """githubAppClientSecret : String
githubAppClientSecret =
    \"\""""
    , Install.Function.InsertFunction.init "Env"
        "googleAppClientId"
        """googleAppClientId : String
googleAppClientId =
    \"\""""
    , Install.Function.InsertFunction.init "Env"
        "googleAppClientSecret"
        """googleAppClientSecret : String
googleAppClientSecret =
    \"\""""
    ]
        |> List.map Install.Function.InsertFunction.makeRule
