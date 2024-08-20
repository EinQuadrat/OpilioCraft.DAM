module OpilioCraft.DAM.TransferApp.UI.App

open System

open Elmish.WPF

open OpilioCraft.FSharp.Prelude
open OpilioCraft.DAM.TransferApp
        
[<EntryPoint; STAThread>]
let main _ =
    try
        UserSettings.verifySettings () // throws Exception on error
        
        Program.mkProgramWpf MainModel.init MainModel.update MainModel.bindings
        |> Program.runWindow (new MainWindow())
        |> ignore |> Ok

    with
    | :? IncompleteSetupException as exn ->
        sprintf "%s\n\n%s: %s" (Resources.I18N["IncompleteSetup"]) (Resources.I18N["MissingFile"]) exn.MissingFile
        |> showError; Error SetupPending

    | :? InvalidUserSettingsException as exn ->
        sprintf "%s\n\n%s\n%s" Resources.I18N["CorruptSetup"] exn.File exn.ErrorMessage
        |> showError; Error (InvalidConfiguration(exn.Message))

    | exn ->
        Resources.errorMessageTemplate exn.Message
        |> showError; Error RuntimeError
    
    |> function
        | Ok _ -> 0
        | Error reason -> reason.ReturnCode
