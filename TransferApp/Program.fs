module OpilioCraft.DAM.TransferApp.Main

open System

open Argu

open OpilioCraft.FSharp.Prelude
open OpilioCraft.DAM.TransferApp.Runtime

// commandline definition
type CheckArgs =
    | [<NoCommandLine>] Dummy // enforce subcommand style

    interface IArgParserTemplate with
        member x.Usage = ""

type SetupArgs =
    | [<NoCommandLine>] Dummy // enforce subcommand style

    interface IArgParserTemplate with
        member x.Usage = ""

type RunArgs =
    | [<MainCommand; ExactlyOnce; Last>] Profile of ProfileName:string
    | Target_Path of Path:string
    | Slow_Down
    
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Profile _ -> "name of transfer profile to be used"
            | Target_Path _ -> "overwrite configured transfer target path"
            | Slow_Down -> "slow down execution for testing purposes"

[<NoAppSettings>]
type Arguments =
    | [<CliPrefix(CliPrefix.None)>] Check of ParseResults<CheckArgs>
    | [<CliPrefix(CliPrefix.None)>] Setup of ParseResults<SetupArgs>
    | [<CliPrefix(CliPrefix.None)>] Run of ParseResults<RunArgs>

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Check _   -> "check current setup"
            | Setup _   -> "run setup and write example configuration"
            | Run _     -> "run transfer with specified profile"


[<EntryPoint>]
let main args =
    let errorHandler = ProcessExiter(colorizer = function | ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "OpilioCraft.DAM.TransferApp.exe", errorHandler = errorHandler)
    
    match parser.ParseCommandLine args with
    | p when p.Contains(Check) -> Runtime.runCheck ()
    | p when p.Contains(Setup) -> Runtime.runSetup ()

    | p when p.Contains(Run) ->
        try
            UserSettings.verifySettings () // throws Exception on error
            let config = UserSettings.config ()
            let profilesCatalogue = UserSettings.profilesCatalogue ()

            let runArgs = p.GetResult(Run)
            let profileName = runArgs.GetResult(Profile)
            let targetPath = runArgs.TryGetResult(Target_Path) |> Option.defaultValue config.Locations.["Incoming"]

            if IO.Directory.Exists(targetPath)
            then
                match profilesCatalogue |> Map.tryFind profileName with
                | Some profile -> Runtime.runTransfer profile targetPath (runArgs.Contains(Slow_Down))
                | _ -> Logging.emitError UnknownProfile $"[ERROR] unknown profile: {profileName}"
            else
                Logging.emitError (InvalidTargetPath targetPath) $"[ERROR] invalid target path: {targetPath}"

        with
        | :? IncompleteSetupException as exn -> Error SetupPending
        | :? InvalidUserSettingsException as exn -> Error (InvalidConfiguration(exn.Message))
        | _ -> Error RuntimeError
    
    | _ -> Logging.emitError UsageError $"{parser.PrintUsage()}"

    |> function
        | Ok _ -> 0
        | Error reason -> reason.ReturnCode
