namespace OpilioCraft.DAM.TransferApp.Runtime

open System
open System.IO
open System.Text.Json
open Microsoft.VisualBasic.CompilerServices

open OpilioCraft.FSharp.Prelude
open OpilioCraft.PortableDevices

open OpilioCraft.DAM.TransferApp

// ------------------------------------------------------------------------------------------------

type ProgressUpdateEventArgs =
    | Transferring of string
    | Transferred of string
    | Skipped of string

type TransferWorkerEvent =
    | TransferInitiated
    | ScanCompleted of NumberOfFilesFound:int
    | ProgressUpdate of Details:ProgressUpdateEventArgs
    | TransferCompleted
    | TransferError of System.Exception

// ------------------------------------------------------------------------------------------------

type ITransferWorker =
    abstract member Run : unit -> unit
    abstract member WorkerEvent : IEvent<TransferWorkerEvent>

// ------------------------------------------------------------------------------------------------

[<AbstractClass>]
type TransferWorker<'ItemType> (transferProfile : TransferProfile, targetDir : string) =
    // event queues
    let workerEvent = new Event<_>()

    // .NET events
    [<CLIEvent>]
    member _.WorkerEvent = workerEvent.Publish

    // trigger methods
    member _.TriggerEvent eventType = workerEvent.Trigger(eventType)

    // properties
    member _.TransferProfile = transferProfile
    member _.TargetDir = targetDir

    member _.OverrideOption = transferProfile.Options.Contains TransferOption.Override

    // item type specific methods
    abstract member ScanSource : SourceDefinition -> 'ItemType list
    abstract member TransferFile : 'ItemType -> unit

    member x.TriggerProgressUpdate details = details |> ProgressUpdate |> x.TriggerEvent

    // factory method
    static member CreateWorker (transferProfile : TransferProfile) (targetDir : string) (slowDown : bool) : ITransferWorker =
        match transferProfile.Device with
        | Drive ->
            new DriveTransferWorker(transferProfile, targetDir, slowDown) :> ITransferWorker
        | Portable deviceName ->
            let portableDevice = PortableDeviceManager.GetDeviceByName(deviceName, refresh = true)
            new PortableTransferWorker(portableDevice, transferProfile, targetDir) :> ITransferWorker

    // interfaces
    interface ITransferWorker with
        member x.WorkerEvent = x.WorkerEvent

        member x.Run () = 
            try
                x.TriggerEvent TransferInitiated

                x.TransferProfile.Sources
                |> List.collect x.ScanSource
                |> (fun itemList -> x.TriggerEvent (ScanCompleted itemList.Length); itemList)
                |> List.iter x.TransferFile

                x.TriggerEvent TransferCompleted
            with
            | exn -> x.TriggerEvent (TransferError exn)

// ------------------------------------------------------------------------------------------------

and DriveTransferWorker (transferProfile, targetDir, slowDown) =
    inherit TransferWorker<string>(transferProfile, targetDir)

    member private x.TransferAction =
        match transferProfile.Action with
        | Copy -> fun sourcePath targetPath -> File.Copy(sourcePath, targetPath, x.OverrideOption)
        | Move -> fun sourcePath targetPath -> File.Move(sourcePath, targetPath, x.OverrideOption)

    override x.ScanSource sourceDef =
        let rootFolder = Path.GetPathRoot(sourceDef.Path)
        let pathParts = Path.GetRelativePath(rootFolder, sourceDef.Path).Split('/', '\\') |> Array.toList

        let getMatchingSubFolders pattern parent =
            Directory.GetDirectories(parent, pattern)
            |> Array.toList
            |> function | [] -> raise <| new Exception($"source path not found: {sourceDef.Path}") | folderList -> folderList

        let rec treeWalker (parent : string) (pathParts : string list) : string list =
            match pathParts with
            | [] -> [ parent ]
            | part :: tail ->
                parent
                |> getMatchingSubFolders part
                |> List.collect (fun subfolder -> treeWalker subfolder tail)

        treeWalker rootFolder pathParts
        |> List.collect (fun folder ->
            sourceDef.Selectors |> List.collect (fun selector -> Directory.GetFiles(folder, selector) |> Array.toList))

    override x.TransferFile file =
        let fileName = Path.GetFileName(file)
        let targetPath = Path.Combine(x.TargetDir, fileName) in

        match (not <| File.Exists(targetPath)) || x.OverrideOption with
        | true ->
            x.TriggerProgressUpdate <| Transferring file
            x.TransferAction file targetPath
            x.TriggerProgressUpdate <| Transferred file
        | _ -> x.TriggerProgressUpdate <| Skipped fileName

        if slowDown then System.Threading.Thread.Sleep(500) // for testing purposes only

and PortableTransferWorker (portableDevice, transferProfile, targetDir) =
    inherit TransferWorker<PortableDevice.File> (transferProfile, targetDir)

    member _.PortableDevice = portableDevice

    member private x.TransferAction =
        match transferProfile.Action with
        | Copy -> fun file -> x.PortableDevice.DownloadFile(file, x.TargetDir)
        | Move -> fun file -> x.PortableDevice.MoveFile(file, x.TargetDir)

    override x.ScanSource sourceDef =
        let rootFolder = x.PortableDevice.GetRootFolder()
        let pathParts = sourceDef.Path.Split('/', '\\') |> Array.toList
        
        let like pattern text =
            LikeOperator.LikeString(text, pattern, Microsoft.VisualBasic.CompareMethod.Text)

        let getMatchingSubFolders (pattern : string) (parent : PortableDevice.Folder) : PortableDevice.Folder list =
            parent.GetSubFolders()
            |> Seq.choose (fun folder -> match folder.Name |> like pattern with | true -> Some folder | _ -> None)
            |> Seq.toList
            |> function | [] -> raise <| new Exception($"source path not found: {sourceDef.Path}") | folderList -> folderList

        let rec treeWalker (parent : PortableDevice.Folder) (pathParts : string list) : PortableDevice.Folder list =
            match pathParts with
            | [] -> [ parent ]
            | part :: tail ->
                parent
                |> getMatchingSubFolders part
                |> List.collect (fun subfolder -> treeWalker subfolder tail)

        treeWalker rootFolder pathParts
        |> List.collect (fun (folder : PortableDevice.Folder) ->
                sourceDef.Selectors
                |> List.collect (fun (selector : string) ->
                    folder.GetFiles()
                    |> Seq.choose (fun file -> match file.Name |> like selector with | true -> Some file | _ -> None)
                    |> Seq.toList
                    )
                )

    override x.TransferFile file =
        let targetPath = Path.Combine(x.TargetDir, file.Name) in

        match (not <| File.Exists(targetPath)) || x.OverrideOption with
        | true ->
            x.TriggerProgressUpdate <| Transferring file.Name
            x.TransferAction file
            x.TriggerProgressUpdate <| Transferred file.Name
        | _ -> x.TriggerProgressUpdate <| Skipped file.Name

// ------------------------------------------------------------------------------------------------

module EventMonitor =
    let allEvents = function
        | TransferInitiated -> Console.WriteLine "Transfer initiated"
        | ScanCompleted numberOfFiles -> Console.WriteLine $"{numberOfFiles} files found"

        | ProgressUpdate details ->
            match details with
            | Transferring filename -> Console.Write $"Transferring {filename}... "
            | Transferred _         -> Console.WriteLine "OK"
            | Skipped filename     -> Console.WriteLine $"Skipped {filename}"
    
        | TransferCompleted -> Console.WriteLine "Transfer completed"
        | TransferError exn -> Console.WriteLine $"Transfer error: {exn.Message}"

    let errorsOnly = function
        | TransferError exn -> Console.WriteLine $"Transfer error: {exn.Message}"
        | _ -> ignore ()
        
    let subscribe processor (transferWorker : ITransferWorker) = transferWorker.WorkerEvent |> Event.add processor

// ------------------------------------------------------------------------------------------------

module Runtime =
    let runCheck () =
        try
            Console.WriteLine "Checking DAM configuration... "

            Console.WriteLine $". path = {Settings.ConfigFilename}"

            Console.Write ". load settings: "
            UserSettings.config () |> ignore
            Console.WriteLine "OK"

            Console.Write ". verify Location.Incoming: "
            UserSettings.config ()
            |> fun settings -> settings.Locations |> UserSettings.verifyLocation "Incoming"
            |> fun _ -> Console.WriteLine "OK"
            Console.WriteLine ()

            Console.WriteLine "Checking transfer profiles catalogue... "

            Console.WriteLine $". path = {Settings.ProfilesCatalogueFilename}"
        
            Console.Write ". load settings: "
            UserSettings.profilesCatalogue () |> ignore
            Console.WriteLine "OK"
        
            Ok ()
        with
        | :? IncompleteSetupException as exn ->
                Logging.emitError SetupPending $"missing user settings file: {exn.MissingFile}"
        | :? InvalidUserSettingsException as exn ->
                Logging.emitError (InvalidConfiguration exn.Message) $"invalid user settings: {exn.File}, {exn.ErrorMessage}"
        | exn ->
                Logging.emitError (InvalidConfiguration exn.Message) $"something is wrong with the setup: {exn.Message}"

    let runSetup () =
        let exampleConfig : DAMConfig = 
            {
                Locations = Map [
                    "Incoming", "Enter path here"
                    "LightTable", "Enter path here"
                    "Processed", "Enter path here"
                    "TrashBin", "Enter path here"
                ]
                Dependencies = Map [
                    "ContentStoreCmdlets", @"C:\Program Files\Common Files\ContentStoreFramework\OpilioCraft.ContentStore.Cmdlets.dll"
                ]
                ContentCategories = Map [
                    "Image", [ ".arw"; ".jpg"; ".jpeg" ]
                    "Movie", [ ".mov"; ".mp4"; ".mts" ]
                ]
            }

        let exampleSource =
            {
                Path = "Insert source path here"
                Selectors = [ "some file selector, e.g. *.jpg" ]
            }

        let exampleDriveProfile =
            {
                Device = DeviceType.Drive
                Sources = [ exampleSource ]
                Action = TransferAction.Copy
                Options = Set.ofList [ TransferOption.Silent ]
            }

        let examplePortableProfile =
            {
                Device = DeviceType.Portable "PhoneName"
                Sources = [ exampleSource ]
                Action = TransferAction.Copy
                Options = Set.ofList [ TransferOption.Silent ]
            }

        try
            if not <| Directory.Exists(OpilioCraft.Settings.AppDataLocation)
            then
                Console.Write "Directory for user settings missing"
                Directory.CreateDirectory(OpilioCraft.Settings.AppDataLocation) |> ignore
                Console.WriteLine " --> created"

            if not <| File.Exists(Settings.ConfigFilename)
            then
                Console.WriteLine "DAM configuration file missing"
                let json = JsonSerializer.Serialize<DAMConfig>(exampleConfig, options = UserSettings.configJsonOptions)
                File.WriteAllText(Settings.ConfigFilename, json)
                Console.WriteLine "Example of DAM configuration saved. Please make sure to customize settings before using this app."
                Console.WriteLine $"\nFile is located here: {Settings.ConfigFilename}"
            else
                Console.Write "Checking DAM configuration... "
                UserSettings.config ()
                |> fun settings -> settings.Locations |> UserSettings.verifyLocation "Incoming"
                |> fun _ -> Console.WriteLine "OK"

            let profilesCatalogue : ProfilesCatalogue =
                Map.empty
                |> Map.add "Enter a name for the profile / drive example" exampleDriveProfile
                |> Map.add "Enter a name for the profile / portable example" examplePortableProfile

            if not <| File.Exists(Settings.ProfilesCatalogueFilename)
            then
                let json = JsonSerializer.Serialize<ProfilesCatalogue>(profilesCatalogue, options = UserSettings.profilesCatalogueJsonOptions)
                File.WriteAllText(Settings.ProfilesCatalogueFilename, json)
                Console.WriteLine "Example of profiles catalogue saved. Please make sure to customize settings before using this app."
                Console.WriteLine $"\nFile is located here: {Settings.ProfilesCatalogueFilename}"
            else
                Console.Write "Checking transfer profiles catalogue... "
                UserSettings.profilesCatalogue () |> ignore
                Console.WriteLine "OK"

            Ok ()
        with
        | exn -> Logging.emitError RuntimeError $"setup error: {exn.Message}"

    let runTransfer transferProfile targetDir slowDown =
        try
            let transferWorker = TransferWorker<_>.CreateWorker transferProfile targetDir slowDown

            let eventCategory = if transferProfile.Options.Contains(TransferOption.Silent) then EventMonitor.errorsOnly else EventMonitor.allEvents
            transferWorker |> EventMonitor.subscribe eventCategory
        
            transferWorker.Run ()
            Ok ()
        with
        | exn -> Logging.emitError RuntimeError $"Error occurred: {exn.Message}"
