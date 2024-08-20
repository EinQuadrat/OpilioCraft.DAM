namespace OpilioCraft.DAM.TransferApp.UI

open Elmish

open OpilioCraft.DAM.TransferApp.Runtime

[<RequireQualifiedAccess>]
module Run =
    type State = {
        Activity : string
        NumberOfItems : int option
        ItemsCompleted : int
        Details  : string option
        TransferCompleted : bool
    }

    type Msg =
    | WorkerEvent of EventType:TransferWorkerEvent

    let init () = {
        Activity = Resources.I18N["Initializing"]
        NumberOfItems = None
        ItemsCompleted = 0
        Details = None
        TransferCompleted = false
    }
    
    let update (runMsg : Msg) (runState : State) =
        match runMsg with
        | WorkerEvent eventType ->
            match eventType with
            | TransferInitiated ->
                { runState with
                    Activity = Resources.I18N["TransferInitiated"]
                    NumberOfItems = None
                    ItemsCompleted = 0
                    Details = None
                }, Cmd.none

            | ScanCompleted numberOfFiles ->
                { runState with
                    Activity = Resources.I18N["Transferring"]
                    NumberOfItems = Some (numberOfFiles + 1)
                    ItemsCompleted = 0
                    Details = None
                }, Cmd.none

            | ProgressUpdate details ->
                let updatedModel =
                    match details with
                    | Transferring file -> { runState with Details = Some file }
                    | Transferred file
                    | Skipped file -> { runState with Details = Some file; ItemsCompleted = runState.ItemsCompleted + 1 }

                updatedModel, Cmd.none

            | TransferCompleted ->
                { runState with
                    Activity = Resources.I18N["TransferCompleted"]
                    NumberOfItems = Some 1
                    ItemsCompleted = 1
                    Details = None
                    TransferCompleted = true
                }, Cmd.none

            | TransferError exn ->
                showError exn.Message
                closeApp ()
                runState, Cmd.none
