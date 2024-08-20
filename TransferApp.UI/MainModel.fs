namespace OpilioCraft.DAM.TransferApp.UI

open Elmish
open Elmish.WPF

open OpilioCraft.DAM.TransferApp

// UI model
type WizardState =
    | Prepare
    | Run

type Model = {
    PrepareState : Prepare.State
    RunState : Run.State
    ActiveStep : WizardState
}

type Msg =
    | PrepareMsg of Prepare.Msg
    | RunMsg of Run.Msg
    | RunTransfer of string
    | CloseApp

// UI bindings
[<RequireQualifiedAccess>]
module MainModel =
    let init () =
        {
            PrepareState = Prepare.init()
            RunState = Run.init()
            ActiveStep = Prepare
        }, Cmd.none

    let update msg model =
        match msg with
        | PrepareMsg prepareMsg ->
            let updatedPrepareState, cmd = model.PrepareState |> Prepare.update prepareMsg
            { model with PrepareState = updatedPrepareState }, cmd |> Cmd.map PrepareMsg

        | RunMsg runMsg ->
            let updatedRunState, cmd = model.RunState |> Run.update runMsg
            { model with RunState = updatedRunState }, cmd |> Cmd.map RunMsg

        | RunTransfer profileName ->
            let transferProfile = UserSettings.profilesCatalogue().[profileName]
            let targetDir = UserSettings.config().Locations.["Incoming"]
            let transferWorker = Runtime.TransferWorker<_>.CreateWorker transferProfile targetDir false

            let runTransfer (dispatch: Msg -> unit) : unit =
                // register event listener
                transferWorker.WorkerEvent |> Event.add (fun evt -> evt |> Run.WorkerEvent |> RunMsg |> dispatch)
                
                // run worker
                async { transferWorker.Run () }
                |> Async.Start

            { model with ActiveStep = Run }, Cmd.ofSub runTransfer

        | CloseApp ->
            closeApp ()
            model, Cmd.none

    let bindings () : Binding<Model,Msg> list =
        [
            "WindowTitle"       |> Binding.oneWay (fun _ -> "Transfer App")

            "I18N_UsageHint"    |> Binding.oneWay (fun _ -> Resources.I18N["UsageHint"])
            "I18N_CancelBtn"    |> Binding.oneWay (fun _ -> Resources.I18N["CancelBtn"])
            "I18N_TransferBtn"  |> Binding.oneWay (fun _ -> Resources.I18N["TransferBtn"])
            "I18N_FinishBtn"    |> Binding.oneWay (fun _ -> Resources.I18N["FinishBtn"])

            "VisibleTab"        |> Binding.oneWay (fun model -> match model.ActiveStep with | Prepare -> 0 | Run -> 1)

            "ProfileList"       |> Binding.oneWay (fun m -> m.PrepareState.Profiles)
            "SelectedProfile"   |> Binding.twoWayOpt (
                                    (fun m -> m.PrepareState.SelectedProfile),
                                    (fun v _ -> v |> Prepare.Msg.SelectProfile |> PrepareMsg) )

            "Cancel"            |> Binding.cmd (fun _ -> CloseApp)

            "Run"               |> Binding.cmdIf (
                                    (fun m -> m.PrepareState.SelectedProfile.Value |> RunTransfer),
                                    (fun m -> m.PrepareState.SelectedProfile |> Option.isSome) )

            "Finish"            |> Binding.cmdIf (
                                    (fun _ -> CloseApp),
                                    (fun m -> m.RunState.TransferCompleted) )

            "CurrentActivity"   |> Binding.oneWay (fun model -> model.RunState.Activity )
            "IsIndeterminate"   |> Binding.oneWay (fun model -> model.RunState.NumberOfItems.IsNone)
            "ItemsCompleted"    |> Binding.oneWay (fun model -> float model.RunState.ItemsCompleted)
            "NumberOfItems"     |> Binding.oneWay (fun model -> float (model.RunState.NumberOfItems |> Option.defaultValue 1))
            "ActivityDetails"   |> Binding.oneWayOpt (fun model -> model.RunState.Details)
        ]
