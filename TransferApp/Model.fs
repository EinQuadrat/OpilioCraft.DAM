namespace OpilioCraft.DAM.TransferApp

open System.Text.Json.Serialization

// error return codes
type ErrorReason =
    | UsageError
    | SetupPending
    | InvalidConfiguration of Message:string
    | UnknownProfile
    | InvalidTargetPath of Path:string
    | RuntimeError

    with
        member x.ReturnCode =
            match x with
            | UsageError -> 1
            | SetupPending -> 2
            | InvalidConfiguration _ -> 3
            | UnknownProfile -> 4
            | InvalidTargetPath _ -> 5
            | RuntimeError -> 99

// app config
type DAMConfig =
    {
        Locations : Map<string, string>
        Dependencies : Map<string, string>
        ContentCategories : Map<string, string list>
    }

    // workaround to ensure required properties
    member private x.AssertLabelsNotNull() =
        if box x.Locations |> isNull then invalidArg "Locations" "missing in corresponding JSON file"
        if box x.Dependencies |> isNull then invalidArg "Dependencies" "missing in corresponding JSON file"
        if box x.ContentCategories |> isNull then invalidArg "ContentCategories" "missing in corresponding JSON file"

    interface System.Text.Json.Serialization.IJsonOnDeserialized with
        member x.OnDeserialized() = x.AssertLabelsNotNull()

// transfer profiles
type TransferProfile = {
    Device : DeviceType
    Sources : SourceDefinition list
    Action : TransferAction
    Options : Set<TransferOption>
}

and DeviceType =
    | Drive
    | Portable of DeviceName:string

and SourceDefinition = {
    Path : string
    Selectors : string list
}

and TransferAction =
    | Copy
    | Move

and TransferOption =
    | Silent
    | Override

type ProfilesCatalogue =
    Map<string, TransferProfile>
