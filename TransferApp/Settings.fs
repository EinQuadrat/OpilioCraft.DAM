namespace OpilioCraft.DAM.TransferApp

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open OpilioCraft.FSharp.Prelude
open OpilioCraft.FSharp.Prelude.UserSettings

// ------------------------------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module Settings =
    // configuration file
    let ConfigFilename = Path.Combine(OpilioCraft.Settings.AppDataLocation, "DAM.json")
    let ProfilesCatalogueFilename = Path.Combine(OpilioCraft.Settings.AppDataLocation, "DAM.TransferApp.json")

// ------------------------------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module UserSettings =
    // possible errors
    type ConfigError =
        | Unknown
        | SyntaxError of File:string * ErrorMessage:string
        | MissingSection of File:string * Section:string
        | MissingItem of Item:string
        | InvalidDirectory of Item:string * Directory:string

    // json converters
    type DeviceTypeConverter () =
        inherit JsonConverter<DeviceType> ()

        let readStringProperty propName (reader : byref<Utf8JsonReader>) =
            reader.Read() |> ignore
            if (reader.TokenType <> JsonTokenType.PropertyName || reader.GetString() <> propName) then raise (new JsonException())
            reader.Read() |> ignore
            if (reader.TokenType <> JsonTokenType.String) then raise (new JsonException())
            reader.GetString()

        override _.Read (reader : byref<Utf8JsonReader>, _: Type, options: JsonSerializerOptions) =
            if (reader.TokenType <> JsonTokenType.StartObject) then raise (new JsonException())

            let typeName = readStringProperty "Type" &reader

            let deviceType =
                match typeName with
                | "Drive" -> Drive
                | "Portable" -> readStringProperty "DeviceName" &reader |> Portable
                | x -> raise (new JsonException())

            reader.Read() |> ignore
            if (reader.TokenType <> JsonTokenType.EndObject) then raise (new JsonException())

            deviceType

        override _.Write (writer: Utf8JsonWriter, value: DeviceType, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            match value with
            | Drive ->
                writer.WriteString("Type", "Drive")
            | Portable deviceName ->
                writer.WriteString("Type", "Portable")
                writer.WriteString("DeviceName", deviceName)

            writer.WriteEndObject()

    // JSON serializer configuration
    let configJsonOptions =
        JsonSerializerOptions()
        |> fun jsonOpts ->
            jsonOpts.WriteIndented <- true
            jsonOpts

    let profilesCatalogueJsonOptions =
        JsonSerializerOptions()
        |> fun jsonOpts ->
            jsonOpts.WriteIndented <- true
            jsonOpts.Converters.Add(EnumUnionConverter<TransferAction>())
            jsonOpts.Converters.Add(EnumUnionConverter<TransferOption>())
            jsonOpts.Converters.Add(DeviceTypeConverter())
            jsonOpts

    // load user settings on demand
    let private loadConfig = lazyLoad<DAMConfig> Settings.ConfigFilename
    let config () = loadConfig.Value

    let private loadProfilesCatalogue = lazyLoadWithOptions<ProfilesCatalogue> Settings.ProfilesCatalogueFilename profilesCatalogueJsonOptions
    let profilesCatalogue () = loadProfilesCatalogue.Value

    // config checker
    let verifyLocation name (locations : Map<string,string>) =
        locations.TryFind name
        |> Verify.raiseIfNone Settings.ConfigFilename $"missing slot {name}"
        |> Option.bind Verify.isValidDirectory
        |> Verify.raiseIfNone Settings.ConfigFilename $"value of slot {name} is not a valid directory"
        
    let verifySettings () =
        config()
        |> (fun settings -> settings.Locations |> verifyLocation "Incoming")
        |> ignore

        profilesCatalogue()
        |> ignore
