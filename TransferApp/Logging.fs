module OpilioCraft.DAM.TransferApp.Logging

open Microsoft.Extensions.Logging

let logger : ILogger =
    use factory = LoggerFactory.Create(fun builder -> builder.AddConsole() |> ignore)
    factory.CreateLogger("OpilioCraft.DAM.Transfer");

let emitError (reason : ErrorReason) (errorMessage : string) =
    logger.LogError(errorMessage)
    Error reason
