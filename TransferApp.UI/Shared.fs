namespace OpilioCraft.DAM.TransferApp.UI

open System.Windows

[<AutoOpen>]
module Shared =
    let showError errorMessage =
        MessageBox.Show(
            errorMessage,
            Resources.I18N["Error"],
            MessageBoxButton.OK,
            MessageBoxImage.Error
        ) |> ignore

    let closeApp () =
        Application.Current.MainWindow.Close()
