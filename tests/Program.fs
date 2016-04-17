open FSharp.GtkWidgets
open System

[<EntryPoint>]
let main argv = 
    let model = Model<int>(0)

    let widget =
        Widget.Window ("Hello"
                      , 500
                      , 500
                      , Widget.VBox
                        (2, [|
                            Widget.GLWidget (-1, 500, fun (width, height) m -> printfn "Render %dx%d" width height; m)
                            Widget.Button("Click 1", { Apply = fun m -> printfn "Click 1 %d" m; (m + 1) })
                            Widget.Button("Click 2", { Apply = fun m -> printfn "Click 2 %d" m; (m + 1) })
                            Widget.Button("Click 3", { Apply = fun m -> printfn "Click 3 %d" m; (m + 1) })
                            |]))
    appRun model widget
    0 // return an integer exit code
