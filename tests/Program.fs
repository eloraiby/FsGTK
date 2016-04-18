open FSharp.GtkWidgets
open System

[<EntryPoint>]
let main argv = 
    let model = Model<int>(0)

    let mutable cbState = false
    let mutable slider = 0.0
    let widget =
        Widget.Window ("Hello"
                      , 500
                      , 500
                      , [|
                            MenuItem.Action ("Hello", { Command = fun m -> printfn "Hello"; m })
                            MenuItem.Tree ("File",  [|
                                                        MenuItem.Action ("New", { Command = fun m -> printfn "New"; m })
                                                        MenuItem.Action ("Save", { Command = fun m -> printfn "Save"; m })
                                                        MenuItem.Toggle ("Saved", { Apply = fun b m -> printfn "Saved %A" b; m
                                                                                    Project = fun m -> false })

                                                        MenuItem.Tree ("File2",
                                                            [|
                                                                MenuItem.Action ("New2", { Command = fun m -> printfn "New2"; m })
                                                                MenuItem.Action ("Save2", { Command = fun m -> printfn "Save2"; m })
                                                                MenuItem.Toggle ("Saved2", { Apply = fun b m -> printfn "Saved2 %A" b; m
                                                                                             Project = fun m -> false })
                                                            |])
                                                    |])
                        |]
                      , Widget.VBox
                        (2, [|
                            Widget.GLWidget (-1, 500, fun (width, height) m -> printfn "Render %dx%d" width height; m)
                            Widget.Button("Click 1", { Command = fun m -> printfn "Click 1 %d" m; (m + 1) })
                            Widget.Button("Click 2", { Command = fun m -> printfn "Click 2 %d" m; (m + 1) })
                            Widget.Button("Click 3", { Command = fun m -> printfn "Click 3 %d" m; (m + 1) })
                            Widget.Slider("ggg", { Range.Min = 0.0; Max = 10.0; Step = 0.5; Value = 5.0 },
                                { State.Apply   = fun r m -> printfn "Slider Changed: %A" r; slider <- r.Value; m
                                  Project       = fun m -> { Min = 0.0; Max = 15.0; Step = 0.5; Value = slider }
                                })
                            Widget.CheckBox ("CheckBox", false, { Apply = fun b m -> printfn "toggle: %b" b; cbState <- b; m
                                                                  Project = fun m -> cbState
                                                                })
                            |]))
    appRun model widget
    0 // return an integer exit code
