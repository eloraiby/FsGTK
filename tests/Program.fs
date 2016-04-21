open FSharp.GtkWidgets
open System

type TestModel = {
    CheckBox    : bool
    Saved0      : bool
    Saved1      : bool
    Slider      : Range
    Elements    : (Guid * string) []
} with
    static member create () = {
        CheckBox    = false
        Saved0      = false
        Saved1      = false
        Slider      = { Min = 0.0; Max = 20.0; Step = 0.5; Value = 10.0 }
        Elements    = [| Guid.NewGuid(), "element 1"
                         Guid.NewGuid(), "element 2"
                      |]
    }

[<EntryPoint>]
let main argv = 
    let model = Model<TestModel>(TestModel.create ())

    let widget =
        Widget.Window ("Hello"
                      , 500
                      , 500
                      , [|
                            MenuItem.Action ("Hello", { Command = fun m -> printfn "Hello"; m })
                            MenuItem.Tree ("File",  [|
                                                        MenuItem.Action ("New",   { Command = fun m -> printfn "New"; m })
                                                        MenuItem.Action ("Save",  { Command = fun m -> printfn "Save"; m })
                                                        MenuItem.Toggle ("Saved", { Apply = fun b m -> printfn "Saved %A" b; { m with Saved0 = b }
                                                                                    Project = fun m -> m.Saved0 })

                                                        MenuItem.Tree ("File2",
                                                            [|
                                                                MenuItem.Action ("New2", { Command = fun m -> printfn "New2"; m })
                                                                MenuItem.Action ("Save2", { Command = fun m -> printfn "Save2"; m })
                                                                MenuItem.Toggle ("Saved2", { Apply = fun b m -> printfn "Saved2 %A" b; { m with Saved1 = b }
                                                                                             Project = fun m -> m.Saved1 })
                                                            |])
                                                    |])
                        |]
                      , Widget.VBox
                        (2, [|
                            Widget.Label "starts here"
                            Widget.GLWidget (-1, 500, fun (width, height) m -> printfn "Render %dx%d" width height; m)
                            Widget.Button("Click 1", { Command = fun m -> printfn "Click 1 %A" m; m })
                            Widget.Button("Click 2", { Command = fun m -> printfn "Click 2 %A" m; m })
                            Widget.Button("Click 3", { Command = fun m -> printfn "Click 3 %A" m; m })
                            Widget.Slider
                                { State.Apply   = fun r m -> printfn "Slider Changed: %A" r; { m with Slider = r }
                                  Project       = fun m -> m.Slider
                                }
                            Widget.CheckBox ("CheckBox", { Apply = fun b m -> printfn "toggle: %b" b; { m with CheckBox = b }
                                                           Project = fun m -> m.CheckBox
                                                          })

                            Widget.ListWidget ({ State.Apply    = fun s m -> m
                                                 State.Project  = fun m -> { Elements.Selection = [||]; Elements.Elements = [||] }
                                               })
                            |]))
    appRun model widget
    0 // return an integer exit code
