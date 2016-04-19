//
// F# GTK,
// Copyright(C) 2016 Wael El Oraiby
// 
// This program is free software : you can redistribute it and / or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Affero General Public License for more details.
// 
// You should have received a copy of the GNU Affero General Public License
// along with this program.If not, see <http://www.gnu.org/licenses/>.
//
module FSharp.GtkWidgets

open System

type State<'S, 'M> = {
    Apply   : 'S -> 'M -> 'M
    Project : 'M -> 'S
}

type Action<'M> = {
    Command : 'M -> 'M
}

type MenuItem<'M> =
    | Action    of string * Action<'M>
    | Toggle    of string * State<bool, 'M>
    | Tree      of string * MenuItem<'M>[]

type Range = {
    Min     : float
    Max     : float
    Step    : float
    Value   : float
}

type Widget<'M> =
    | Label     of string
    | Button    of string * Action<'M>
    | CheckBox  of string * State<bool, 'M>
    | Slider    of State<Range, 'M>
    | GLWidget  of int * int * (int * int -> 'M -> 'M)
    | HBox      of int * Widget<'M>[]
    | VBox      of int * Widget<'M>[]
    | Window    of string * int * int * MenuItem<'M>[] * Widget<'M>

type IModel<'M> =
    abstract Apply  : ('M -> 'M) -> unit

type MenuItem
with
    static member toGtk (model: IModel<'M>) (m: MenuItem<'M>) : Gtk.Widget * ('M -> unit) option =
        match m with
        | Action    (s, action) ->
            let m = new Gtk.MenuItem(s)
            m.Activated.Add(fun _ ->
                model.Apply action.Command)
            m :> Gtk.Widget, None

        | Toggle    (s, state)  ->

            let mi = new Gtk.CheckMenuItem(s)
            mi.Active <- false
            let update (m: 'M) =
                let v = state.Project m
                if v <> mi.Active
                then mi.Active <- v

            mi.Activated.Add(fun _ ->
                model.Apply (state.Apply mi.Active))
            mi :> Gtk.Widget, Some update

        | Tree      (s, items)  ->
            let m = new Gtk.MenuItem(s)
            let children =
                items
                |> Array.map (MenuItem.toGtk model)
            let menu = new Gtk.Menu()
            children
            |> Array.iter (fun (mi, _) -> menu.Append mi)
            m.Submenu <- menu

            let children =
                children
                |> Array.filter (fun (_, u) -> u |> Option.isSome)
                |> Array.map(fun (_, u) ->
                    match u with
                    | Some u -> u
                    | _ -> failwith "unreachable")

            let update m =
                children
                |> Array.iter(fun u -> u m)

            m :> Gtk.Widget, Some update

type Widget
with
    static member toGtk (model: IModel<'M>) (w: Widget<'M>) : Gtk.Widget * ('M -> unit) option =
        match w with
        | Label s ->
            let l = new Gtk.Label (s)
            l.Justify <- Gtk.Justification.Left
            l.SetAlignment(-1.0f, 0.5f)
            l :> Gtk.Widget, None

        | Button (s, fb) ->
            let but = new Gtk.Button(s)
            but.Clicked.Add (fun _ -> fb.Command |> model.Apply)
            but :> Gtk.Widget, None

        | CheckBox (s, state) ->
            let cb = new Gtk.CheckButton(s)
            cb.Toggled.Add(fun _ -> state.Apply cb.Active |> model.Apply)
            cb.Active <- false
            let update m =
                let b = cb.Active
                let nB = state.Project m
                if nB <> b
                then cb.Active <- nB

            cb :> Gtk.Widget, Some update

        | Slider state ->
            let slider = new Gtk.HScale(0.0, 10.0, 0.5)
            
            slider.Value <- 0.0
            slider.ChangeValue.Add(fun _ ->
                let ov = { Range.Min = slider.Adjustment.Lower
                           Range.Max = slider.Adjustment.Upper
                           Range.Step = slider.Adjustment.StepIncrement
                           Range.Value = slider.Value }
                state.Apply ov
                |> model.Apply)
                
            let update m =
                let ov = { Range.Min = slider.Adjustment.Lower
                           Range.Max = slider.Adjustment.Upper
                           Range.Step = slider.Adjustment.StepIncrement
                           Range.Value = slider.Value }
                let nV = state.Project m
                if nV <> ov
                then
                    printfn "ov: %f - nV: %f" ov.Value nV.Value
                    slider.SetRange(nV.Min, nV.Max)
                    slider.Adjustment.StepIncrement <- nV.Step
                    slider.Value <- nV.Value

            slider :> Gtk.Widget, Some update

        | GLWidget (width, height, render) ->
            let glW = new Gtk.GLWidget()
            glW.SetSizeRequest(width, height)
            glW.RenderFrame.Add(fun _ ->
                let width = glW.Allocation.Width
                let height = glW.Allocation.Height
                render (width, height) |> model.Apply)
            glW :> Gtk.Widget, None

        | HBox (padding, ws) ->
            let nWs = ws |> Array.map (Widget.toGtk model)
            let hbox = new Gtk.HBox()
            hbox.BorderWidth <- padding |> uint32
            nWs
            |> Array.iter(fun (w, _) -> hbox.PackStart(w, false, false, 0 |> uint32))
            
            let filtered =
                nWs
                |> Array.filter (snd >> Option.isSome)
                |> Array.map (fun (_, u) ->
                    match u with
                    | Some u -> u
                    | None   -> failwith "unreachable")

            let update m =
                filtered
                |> Array.iter(fun u -> u m)

            hbox :> Gtk.Widget, Some update

        | VBox (padding, ws) ->
            let nWs = ws |> Array.map (Widget.toGtk model)
            let vbox = new Gtk.VBox()
            vbox.BorderWidth <- padding |> uint32
            nWs
            |> Array.iter (fun (w, _) -> vbox.PackStart(w, false, false, 0 |> uint32))

            let filtered =
                nWs
                |> Array.filter (snd >> Option.isSome)
                |> Array.map (fun (_, u) ->
                    match u with
                    | Some u -> u
                    | None   -> failwith "unreachable")

            let update m =
                filtered
                |> Array.iter(fun u -> u m)

            vbox :> Gtk.Widget, Some update

        | Window (title, width, height, menus, w) ->
            let window = new Gtk.Window(title)
            window.SetDefaultSize(width, height)
            window.DeleteEvent.Add(fun e -> window.Hide(); Gtk.Application.Quit(); e.RetVal <- true)
            let vbox = new Gtk.VBox()

            let menuBar = new Gtk.MenuBar()
            let ms =
                menus
                |> Array.map (MenuItem.toGtk model)
            ms
            |> Array.iter (fst >> menuBar.Append)

            let filtered =
                ms
                |> Array.filter (snd >> Option.isSome)
                |> Array.map(fun (_, u) ->
                    match u with
                    | Some u -> u
                    | _ -> failwith "unreachable")

            vbox.PackStart(menuBar, false, false, 0u)

            let gtkW, u = w |> Widget.toGtk model
            vbox.PackStart (gtkW, true, true, 0u)

            let filtered =
                match u with
                | Some u ->
                    [| u |]
                    |> Array.append filtered
                | None -> filtered

            let update m =
                filtered
                |> Array.iter (fun u -> u m)
            window.Add vbox
           
            window.ShowAll()
            window.Show()
            window :> Gtk.Widget, Some update
          
type Model<'M>(init: 'M) =
    let mutable m = init
    let mutable inApply = false
    member x.Latest = m
    interface IModel<'M> with
        member x.Apply f =
            match inApply with
            | false ->
                inApply <- true
                m <- f m
                inApply <- false
            | _ -> ()

let appRun (model: Model<'M>) (w: Widget<'M>) =
    use otk = OpenTK.Toolkit.Init()
    Gtk.Application.Init()
    Gtk.Application.Invoke(fun _ _ ->
        let gtkW, update = Widget.toGtk model w
        match update with
        | Some u -> u model.Latest
        | None -> ()
        ())
    Gtk.Application.Run()    

        


