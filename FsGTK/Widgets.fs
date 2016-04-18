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
    | Button    of string * Action<'M>
    | CheckBox  of string * bool * State<bool, 'M>
    | Slider    of string * Range * State<Range, 'M>
    | GLWidget  of int * int * (int * int -> 'M -> 'M)
    | HBox      of int * Widget<'M>[]
    | VBox      of int * Widget<'M>[]
    | Window    of string * int * int * MenuItem<'M>[] * Widget<'M>

type IModel<'M> =
    abstract Apply  : ('M -> 'M) -> unit
    abstract Eval   : ('M -> 'M) -> ('M -> unit) -> unit

type MenuItem
with
    static member toGtk (model: IModel<'M>) (m: MenuItem<'M>) : Gtk.Widget =
        match m with
        | Action    (s, action) ->
            let m = new Gtk.MenuItem(s)
            m.Activated.Add(fun _ ->
                model.Apply action.Command)
            m :> Gtk.Widget

        | Toggle    (s, state)  ->

            let m = new Gtk.CheckMenuItem(s)
            m.Activated.Add(fun _ ->
                model.Apply (state.Apply m.Active))
            m :> Gtk.Widget

        | Tree      (s, items)  ->
            let m = new Gtk.MenuItem(s)
            let children =
                items
                |> Array.map (MenuItem.toGtk model)
            let menu = new Gtk.Menu()
            children
            |> Array.iter (fun m -> menu.Append m)
            m.Submenu <- menu
            m :> Gtk.Widget

type Widget
with
    static member toGtk (model: IModel<'M>) (w: Widget<'M>) : Gtk.Widget =
        match w with
        | Button (s, fb) ->
            let but = new Gtk.Button(s)
            but.Clicked.Add (fun _ -> fb.Command |> model.Apply)
            but :> Gtk.Widget

        | CheckBox (s, b, state) ->
            let cb = new Gtk.CheckButton(s)
            cb.Active <- b
            cb.Toggled.Add(fun _ ->
                let cmd = state.Apply cb.Active
                
                model.Eval cmd (fun m ->
                    let b = cb.Active
                    let nB = state.Project m
                    if nB <> b
                    then cb.Active <- nB))

            cb :> Gtk.Widget

        | Slider (s, r, state) ->
            let slider = new Gtk.HScale(r.Min, r.Max, r.Step)
            slider.Value <- r.Value
            
            slider.ChangeValue.Add(fun _ ->
                let ov = { Range.Min = slider.Adjustment.Lower
                           Range.Max = slider.Adjustment.Upper
                           Range.Step = slider.Adjustment.StepIncrement
                           Range.Value = slider.Value }
                let cmd = state.Apply ov
                
                model.Eval cmd (fun m ->
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
                        slider.Value <- nV.Value))
            slider :> Gtk.Widget

        | GLWidget (width, height, render) ->
            let glW = new Gtk.GLWidget()
            glW.SetSizeRequest(width, height)
            glW.RenderFrame.Add(fun _ ->
                let width = glW.Allocation.Width
                let height = glW.Allocation.Height
                render (width, height) |> model.Apply)
            glW :> Gtk.Widget

        | HBox (padding, ws) ->
            let nWs = ws |> Array.map (Widget.toGtk model)
            let hbox = new Gtk.HBox()
            hbox.BorderWidth <- padding |> uint32
            nWs
            |> Array.iter(fun w -> hbox.PackStart(w, false, false, 0 |> uint32))
            hbox :> Gtk.Widget

        | VBox (padding, ws) ->
            let nWs = ws |> Array.map (Widget.toGtk model)
            let vbox = new Gtk.VBox()
            vbox.BorderWidth <- padding |> uint32
            nWs
            |> Array.iter(fun w -> vbox.PackStart(w, false, false, 0 |> uint32))
            vbox :> Gtk.Widget

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
            |> Array.iter menuBar.Append

            vbox.PackStart(menuBar, false, false, 0u)

            let gtkW = w |> Widget.toGtk model
            vbox.PackStart (gtkW, true, true, 0u)

            window.Add vbox
           
            window.ShowAll()
            window.Show()
            window :> Gtk.Widget
          
type Model<'M>(init: 'M) =
    let mutable m = init
    interface IModel<'M> with
        member x.Apply f = m <- f m
        member x.Eval  f p =
            m <- f m
            p m

let appRun (model: Model<'M>) (w: Widget<'M>) =
    use otk = OpenTK.Toolkit.Init()
    Gtk.Application.Init()
    Gtk.Application.Invoke(fun _ _ ->
        let gtkW = Widget.toGtk model w
        ())
    Gtk.Application.Run()    

        


