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
open Gtk

type State<'S, 'M> = {
    Apply   : 'S -> 'M -> 'M
    Project : 'M -> 'S
}

type Action<'M> = {
    Apply   : 'M -> 'M
}

type MenuItem<'M> =
    | Action    of string * Action<'M>
    | Toggle    of string * State<bool, 'M>
    | Tree      of string * MenuItem<'M>[]

type Widget<'M> =
    | Button    of string * Action<'M>
    | CheckBox  of string * State<bool, 'M>
    | Slider    of string * State<float * float * float, 'M>
    | GLWidget  of int * int * (int * int -> 'M -> 'M)
    | HBox      of int * Widget<'M>[]
    | VBox      of int * Widget<'M>[]
    | Menu      of string * MenuItem<'M> []
    | Window    of string * int * int * Widget<'M>

type IModel<'M> =
    abstract Apply  : ('M -> 'M) -> unit

type Widget
with
    static member toGtk (model: IModel<'M>) (w: Widget<'M>) : Gtk.Widget =
        match w with
        | Button (s, fb) ->
            let but = new Gtk.Button(s)
            but.Clicked.Add (fun _ -> fb.Apply |> model.Apply)
            but :> Gtk.Widget

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

        | Window (title, width, height, w) ->
            let window = new Window(title)
            window.SetDefaultSize(width, height)
            window.DeleteEvent.Add(fun e -> window.Hide(); Application.Quit(); e.RetVal <- true)
            let gtkW = w |> Widget.toGtk model
            window.Add gtkW
            window.ShowAll()
            window.Show()
            window :> Gtk.Widget
          
type Model<'M>(init: 'M) =
    let mutable m = init
    interface IModel<'M> with
        member x.Apply f = m <- f m

let appRun (model: Model<'M>) (w: Widget<'M>) =
    use otk = OpenTK.Toolkit.Init()
    Application.Init()
    Application.Invoke(fun _ _ ->
        let gtkW = Widget.toGtk model w
        ())
    Application.Run()    

        


