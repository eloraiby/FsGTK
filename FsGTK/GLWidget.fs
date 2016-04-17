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
////////////////////////////////////////////////////////////////////////////////
//
// Originaly based on GLWidget done in C#
//
////////////////////////////////////////////////////////////////////////////////
// Gtk GLWidget Sharp - Gtk OpenGL Widget for CSharp using OpenTK
////////////////////////////////////////////////////////////////////////////////
(*
Usage:
	To render either override OnRenderFrame() or hook to the RenderFrame event.

	When GraphicsContext.ShareContexts == True (Default)
	To setup OpenGL state hook to the following events:
		GLWidget.GraphicsContextInitialized
		GLWidget.GraphicsContextShuttingDown

	When GraphicsContext.ShareContexts == False
	To setup OpenGL state hook to the following events:
		GLWidget.Initialized
		GLWidget.ShuttingDown 
*)
////////////////////////////////////////////////////////////////////////////////
#nowarn "9"
namespace Gtk

open OpenTK
open OpenTK.Graphics
open OpenTK.Platform
open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Runtime
open System.Security
open System.Threading
open System.ComponentModel

open Gtk
open Gdk

module private GLWidgetModule =
    let mutable graphicsContextCount = 0

    let mutable sharedContextInitialized = false

    type XVisualClass =
        | StaticGray    = 0
        | GrayScale     = 1
        | StaticColor   = 2
        | PseudoColor   = 3
        | TrueColor     = 4
        | DirectColor   = 5


    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type XVisualInfo =
        val mutable Visual       : IntPtr      
        val mutable VisualID     : IntPtr      
        val mutable Screen       : int         
        val mutable Depth        : int         
        val mutable Class        : XVisualClass
        val mutable RedMask      : uint32
        val mutable GreenMask    : uint32       
        val mutable blueMask     : uint32       
        val mutable ColormapSize : int         
        val mutable BitsPerRgb   : int         

        override x.ToString() =
            String.Format("id ({0}), screen ({1}), depth ({2}), class ({3})", x.VisualID, x.Screen, x.Depth, x.Class)

    [<Flags>]
    type internal XVisualInfoMask =
        | No        = 0x0
        | ID        = 0x1
        | Screen    = 0x2
        | Depth     = 0x4
        | Class     = 0x8
        | Red       = 0x10
        | Green     = 0x20
        | Blue      = 0x40
        | ColormapSize = 0x80
        | BitsPerRGB = 0x100
        | All       = 0x1FF


    [<SuppressUnmanagedCodeSecurity; DllImport("libgdk-win32-2.0-0.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr gdk_win32_drawable_get_handle(IntPtr d);


    [<DllImport("libX11", EntryPoint = "XGetVisualInfo")>]
    extern IntPtr XGetVisualInfoInternal(IntPtr display, IntPtr vinfo_mask, [<In>] XVisualInfo template, [<Out>] int& nitems)
    let XGetVisualInfo(display: IntPtr, vinfo_mask: XVisualInfoMask, template: byref<XVisualInfo>) : int * IntPtr =
        let mutable nItems = 0
        let gc = GCHandle.Alloc (vinfo_mask, GCHandleType.Pinned)
        let ptr = XGetVisualInfoInternal(display, gc.AddrOfPinnedObject(), template, &nItems)
        gc.Free()
        nItems, ptr

    let [<Literal>] linux_libx11_name = @"libX11.so.6"

    [<SuppressUnmanagedCodeSecurity; DllImport(linux_libx11_name)>]
    extern void XFree(IntPtr handle);

    let [<Literal>] linux_libgdk_x11_name = @"libgdk-x11-2.0.so.0";

    /// <summary> Returns the X resource (window or pixmap) belonging to a GdkDrawable. </summary>
    /// <remarks> XID gdk_x11_drawable_get_xid(GdkDrawable *drawable); </remarks>
    /// <param name="gdkDisplay"> The GdkDrawable. </param>
    /// <returns> The ID of drawable's X resource. </returns>
    [<SuppressUnmanagedCodeSecurity; DllImport(linux_libgdk_x11_name)>]
    extern IntPtr gdk_x11_drawable_get_xid(IntPtr gdkDisplay)

    /// <summary> Returns the X display of a GdkDisplay. </summary>
    /// <remarks> Display* gdk_x11_display_get_xdisplay(GdkDisplay *display); </remarks>
    /// <param name="gdkDisplay"> The GdkDrawable. </param>
    /// <returns> The X Display of the GdkDisplay. </returns>
    [<SuppressUnmanagedCodeSecurity; DllImport(linux_libgdk_x11_name)>]
    extern IntPtr gdk_x11_display_get_xdisplay(IntPtr gdkDisplay)

    type GLXAttrib =
        | GLX_NONE          = 0
        | GLX_USE_GL        = 1
        | GLX_BUFFER_SIZE   = 2
        | GLX_LEVEL         = 3
        | GLX_RGBA          = 4
        | GLX_DOUBLEBUFFER  = 5
        | GLX_STEREO        = 6
        | GLX_AUX_BUFFERS   = 7
        | GLX_RED_SIZE      = 8
        | GLX_GREEN_SIZE    = 9
        | GLX_BLUE_SIZE     = 10
        | GLX_ALPHA_SIZE    = 11
        | GLX_DEPTH_SIZE    = 12
        | GLX_STENCIL_SIZE  = 13
        | GLX_ACCUM_RED_SIZE    = 14
        | GLX_ACCUM_GREEN_SIZE  = 15
        | GLX_ACCUM_BLUE_SIZE   = 16
        | GLX_ACCUM_ALPHA_SIZE  = 17

    let buildAttribList(single: bool, stereo: bool, color: int, depth: int, stencil: int, accum: int) =
            [|
                yield GLXAttrib.GLX_RGBA |> int

                if (not single) then yield GLXAttrib.GLX_DOUBLEBUFFER |> int

                if stereo then yield GLXAttrib.GLX_STEREO |> int

                yield GLXAttrib.GLX_RED_SIZE |> int
                yield color / 4 // TODO support 16-bit

                yield GLXAttrib.GLX_GREEN_SIZE |> int
                yield color / 4 // TODO support 16-bit

                yield GLXAttrib.GLX_BLUE_SIZE |> int
                yield color / 4 // TODO support 16-bit

                yield GLXAttrib.GLX_ALPHA_SIZE |> int
                yield color / 4 // TODO support 16-bit

                yield GLXAttrib.GLX_DEPTH_SIZE |> int
                yield depth

                yield GLXAttrib.GLX_STENCIL_SIZE |> int
                yield stencil

                //attributeList.Add(GLX_AUX_BUFFERS);
                //attributeList.Add(Buffers);

                yield GLXAttrib.GLX_ACCUM_RED_SIZE |> int
                yield accum / 4// TODO support 16-bit

                yield GLXAttrib.GLX_ACCUM_GREEN_SIZE |> int
                yield accum / 4// TODO support 16-bit

                yield GLXAttrib.GLX_ACCUM_BLUE_SIZE |> int
                yield accum / 4// TODO support 16-bit

                yield GLXAttrib.GLX_ACCUM_ALPHA_SIZE |> int
                yield accum / 4// TODO support 16-bit

                yield GLXAttrib.GLX_NONE |> int
            |]

    let [<Literal>] linux_libgl_name = @"libGL.so.1"

    [<SuppressUnmanagedCodeSecurity; DllImport(linux_libgl_name)>]
    extern IntPtr glXChooseVisual(IntPtr display, int screen, int[] attr)

    let GetVisualInfo(display: IntPtr, screen: int, attribList: int[]) : IntPtr =
        try
            let attributes : int[]  = attribList |> Array.map (fun e -> int e)
            glXChooseVisual(display, screen, attributes)
        with
            | :? DllNotFoundException as e -> raise (DllNotFoundException("OpenGL dll not found!", e))
            | :? EntryPointNotFoundException as enf -> raise (EntryPointNotFoundException("Glx entry point not found!", enf))


    let graphicsContextInitialized = Event<EventArgs>()
    let graphicsContextShuttingDown = Event<EventArgs>()



[<ToolboxItem(true)>]
type GLWidget(graphicsMode_: GraphicsMode, glVersionMajor: int, glVersionMinor: int, graphicsContextFlags: GraphicsContextFlags) as this =
    inherit DrawingArea()

    do this.DoubleBuffered <- false
    let mutable graphicsMode = graphicsMode_


    let mutable initialized = false

    let mutable graphicsContext : IGraphicsContext = null
    let mutable windowInfo      : IWindowInfo   = null

    let renderFrameEvent    = Event<_>()
    let initializedEvent    = Event<_>()
    let shuttingDownEvent   = Event<_>()

    [<Browsable(true)>]
    member x.SingleBuffer    = (graphicsMode.Buffers = 1)
    member x.ColorBPP        = graphicsMode.ColorFormat.BitsPerPixel
    member x.AccumulatorBPP  = graphicsMode.AccumulatorFormat.BitsPerPixel
    member x.DepthBPP        = graphicsMode.Depth
    member x.StencilBPP      = graphicsMode.Stencil
    member x.Samples         = graphicsMode.Samples
    member x.Stero           = graphicsMode.Stereo

    /// <summary>The major version of OpenGL to use.</summary>
    member x.GlVersionMajor = glVersionMajor

    /// <summary>The minor version of OpenGL to use.</summary>
    member x.GlVersionMinor = glVersionMinor

    member x.GraphicsContextFlags   = graphicsContextFlags

    /// <summary>Constructs a new GLWidget using a given GraphicsMode</summary>
    new(graphicsMode)   = new GLWidget(graphicsMode, 1, 0, GraphicsContextFlags.Default)
    /// <summary>Constructs a new GLWidget.</summary>
    new()               = new GLWidget(GraphicsMode.Default)


    member x.Dispose (disposing: bool) =
        if disposing
        then
            graphicsContext.MakeCurrent windowInfo
            x.OnShuttingDown()
            if (GraphicsContext.ShareContexts && (Interlocked.Decrement(&GLWidgetModule.graphicsContextCount) = 0))
            then
                GLWidget.OnGraphicsContextShuttingDown()
                GLWidgetModule.sharedContextInitialized <- false
            graphicsContext.Dispose()

    override x.Finalize() =
        x.Dispose false
    
    // Called when the widget needs to be (fully or partially) redrawn.
    override x.OnExposeEvent(eventExpose: Gdk.EventExpose) =
        if not initialized
        then
            initialized <- true;

            // If this looks uninitialized...  initialize.
            graphicsMode <-
                GraphicsMode( if x.ColorBPP = 0 then ColorFormat(32) else ColorFormat(x.ColorBPP)
                            , if x.DepthBPP = 0 then 16 else x.DepthBPP
                            , graphicsMode.Stencil
                            , graphicsMode.Stencil
                            , graphicsMode.AccumulatorFormat
                            , graphicsMode.Buffers
                            , graphicsMode.Stereo)
            

            let colorBufferColorFormat = ColorFormat(graphicsMode.ColorFormat.BitsPerPixel)

            let accumulationColorFormat = ColorFormat(x.AccumulatorBPP)

            let buffers =
                if graphicsMode.Buffers = 1
                then 1
                else 2

            let graphicsMode = new GraphicsMode(colorBufferColorFormat, graphicsMode.Depth, graphicsMode.Stencil, graphicsMode.Samples, accumulationColorFormat, buffers, graphicsMode.Stereo)

            // IWindowInfo
            if Configuration.RunningOnWindows
            then
                let windowHandle = GLWidgetModule.gdk_win32_drawable_get_handle(this.GdkWindow.Handle)
                windowInfo <- OpenTK.Platform.Utilities.CreateWindowsWindowInfo(windowHandle)
            elif Configuration.RunningOnMacOS
            then
                let windowHandle = GLWidgetModule.gdk_x11_drawable_get_xid(this.GdkWindow.Handle)
                let ownHandle = true
                let isControl = true
                windowInfo <- OpenTK.Platform.Utilities.CreateMacOSCarbonWindowInfo(windowHandle, ownHandle, isControl)
            elif (Configuration.RunningOnX11)
            then
                let display = GLWidgetModule.gdk_x11_display_get_xdisplay(this.Display.Handle)
                let screen = this.Screen.Number
                let windowHandle = GLWidgetModule.gdk_x11_drawable_get_xid(this.GdkWindow.Handle)
                let rootWindow = GLWidgetModule.gdk_x11_drawable_get_xid(this.RootWindow.Handle)

                let visualInfo =
                    if graphicsMode.Index.HasValue
                    then
                        let mutable info = GLWidgetModule.XVisualInfo()
                        info.VisualID <- graphicsMode.Index.Value
                        let _, vi = GLWidgetModule.XGetVisualInfo(display, GLWidgetModule.XVisualInfoMask.ID, &info)
                        vi
                    else
                        let attribs =
                            GLWidgetModule.buildAttribList( x.SingleBuffer
                                                          , x.Stero
                                                          , x.ColorBPP
                                                          , x.DepthBPP
                                                          , x.StencilBPP
                                                          , x.AccumulatorBPP)
                        GLWidgetModule.GetVisualInfo(display, this.Screen.Number, attribs)

                windowInfo <- OpenTK.Platform.Utilities.CreateX11WindowInfo(display, screen, windowHandle, rootWindow, visualInfo)
                GLWidgetModule.XFree(visualInfo)
            else
                raise (PlatformNotSupportedException())

            // GraphicsContext
            graphicsContext <- new GraphicsContext(graphicsMode, windowInfo, glVersionMajor, glVersionMinor, graphicsContextFlags)
            graphicsContext.MakeCurrent(windowInfo)

            if GraphicsContext.ShareContexts
            then
                Interlocked.Increment(&GLWidgetModule.graphicsContextCount) |> ignore

                if (not GLWidgetModule.sharedContextInitialized)
                then
                    GLWidgetModule.sharedContextInitialized <- true
                    graphicsContext.LoadAll()
                    GLWidget.OnGraphicsContextInitialized()
            else
                graphicsContext.LoadAll()
                GLWidget.OnGraphicsContextInitialized();

            x.OnInitialized();
        else
            graphicsContext.MakeCurrent(windowInfo)

        let result = base.OnExposeEvent(eventExpose)
        x.OnRenderFrame()
        eventExpose.Window.Display.Sync(); // Add Sync call to fix resize rendering problem (Jay L. T. Cornwall) - How does this affect VSync?
        graphicsContext.SwapBuffers()
        result;

    // Called when this GLWidget needs to render a frame
    [<CLIEventAttribute>]
    member x.RenderFrame    = renderFrameEvent.Publish

    member private x.OnRenderFrame() =
        renderFrameEvent.Trigger(this, EventArgs.Empty)

    [<CLIEventAttribute>]
    member x.Initialized    = initializedEvent.Publish

    [<CLIEventAttribute>]
    member x.ShuttingDown   = shuttingDownEvent.Publish

    // Called on Resize
    override x.OnConfigureEvent (evnt: Gdk.EventConfigure) =
        let result = base.OnConfigureEvent(evnt)
        if graphicsContext <> null
        then graphicsContext.Update windowInfo
        result

    // Called when this GLWidget has a valid GraphicsContext
    member private x.OnInitialized() = initializedEvent.Trigger(this, EventArgs.Empty)

    // Called when this GLWidget is being Disposed
    member x.OnShuttingDown()   = shuttingDownEvent.Trigger (this, EventArgs.Empty)

    // Called when a widget is realized. (window handles and such are valid)
    override x.OnRealized() = base.OnRealized()


    interface IDisposable with
        member x.Dispose () =
            GC.SuppressFinalize(this)
            x.Dispose(true)
            base.Dispose()


    // Called when the first GraphicsContext is created in the case of GraphicsContext.ShareContexts == True;
    [<CLIEventAttribute>]
    static member GraphicsContextInitialized = GLWidgetModule.graphicsContextInitialized.Publish
    static member private OnGraphicsContextInitialized() = GLWidgetModule.graphicsContextInitialized.Trigger EventArgs.Empty

    // Called when the first GraphicsContext is being destroyed in the case of GraphicsContext.ShareContexts == True;
    [<CLIEventAttribute>]
    static member GraphicsContextShuttingDown = GLWidgetModule.graphicsContextShuttingDown.Publish
    static member private OnGraphicsContextShuttingDown() = GLWidgetModule.graphicsContextShuttingDown.Trigger EventArgs.Empty



