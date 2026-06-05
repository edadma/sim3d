package io.github.edadma.sim3d

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/** Minimal hand-written SDL2 bindings — just the 2D-renderer and input calls
  * this app needs. `@link("SDL2")` makes the native linker pull in libSDL2;
  * the build points it at Homebrew's copy. There is no maintained published
  * SDL2 binding for Scala Native 0.5, and this surface is small and stable, so
  * we declare it directly.
  */
@link("SDL2")
@extern
object SDL:
  def SDL_SetMainReady(): Unit                                                              = extern
  def SDL_Init(flags: UInt): CInt                                                           = extern
  def SDL_Quit(): Unit                                                                      = extern
  def SDL_CreateWindow(title: CString, x: CInt, y: CInt, w: CInt, h: CInt, flags: UInt): Ptr[Byte] = extern
  def SDL_CreateRenderer(window: Ptr[Byte], index: CInt, flags: UInt): Ptr[Byte]            = extern
  def SDL_DestroyRenderer(r: Ptr[Byte]): Unit                                               = extern
  def SDL_DestroyWindow(w: Ptr[Byte]): Unit                                                 = extern
  def SDL_SetRenderDrawColor(r: Ptr[Byte], red: UByte, g: UByte, b: UByte, a: UByte): CInt  = extern
  def SDL_RenderClear(r: Ptr[Byte]): CInt                                                   = extern
  def SDL_RenderDrawLine(r: Ptr[Byte], x1: CInt, y1: CInt, x2: CInt, y2: CInt): CInt        = extern
  def SDL_RenderPresent(r: Ptr[Byte]): Unit                                                 = extern
  def SDL_PollEvent(event: Ptr[Byte]): CInt                                                 = extern
  def SDL_GetKeyboardState(numkeys: Ptr[CInt]): Ptr[UByte]                                  = extern
  def SDL_GetMouseState(x: Ptr[CInt], y: Ptr[CInt]): UInt                                   = extern
  def SDL_SetHint(name: CString, value: CString): CInt                                      = extern
  def SDL_GetWindowPixelFormat(window: Ptr[Byte]): UInt                                     = extern
  def SDL_CreateTexture(renderer: Ptr[Byte], format: UInt, access: CInt, w: CInt, h: CInt): Ptr[Byte] = extern
  def SDL_DestroyTexture(texture: Ptr[Byte]): Unit                                          = extern
  def SDL_SetTextureScaleMode(texture: Ptr[Byte], scaleMode: CInt): CInt                    = extern
  def SDL_SetRenderTarget(renderer: Ptr[Byte], texture: Ptr[Byte]): CInt                    = extern
  def SDL_RenderCopy(renderer: Ptr[Byte], texture: Ptr[Byte], srcrect: Ptr[Byte], dstrect: Ptr[Byte]): CInt = extern

/** SDL2_gfx antialiased primitives. The base SDL renderer has no AA; SDL2_gfx's
  * `aaline`/`aacircle` give the smooth edges the Swing and browser backends get
  * for free. Coordinates are `Sint16` (CShort).
  */
@link("SDL2_gfx")
@extern
object Gfx:
  def aalineRGBA(r: Ptr[Byte], x1: CShort, y1: CShort, x2: CShort, y2: CShort, red: UByte, g: UByte, b: UByte, a: UByte): CInt = extern
  def filledCircleRGBA(r: Ptr[Byte], x: CShort, y: CShort, rad: CShort, red: UByte, g: UByte, b: UByte, a: UByte): CInt        = extern
  def aacircleRGBA(r: Ptr[Byte], x: CShort, y: CShort, rad: CShort, red: UByte, g: UByte, b: UByte, a: UByte): CInt            = extern

/** SDL2-renderer adapter for the shared [[Canvas]]. As on every platform, only
  * these three primitives are platform-specific; the projection, depth sorting,
  * and trails come from [[Scene]]. Filled circles use SDL2_gfx so they get an
  * antialiased outline, and trails use antialiased lines.
  */
final class SdlCanvas(renderer: Ptr[Byte], val width: Double, val height: Double) extends Canvas:
  // SDL2_gfx coordinates are 16-bit; clamp so far-off-screen points don't wrap.
  private def sh(v: Double): CShort = math.max(-16000.0, math.min(16000.0, v)).toShort
  private def rc(c: Int): UByte     = ((c >> 16) & 0xff).toUByte
  private def gc(c: Int): UByte     = ((c >> 8) & 0xff).toUByte
  private def bc(c: Int): UByte     = (c & 0xff).toUByte

  def clear(color: Int): Unit =
    SDL.SDL_SetRenderDrawColor(renderer, rc(color), gc(color), bc(color), 255.toUByte)
    SDL.SDL_RenderClear(renderer)

  def strokeLine(x1: Double, y1: Double, x2: Double, y2: Double, color: Int, w: Double): Unit =
    Gfx.aalineRGBA(renderer, sh(x1), sh(y1), sh(x2), sh(y2), rc(color), gc(color), bc(color), 255.toUByte)

  def fillCircle(cx: Double, cy: Double, r: Double, color: Int): Unit =
    val x   = sh(cx)
    val y   = sh(cy)
    val rad = math.max(1.0, math.min(8000.0, r)).toShort
    Gfx.filledCircleRGBA(renderer, x, y, rad, rc(color), gc(color), bc(color), 255.toUByte)
    Gfx.aacircleRGBA(renderer, x, y, rad, rc(color), gc(color), bc(color), 255.toUByte)

/** SDL scancodes for the keys we watch (USB HID usage IDs, which is what
  * `SDL_GetKeyboardState` is indexed by).
  */
private object Scan:
  val Key1 = 30; val Key5 = 34
  val N = 17; val F = 9; val T = 23; val R = 21
  val Space = 44; val LeftBracket = 47; val RightBracket = 48
  val Minus = 45; val Equals = 46; val Escape = 41

/** The native front-end: an SDL2 window driving the same simulation and renderer
  * as the Swing and browser apps. State changes are echoed to stdout since this
  * build has no text overlay.
  */
def runGui(): Unit =
  val Width  = 1000
  val Height = 720

  SDL.SDL_SetMainReady()
  if SDL.SDL_Init(0x20.toUInt) != 0 then // SDL_INIT_VIDEO
    System.err.println("SDL_Init failed")
    return

  val window =
    SDL.SDL_CreateWindow(c"sim3d - native (SDL2)", 0x2fff0000, 0x2fff0000, Width, Height, 0x4.toUInt) // CENTERED, SHOWN
  // SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC paces us to the display.
  val renderer = SDL.SDL_CreateRenderer(window, -1, (0x2 | 0x4).toUInt)

  // Antialiasing by supersampling: draw the scene into a 2x off-screen texture,
  // then let the GPU downscale it with linear filtering (a 2x2 box average per
  // pixel). SDL's base renderer has no AA, so this is what gives smooth edges.
  val ss     = 2
  val texW   = Width * ss
  val texH   = Height * ss
  SDL.SDL_SetHint(c"SDL_RENDER_SCALE_QUALITY", c"1") // linear (vs nearest)
  val target = SDL.SDL_CreateTexture(renderer, SDL.SDL_GetWindowPixelFormat(window), 2, texW, texH) // ACCESS_TARGET
  SDL.SDL_SetTextureScaleMode(target, 1)             // SDL_ScaleModeLinear
  val canvas = new SdlCanvas(renderer, texW.toDouble, texH.toDouble)

  val scenarios   = Scenarios.all
  var scenarioIdx = 0
  var integIdx    = Integrator.all.indexOf(Leapfrog)

  var scenario: Scenario = null
  var sim: Simulation    = null
  var scene: Scene       = null
  var camera: Camera     = null
  var substeps           = 1
  var speedMul           = 1.0
  var focusIdx           = -1
  var paused             = false
  var showTrails         = true

  def updateSubsteps(): Unit =
    substeps = math.max(1, math.round(scenario.baseSubsteps * speedMul).toInt)

  def build(): Unit =
    val sc = scenarios(scenarioIdx)
    scenario = sc
    val st = sc.state()
    sim = new Simulation(st, sc.gravity(), Integrator.all(integIdx), sc.dt)
    scene = new Scene(sc.styles, new Trails(st.n, 240))
    camera = Camera(distance = sc.cameraDistance)
    focusIdx = -1
    speedMul = 1.0
    updateSubsteps()
    println(s"scenario: ${sc.name}   integrator: ${sim.integrator.name}")

  build()

  val ev   = stackalloc[Byte](128)
  val evU  = ev.asInstanceOf[Ptr[UInt]]
  val mx   = stackalloc[CInt]()
  val my   = stackalloc[CInt]()
  val prev = new Array[Boolean](512)

  var prevMouseX = 0
  var prevMouseY = 0
  var prevDown   = false
  var running    = true

  while running do
    // Pump the event queue; we only care about the quit event here.
    while SDL.SDL_PollEvent(ev) != 0 do
      if (!evU).toInt == 0x100 then running = false // SDL_QUIT

    val keys = SDL.SDL_GetKeyboardState(null)
    def down(sc: Int): Boolean = keys(sc).toInt != 0
    def edge(sc: Int): Boolean = down(sc) && !prev(sc)

    if edge(Scan.Escape) then running = false
    if edge(Scan.Space) then paused = !paused
    if edge(Scan.R) then build()
    if edge(Scan.T) then showTrails = !showTrails
    if edge(Scan.N) then
      scenarioIdx = (scenarioIdx + 1) % scenarios.length
      build()
    if edge(Scan.F) then
      focusIdx = if focusIdx + 1 >= sim.state.n then -1 else focusIdx + 1
      if focusIdx < 0 then camera = camera.copy(target = Vec3.zero, distance = scenario.cameraDistance)
      else camera = camera.copy(distance = scene.styles(focusIdx).radius * 8.0)
    if edge(Scan.LeftBracket) then
      speedMul = math.max(speedMul * 0.5, 1.0 / 64); updateSubsteps()
    if edge(Scan.RightBracket) then
      speedMul = math.min(speedMul * 2.0, 64.0); updateSubsteps()
    var k = Scan.Key1
    while k <= Scan.Key5 do
      if edge(k) then
        val idx = k - Scan.Key1
        if idx < Integrator.all.length then
          integIdx = idx
          sim.integrator = Integrator.all(idx)
          println(s"integrator: ${sim.integrator.name} — ${sim.integrator.blurb}")
      k += 1

    // Held zoom keys (no mouse-wheel handling without parsing the event union).
    if down(Scan.Minus) then camera = camera.zoom(1.04)
    if down(Scan.Equals) then camera = camera.zoom(1.0 / 1.04)

    // Mouse drag orbits the camera.
    val mask = SDL.SDL_GetMouseState(mx, my)
    val down1 = (mask.toInt & 1) != 0
    if down1 && prevDown then
      camera = camera.orbit(-((!mx) - prevMouseX) * 0.01, ((!my) - prevMouseY) * 0.01)
    prevMouseX = !mx; prevMouseY = !my; prevDown = down1

    // Remember this frame's key states for next frame's edge detection.
    prev(Scan.Escape) = down(Scan.Escape); prev(Scan.Space) = down(Scan.Space)
    prev(Scan.R) = down(Scan.R); prev(Scan.T) = down(Scan.T); prev(Scan.N) = down(Scan.N)
    prev(Scan.F) = down(Scan.F); prev(Scan.LeftBracket) = down(Scan.LeftBracket)
    prev(Scan.RightBracket) = down(Scan.RightBracket)
    k = Scan.Key1
    while k <= Scan.Key5 do { prev(k) = down(k); k += 1 }

    if !paused then
      var i = 0
      while i < substeps do { sim.step(); i += 1 }
      scene.trails.record(sim.state.pos)
    if focusIdx >= 0 && focusIdx < sim.state.n then
      camera = camera.copy(target = sim.state.pos(focusIdx))

    val drawScene = if showTrails then scene else new Scene(scene.styles, new Trails(sim.state.n, 0), scene.background)
    SDL.SDL_SetRenderTarget(renderer, target)        // draw into the hi-res buffer
    drawScene.render(canvas, camera, sim.state.pos)
    SDL.SDL_SetRenderTarget(renderer, null)          // back to the window
    SDL.SDL_RenderCopy(renderer, target, null, null) // linear downscale = antialiasing
    SDL.SDL_RenderPresent(renderer)

  SDL.SDL_DestroyTexture(target)
  SDL.SDL_DestroyRenderer(renderer)
  SDL.SDL_DestroyWindow(window)
  SDL.SDL_Quit()

@main def main(args: String*): Unit =
  if args.contains("headless") || args.contains("--headless") then Demo.run()
  else runGui()
