package io.github.edadma.sim3d

import io.github.edadma.sdl2.*
import io.github.edadma.sdl2.Color as SdlColor

/** SDL2 adapter for the shared [[Canvas]], built on the `io.github.edadma.sdl2`
  * binding's pure-Scala layer — no FFI here. As on every platform, only these
  * three primitives are platform-specific; projection, depth sorting, and trails
  * come from [[Scene]].
  */
final class SdlCanvas(r: Renderer, val width: Double, val height: Double) extends Canvas:
  // SDL2_gfx coordinates are 16-bit; clamp so far-off-screen points don't wrap.
  private def s(v: Double): Int = math.max(-16000.0, math.min(16000.0, v)).toInt

  def clear(color: Int): Unit = r.clear(SdlColor.fromRGB(color))

  def strokeLine(x1: Double, y1: Double, x2: Double, y2: Double, color: Int, w: Double): Unit =
    r.aaLine(s(x1), s(y1), s(x2), s(y2), SdlColor.fromRGB(color))

  def fillCircle(cx: Double, cy: Double, radius: Double, color: Int): Unit =
    val rad = math.max(1.0, math.min(8000.0, radius)).toInt
    r.fillCircle(s(cx), s(cy), rad, SdlColor.fromRGB(color))

/** The native front-end: an SDL2 window driving the same simulation and renderer
  * as the Swing and browser apps, via the published `sdl2` binding. State changes
  * are echoed to stdout since this build has no text overlay.
  */
def runGui(): Unit =
  val Width  = 1000
  val Height = 720

  setMainReady()
  if !init(INIT_VIDEO) then
    System.err.println(s"SDL_Init failed: $error")
    return

  val window = createWindow("sim3d — native (SDL2)", Width, Height)
  if window.isNull then
    System.err.println(s"createWindow failed: $error")
    return
  val renderer = window.createRenderer() // accelerated + vsync by default

  // Antialiasing by supersampling: draw into a 2x off-screen texture, then let
  // the GPU downscale it with linear filtering (a 2x2 box average per pixel).
  val ss   = 2
  val texW = Width * ss
  val texH = Height * ss
  setHint(HINT_RENDER_SCALE_QUALITY, "1")
  val target = renderer.createTexture(window.pixelFormat, TEXTUREACCESS_TARGET, texW, texH)
  target.setScaleMode(SCALEMODE_LINEAR)
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

  val prev = new Array[Boolean](512)
  var prevMouseX = 0
  var prevMouseY = 0
  var prevDown   = false
  var running    = true

  while running do
    // Pump the event queue (also refreshes keyboard/mouse state); quit on close.
    var e = pollEvent()
    while e.isDefined do
      if e.get.kind == QUIT then running = false
      e = pollEvent()

    val keys                   = Keyboard.state
    def down(sc: Int): Boolean = keys(sc)
    def edge(sc: Int): Boolean = down(sc) && !prev(sc)

    if edge(Scancode.Escape) then running = false
    if edge(Scancode.Space) then paused = !paused
    if edge(Scancode.R) then build()
    if edge(Scancode.T) then showTrails = !showTrails
    if edge(Scancode.N) then
      scenarioIdx = (scenarioIdx + 1) % scenarios.length
      build()
    if edge(Scancode.F) then
      focusIdx = if focusIdx + 1 >= sim.state.n then -1 else focusIdx + 1
      if focusIdx < 0 then camera = camera.copy(target = Vec3.zero, distance = scenario.cameraDistance)
      else camera = camera.copy(distance = scene.styles(focusIdx).radius * 8.0)
    if edge(Scancode.LeftBracket) then
      speedMul = math.max(speedMul * 0.5, 1.0 / 64); updateSubsteps()
    if edge(Scancode.RightBracket) then
      speedMul = math.min(speedMul * 2.0, 64.0); updateSubsteps()
    var k = Scancode.Num1
    while k <= Scancode.Num5 do
      if edge(k) then
        val idx = k - Scancode.Num1
        if idx < Integrator.all.length then
          integIdx = idx
          sim.integrator = Integrator.all(idx)
          println(s"integrator: ${sim.integrator.name} — ${sim.integrator.blurb}")
      k += 1

    // Held zoom keys (mouse-wheel parsing not needed with the binding's events,
    // but keys keep parity with the other front-ends).
    if down(Scancode.Minus) then camera = camera.zoom(1.04)
    if down(Scancode.Equals) then camera = camera.zoom(1.0 / 1.04)

    // Mouse drag orbits the camera.
    val mouse = Mouse.state
    if mouse.left && prevDown then
      camera = camera.orbit(-(mouse.x - prevMouseX) * 0.01, (mouse.y - prevMouseY) * 0.01)
    prevMouseX = mouse.x; prevMouseY = mouse.y; prevDown = mouse.left

    // Remember this frame's key states for next frame's edge detection.
    prev(Scancode.Escape) = down(Scancode.Escape); prev(Scancode.Space) = down(Scancode.Space)
    prev(Scancode.R) = down(Scancode.R); prev(Scancode.T) = down(Scancode.T); prev(Scancode.N) = down(Scancode.N)
    prev(Scancode.F) = down(Scancode.F); prev(Scancode.LeftBracket) = down(Scancode.LeftBracket)
    prev(Scancode.RightBracket) = down(Scancode.RightBracket)
    k = Scancode.Num1
    while k <= Scancode.Num5 do { prev(k) = down(k); k += 1 }

    if !paused then
      var i = 0
      while i < substeps do { sim.step(); i += 1 }
      scene.trails.record(sim.state.pos)
    if focusIdx >= 0 && focusIdx < sim.state.n then
      camera = camera.copy(target = sim.state.pos(focusIdx))

    val drawScene = if showTrails then scene else new Scene(scene.styles, new Trails(sim.state.n, 0), scene.background)
    renderer.setTarget(target)        // draw into the hi-res buffer
    drawScene.render(canvas, camera, sim.state.pos)
    renderer.resetTarget()            // back to the window
    renderer.copy(target)             // linear downscale = antialiasing
    renderer.present()

  target.destroy()
  renderer.destroy()
  window.destroy()
  quit()

@main def main(args: String*): Unit =
  if args.contains("headless") || args.contains("--headless") then Demo.run()
  else runGui()
