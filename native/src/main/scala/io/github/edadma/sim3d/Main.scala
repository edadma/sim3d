package io.github.edadma.sim3d

import io.github.edadma.sdl3.{Color as _, *}
import io.github.edadma.libcairo.{Context, Format, imageSurfaceCreate}

/** Cairo adapter for the shared [[Canvas]]. Cairo is a real 2D vector engine, so each
  * primitive is anti-aliased by its coverage rasteriser — the native target gets the same
  * smooth output as the Swing (Java2D) and browser (canvas) backends, with no supersampling.
  * As on every platform, only these three primitives are platform-specific; projection, depth
  * sorting, and trails come from [[Scene]]. Colours are packed `0xRRGGBB` integers.
  */
final class CairoCanvas(cr: Context, val width: Double, val height: Double) extends Canvas:
  private def source(color: Int): Unit =
    cr.setSourceRGBA(Color.r(color) / 255.0, Color.g(color) / 255.0, Color.b(color) / 255.0, 1.0)

  def clear(color: Int): Unit =
    source(color)
    cr.paint()

  def strokeLine(x1: Double, y1: Double, x2: Double, y2: Double, color: Int, w: Double): Unit =
    cr.moveTo(x1, y1)
    cr.lineTo(x2, y2)
    cr.setLineWidth(math.max(1.0, w))
    source(color)
    cr.stroke()

  def fillCircle(cx: Double, cy: Double, radius: Double, color: Int): Unit =
    cr.arc(cx, cy, math.max(1.0, radius), 0.0, 2 * math.Pi)
    source(color)
    cr.fill()

/** The native front-end: an SDL3 window driving the same simulation and renderer
  * as the Swing and browser apps, via the published `sdl3` binding. State changes
  * are echoed to stdout since this build has no text overlay.
  */
def runGui(): Unit =
  val Width  = 1000
  val Height = 720

  setMainReady()
  if !init(INIT_VIDEO) then
    System.err.println(s"SDL_Init failed: $error")
    return

  val window = createWindow("sim3d — native (SDL3 + Cairo)", Width, Height)
  if window.isNull then
    System.err.println(s"createWindow failed: $error")
    return
  val renderer = window.createRenderer()
  renderer.setVSync(true) // throttle the frame loop to the display refresh

  // Cairo draws the frame into an in-memory ARGB32 surface — every fill and line
  // anti-aliased by Cairo's coverage rasteriser, no supersampling. Each frame the surface is
  // uploaded to a streaming texture and blitted to the window. Cairo's ARGB32 byte layout
  // matches SDL's ARGB8888 on a little-endian host, so the upload is a straight copy.
  val surface = imageSurfaceCreate(Format.ARGB32, Width, Height)
  val cr      = surface.create
  val texture = renderer.createTexture(PIXELFORMAT_ARGB8888, TEXTUREACCESS_STREAMING, Width, Height)
  val canvas  = new CairoCanvas(cr, Width.toDouble, Height.toDouble)

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
  var prevMouseX = 0.0
  var prevMouseY = 0.0
  var prevDown   = false
  var running    = true

  while running do
    // Pump the event queue (also refreshes keyboard/mouse state); quit on close,
    // zoom on the mouse wheel (positive y = scroll away = zoom in).
    var e = pollEvent()
    while e.isDefined do
      val ev = e.get
      if ev.kind == QUIT then running = false
      else if ev.kind == MOUSE_WHEEL && ev.wheelY != 0.0 then camera = camera.zoom(math.pow(1.1, -ev.wheelY))
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

    // Held zoom keys, in parity with the other front-ends (the mouse wheel,
    // handled above, is the quicker way).
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
    drawScene.render(canvas, camera, sim.state.pos) // Cairo draws (and clears) the surface
    surface.flush()
    texture.update(surface.getData, surface.getStride)
    renderer.copy(texture)
    renderer.present()

  cr.destroy()
  surface.destroy()
  texture.destroy()
  renderer.destroy()
  window.destroy()
  quit()

@main def main(args: String*): Unit =
  if args.contains("headless") || args.contains("--headless") then Demo.run()
  else runGui()
