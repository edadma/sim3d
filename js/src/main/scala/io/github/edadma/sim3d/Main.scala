package io.github.edadma.sim3d

import org.scalajs.dom
import org.scalajs.dom.{document, window}

/** The only browser-specific drawing code: adapt a 2D canvas context to the
  * shared [[Canvas]]. The [[Scene]] does the rest, identically to the Swing app.
  */
final class HtmlCanvas(ctx: dom.CanvasRenderingContext2D, val width: Double, val height: Double) extends Canvas:
  private def css(c: Int): String =
    val s = (c & 0xffffff).toHexString
    "#" + "0" * (6 - s.length) + s

  def clear(color: Int): Unit =
    ctx.fillStyle = css(color)
    ctx.fillRect(0, 0, width, height)

  def fillCircle(cx: Double, cy: Double, r: Double, color: Int): Unit =
    ctx.beginPath()
    ctx.arc(cx, cy, math.max(r, 0.0), 0.0, 2 * math.Pi)
    ctx.fillStyle = css(color)
    ctx.fill()

  def strokeLine(x1: Double, y1: Double, x2: Double, y2: Double, color: Int, width: Double): Unit =
    ctx.beginPath()
    ctx.moveTo(x1, y1)
    ctx.lineTo(x2, y2)
    ctx.strokeStyle = css(color)
    ctx.lineWidth = width
    ctx.stroke()

/** The browser front-end: mirrors the Swing app's behaviour over
  * `requestAnimationFrame`, sharing all physics and rendering code.
  */
@main def main(): Unit =
  val canvasEl = document.createElement("canvas").asInstanceOf[dom.html.Canvas]
  document.body.appendChild(canvasEl)
  val ctx = canvasEl.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  val scenarios     = Scenarios.all
  var scenarioIdx   = 0
  var integratorIdx = Integrator.all.indexOf(Leapfrog)

  var scenario: Scenario = null
  var sim: Simulation    = null
  var scene: Scene       = null
  var camera: Camera     = null
  var e0                 = 0.0
  var substeps           = 1
  var speedMul           = 1.0
  var focusIdx           = -1 // -1 = free (camera targets the origin)
  var paused             = false
  var showTrails         = true

  def updateSubsteps(): Unit =
    substeps = math.max(1, math.round(scenario.baseSubsteps * speedMul).toInt)

  def build(): Unit =
    val sc = scenarios(scenarioIdx)
    scenario = sc
    val st = sc.state()
    sim = new Simulation(st, sc.gravity(), Integrator.all(integratorIdx), sc.dt)
    scene = new Scene(sc.styles, new Trails(st.n, 240))
    camera = Camera(distance = sc.cameraDistance)
    focusIdx = -1
    speedMul = 1.0
    e0 = Energy.total(st, sc.gravity())
    updateSubsteps()

  def resize(): Unit =
    canvasEl.width = window.innerWidth.toInt
    canvasEl.height = window.innerHeight.toInt

  resize()
  build()
  window.addEventListener("resize", (_: dom.Event) => resize())

  // --- input ---------------------------------------------------------------

  var dragging = false
  var lastX    = 0.0
  var lastY    = 0.0

  canvasEl.addEventListener(
    "mousedown",
    (e: dom.MouseEvent) => { dragging = true; lastX = e.clientX; lastY = e.clientY },
  )
  window.addEventListener("mouseup", (_: dom.MouseEvent) => dragging = false)
  window.addEventListener(
    "mousemove",
    (e: dom.MouseEvent) =>
      if dragging then
        val dx = e.clientX - lastX
        val dy = e.clientY - lastY
        lastX = e.clientX; lastY = e.clientY
        camera = camera.orbit(-dx * 0.01, dy * 0.01),
  )
  canvasEl.addEventListener(
    "wheel",
    (e: dom.WheelEvent) => { e.preventDefault(); camera = camera.zoom(math.pow(1.1, math.signum(e.deltaY))) },
  )
  window.addEventListener(
    "keydown",
    (e: dom.KeyboardEvent) =>
      e.key match
        case " "             => paused = !paused
        case "r" | "R"       => build()
        case "t" | "T"       => showTrails = !showTrails
        case "n" | "N"       => scenarioIdx = (scenarioIdx + 1) % scenarios.length; build()
        case "f" | "F" =>
          focusIdx = if focusIdx + 1 >= sim.state.n then -1 else focusIdx + 1
          if focusIdx < 0 then camera = camera.copy(target = Vec3.zero, distance = scenario.cameraDistance)
          else camera = camera.copy(distance = scene.styles(focusIdx).radius * 8.0)
        case "[" => speedMul = math.max(speedMul * 0.5, 1.0 / 64); updateSubsteps()
        case "]" => speedMul = math.min(speedMul * 2.0, 64.0); updateSubsteps()
        case d if d.length == 1 && d(0) >= '1' && d(0) <= '5' =>
          val idx = d(0) - '1'
          if idx < Integrator.all.length then integratorIdx = idx; sim.integrator = Integrator.all(idx)
        case _ =>,
  )

  // --- loop ----------------------------------------------------------------

  def drawHud(): Unit =
    val drift = math.abs((Energy.total(sim.state, sim.field.asInstanceOf[GravityField]) - e0) / e0)
    val focusLabel = if focusIdx < 0 then "free" else s"body #$focusIdx"
    ctx.font = "13px monospace"
    ctx.fillStyle = "#ddddee"
    ctx.fillText(s"scenario : ${scenarios(scenarioIdx).name}", 14, 22)
    ctx.fillText(s"integrator: ${sim.integrator.name} — ${sim.integrator.blurb}", 14, 40)
    ctx.fillText(f"|ΔE/E₀| : $drift%.3e    t = ${sim.time}%.3f", 14, 58)
    ctx.fillText(f"focus: $focusLabel    speed = ${speedMul}%.3gx    ${if paused then "[PAUSED]" else ""}", 14, 76)
    ctx.fillStyle = "#888899"
    ctx.fillText(
      "drag: orbit   wheel: zoom   1-5: integrator   n: scenario   f: focus   [ ]: speed   t: trails   space: pause   r: reset",
      14,
      window.innerHeight - 14,
    )

  def frame(t: Double): Unit =
    if !paused then
      var i = 0
      while i < substeps do { sim.step(); i += 1 }
      scene.trails.record(sim.state.pos)
    if focusIdx >= 0 && focusIdx < sim.state.n then
      camera = camera.copy(target = sim.state.pos(focusIdx))
    val canvas = new HtmlCanvas(ctx, canvasEl.width.toDouble, canvasEl.height.toDouble)
    if showTrails then scene.render(canvas, camera, sim.state.pos)
    else
      val empty = new Scene(scene.styles, new Trails(sim.state.n, 0), scene.background)
      empty.render(canvas, camera, sim.state.pos)
    drawHud()
    window.requestAnimationFrame(t => frame(t))

  window.requestAnimationFrame(t => frame(t))
