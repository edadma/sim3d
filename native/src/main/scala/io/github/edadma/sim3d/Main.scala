package io.github.edadma.sim3d

import io.github.edadma.suit.{Canvas as SuitCanvas, Color as SuitColor, *}
import io.github.edadma.suit.dsl.*
import io.github.edadma.suit.widgets.*

/** Bridges the shared renderer's tiny [[Canvas]] onto suit's drawing surface. The
  * simulation and [[Scene]] never know which toolkit they draw into; this adapter maps the
  * three primitives the renderer needs — fill the background, a body disc, a trail segment —
  * onto the same `suit.Canvas` every suit widget paints through, so the sim draws *through*
  * the toolkit rather than talking to Cairo directly. Colours are packed `0xRRGGBB` ints on
  * the sim side and unpacked into suit's RGBA `Color` here.
  */
final class SuitCanvasAdapter(c: SuitCanvas, val width: Double, val height: Double) extends Canvas:
  private def conv(color: Int): SuitColor = SuitColor(Color.r(color), Color.g(color), Color.b(color))

  def clear(color: Int): Unit =
    c.fillRect(Rect(0, 0, width, height), conv(color))

  def fillCircle(cx: Double, cy: Double, r: Double, color: Int): Unit =
    c.fillCircle(Offset(cx, cy), math.max(1.0, r), conv(color))

  def strokeLine(x1: Double, y1: Double, x2: Double, y2: Double, color: Int, w: Double): Unit =
    c.line(Offset(x1, y1), Offset(x2, y2), math.max(1.0, w), conv(color))

/** The native front-end as a real suit application: the simulation animates inside a
  * [[dsl.canvas]] region while the rest of the window is an ordinary suit UI built from
  * `widgets.*`. The keyboard map of the old hand-rolled SDL loop becomes a control panel —
  * scenario and integrator pickers, speed and trails and pause controls — and the mouse
  * drives the camera over the canvas (drag to orbit, wheel to zoom).
  *
  * The animation is imperative: the mutable physics state (`sim`, `scene`, `camera`) lives in
  * refs that a [[useFrame]] callback steps each frame and the canvas painter reads, so a frame
  * advances and repaints without reconciling the tree. The control values are `useState`, so
  * the panel reconciles normally when one changes; each is mirrored into a ref the frame loop
  * reads. Because the canvas is its own repaint boundary, the sim re-rasterises at frame rate
  * while the cached control panel beside it is left untouched.
  */
val SimApp = view {
  val theme = useTheme()

  // Control state — drives the panel, owned here. Each is mirrored into a ref below so the
  // frame loop (a stable closure) can read the latest value.
  val (scenarioIdx, setScenarioIdx, _) = useState(0)
  val (integIdx, setIntegIdx, _)       = useState(Integrator.all.indexOf(Leapfrog))
  val (paused, setPaused, _)           = useState(false)
  val (showTrails, setShowTrails, _)   = useState(true)
  val (speedExp, setSpeedExp, _)       = useState(0)
  val (focusIdx, setFocusIdx, _)       = useState(-1)
  val (resetTick, setResetTick, _)     = useState(0)

  // The live simulation, mutated in place by the frame loop and rebuilt when the scenario
  // changes; the painter reads these refs each frame.
  val sim         = useRef[Simulation | Null](null)
  val scene       = useRef[Scene | Null](null)
  val scenarioObj = useRef[Scenario | Null](null)
  val camera      = useRef[Camera](Camera())
  val drag        = useRef[Offset](Offset.zero)

  // Latest-value mirrors of the control state for the frame loop's stable closure to read.
  val pausedR = useRef(false); pausedR.current   = paused
  val trailsR = useRef(true); trailsR.current    = showTrails
  val speedR  = useRef(0); speedR.current         = speedExp
  val focusR  = useRef(-1); focusR.current        = focusIdx

  // Build (or rebuild) the simulation for the current scenario. Reset advances `resetTick`,
  // which re-runs this; switching scenarios changes `scenarioIdx`. The chosen integrator is
  // applied here and kept in sync by the effect below when it alone changes.
  useEffect(
    () =>
      val sc = Scenarios.all(scenarioIdx)
      val st = sc.state()
      sim.current         = new Simulation(st, sc.gravity(), Integrator.all(integIdx), sc.dt)
      scene.current       = new Scene(sc.styles, new Trails(st.n, 240))
      camera.current      = Camera(distance = sc.cameraDistance)
      scenarioObj.current = sc
      noCleanup,
    Array(scenarioIdx, resetTick),
  )

  // Swap the integrator on the running system without rebuilding it, so a comparison happens
  // on the same trajectory.
  useEffect(
    () =>
      sim.current match
        case s: Simulation => s.integrator = Integrator.all(integIdx)
        case null          => ()
      noCleanup,
    Array(integIdx),
  )

  // Advance the simulation once per frame and request a repaint. Substeps scale the scenario's
  // base rate by the speed multiplier; trails are recorded continuously so toggling them on
  // shows the accumulated history. When a body is focused, the camera tracks it.
  useFrame { _ =>
    val s    = sim.current
    val sc   = scene.current
    val scen = scenarioObj.current
    if s != null && sc != null && scen != null then
      if !pausedR.current then
        val substeps = math.max(1, math.round(scen.baseSubsteps * math.pow(2.0, speedR.current)).toInt)
        var i        = 0
        while i < substeps do { s.step(); i += 1 }
        sc.trails.record(s.state.pos)
      val fi = focusR.current
      if fi >= 0 && fi < s.state.n then camera.current = camera.current.copy(target = s.state.pos(fi))
  }

  // --- handlers ---

  def slower(): Unit = setSpeedExp(math.max(speedExp - 1, -6))
  def faster(): Unit = setSpeedExp(math.min(speedExp + 1, 6))
  def speedLabel: String = if speedExp >= 0 then s"${1 << speedExp}×" else s"1/${1 << -speedExp}×"

  def onScenario(value: String): Unit =
    setScenarioIdx(value.toInt)
    setFocusIdx(-1)
    setSpeedExp(0)

  // Cycle the focused body (free → 0 → 1 → … → free). Free returns to the scenario's framing;
  // focusing a body pulls the camera in to a few of its display radii so a moon is visible.
  def cycleFocus(): Unit =
    val scen = Scenarios.all(scenarioIdx)
    val n    = scen.bodies.length
    val next = if focusIdx + 1 >= n then -1 else focusIdx + 1
    setFocusIdx(next)
    scene.current match
      case sc: Scene =>
        if next < 0 then camera.current = camera.current.copy(target = Vec3.zero, distance = scen.cameraDistance)
        else camera.current = camera.current.copy(distance = sc.styles(next).radius * 8.0)
      case null => ()

  // Mouse drag orbits the camera; the wheel zooms (away = in), matching the other front-ends.
  def onDown(e: PointerEvent): Unit = drag.current = e.position

  def onMove(e: PointerEvent): Unit =
    if e.button != 0 then
      val dx = e.position.x - drag.current.x
      val dy = e.position.y - drag.current.y
      drag.current   = e.position
      camera.current = camera.current.orbit(-dx * 0.01, dy * 0.01)

  def onWheel(e: ScrollEvent): Unit =
    camera.current = camera.current.zoom(math.pow(1.1, -e.deltaY))

  // Keys on the focused canvas mirror a couple of the old shortcuts for convenience.
  def onKey(e: KeyEvent): Unit =
    e.scancode match
      case Key.Space => setPaused(!paused)
      case _         => ()

  // --- panel pieces ---

  val scenarioOptions  = Scenarios.all.zipWithIndex.map((s, i) => (i.toString, s.name))
  val integratorOptions = Integrator.all.zipWithIndex.map((g, i) => (i.toString, g.name))

  def header(title: String): VNode = text(title, weight = FontWeight.SemiBold)

  val sidebar =
    scrollView(Axis.Vertical)(
      col(spacing = theme.spacing * 1.5, crossAxisAlignment = CrossAxisAlignment.Stretch)(
        text("sim3d", size = theme.textSize * 1.4, weight = FontWeight.Bold),
        Card(
          col(spacing = theme.spacing, crossAxisAlignment = CrossAxisAlignment.Stretch)(
            header("Scenario"),
            RadioGroup(scenarioOptions, scenarioIdx.toString, onScenario),
          ),
        ),
        Card(
          col(spacing = theme.spacing, crossAxisAlignment = CrossAxisAlignment.Stretch)(
            header("Integrator"),
            RadioGroup(integratorOptions, integIdx.toString, i => setIntegIdx(i.toInt)),
            text(Integrator.all(integIdx).blurb, size = theme.textSize * 0.85, color = theme.surfaceText.withAlpha(170), maxLines = 2),
          ),
        ),
        Card(
          col(spacing = theme.spacing, crossAxisAlignment = CrossAxisAlignment.Stretch)(
            header("Playback"),
            row(spacing = 8, crossAxisAlignment = CrossAxisAlignment.Center)(
              Button("–", slower),
              box(flex = 1)(center(text(s"Speed $speedLabel"))),
              Button("+", faster),
            ),
            row(spacing = 8, crossAxisAlignment = CrossAxisAlignment.Center)(
              text("Trails"),
              spacer(),
              Switch(showTrails, setShowTrails),
            ),
            Button(if paused then "Resume" else "Pause", () => setPaused(!paused)),
            Button("Reset", () => setResetTick(resetTick + 1)),
            Button("Cycle focus", cycleFocus),
            text(if focusIdx < 0 then "Focus: free" else s"Focus: body $focusIdx", size = theme.textSize * 0.85, color = theme.surfaceText.withAlpha(170)),
          ),
        ),
        text("Drag the view to orbit · wheel to zoom", size = theme.textSize * 0.8, color = theme.surfaceText.withAlpha(150), maxLines = 2),
      ),
    )

  box(bg = theme.background)(
    row(crossAxisAlignment = CrossAxisAlignment.Stretch)(
      box(flex = 1)(
        canvas(
          focusable   = true,
          onMouseDown = onDown,
          onMouseMove = onMove,
          onWheel     = onWheel,
          onKeyDown   = onKey,
        ) { (c, size) =>
          val s  = sim.current
          val sc = scene.current
          if s != null && sc != null then
            val adapter   = new SuitCanvasAdapter(c, size.width, size.height)
            val drawScene = if trailsR.current then sc else new Scene(sc.styles, new Trails(s.state.n, 0), sc.background)
            drawScene.render(adapter, camera.current, s.state.pos)
        },
      ),
      box(width = 300, bg = theme.surface, padding = EdgeInsets.all(theme.spacing * 2))(sidebar),
    ),
  )
}

@main def main(args: String*): Unit =
  if args.contains("headless") || args.contains("--headless") then Demo.run()
  else Suit.run("sim3d — native (suit)", 1100, 720) { SimApp() }
