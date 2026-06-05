package io.github.edadma.sim3d

import java.awt.{BasicStroke, Color as AwtColor, Dimension, Font, Graphics, Graphics2D, RenderingHints}
import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent, MouseWheelEvent}
import java.awt.geom.{Ellipse2D, Line2D}
import javax.swing.{JFrame, JPanel, SwingUtilities, Timer, WindowConstants}

/** The only Swing-specific drawing code: adapt `Graphics2D` to the shared
  * [[Canvas]]. Everything visual beyond these three primitives is computed by
  * the platform-independent [[Scene]].
  */
final class SwingCanvas(g: Graphics2D, val width: Double, val height: Double) extends Canvas:
  private def awt(c: Int): AwtColor = new AwtColor((c >> 16) & 0xff, (c >> 8) & 0xff, c & 0xff)

  def clear(color: Int): Unit =
    g.setColor(awt(color))
    g.fillRect(0, 0, math.ceil(width).toInt, math.ceil(height).toInt)

  def fillCircle(cx: Double, cy: Double, r: Double, color: Int): Unit =
    g.setColor(awt(color))
    g.fill(new Ellipse2D.Double(cx - r, cy - r, 2 * r, 2 * r))

  def strokeLine(x1: Double, y1: Double, x2: Double, y2: Double, color: Int, width: Double): Unit =
    g.setColor(awt(color))
    g.setStroke(new BasicStroke(width.toFloat))
    g.draw(new Line2D.Double(x1, y1, x2, y2))

/** The interactive simulation panel: owns the running [[Simulation]], the
  * [[Scene]], and the [[Camera]], steps physics on a timer, and renders each
  * frame. Mouse orbits/zooms; keys switch integrator and scenario.
  */
final class SimPanel extends JPanel:
  private val scenarios     = Scenarios.all
  private var scenarioIdx   = 0
  private var integratorIdx = Integrator.all.indexOf(Leapfrog)

  private var sim: Simulation       = scala.compiletime.uninitialized
  private var scene: Scene          = scala.compiletime.uninitialized
  private var camera: Camera        = scala.compiletime.uninitialized
  private var e0: Double            = 0.0
  private var substeps: Int         = 1
  private var paused                = false
  private var showTrails            = true

  private var frames    = 0
  private var fps       = 0.0
  private var lastFpsAt = System.nanoTime()

  setPreferredSize(new Dimension(1000, 720))
  setBackground(AwtColor.BLACK)
  setFocusable(true)
  build()

  private def build(): Unit =
    val sc = scenarios(scenarioIdx)
    val st = sc.state()
    sim = new Simulation(st, sc.gravity(), Integrator.all(integratorIdx), sc.dt)
    scene = new Scene(sc.styles, new Trails(st.n, 240))
    camera = Camera(distance = sc.cameraDistance)
    e0 = sim.field match
      case gf: GravityField => Energy.total(st, gf)
      case _                => 0.0
    // Aim for a roughly constant amount of simulated time per displayed frame.
    substeps = math.max(1, math.round(0.016 / sc.dt).toInt)

  private def currentField: GravityField = sim.field.asInstanceOf[GravityField]

  def tick(): Unit =
    if !paused then
      var i = 0
      while i < substeps do
        sim.step()
        i += 1
      scene.trails.record(sim.state.pos)
    countFps()
    repaint()

  private def countFps(): Unit =
    frames += 1
    val now = System.nanoTime()
    val dt  = now - lastFpsAt
    if dt >= 500_000_000L then
      fps = frames * 1.0e9 / dt
      frames = 0
      lastFpsAt = now

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)

    val canvas = new SwingCanvas(g2, getWidth.toDouble, getHeight.toDouble)
    if showTrails then scene.render(canvas, camera, sim.state.pos)
    else
      val empty = new Scene(scene.styles, new Trails(sim.state.n, 0), scene.background)
      empty.render(canvas, camera, sim.state.pos)

    drawHud(g2)

  private def drawHud(g2: Graphics2D): Unit =
    val drift = math.abs((Energy.total(sim.state, currentField) - e0) / e0)
    g2.setColor(new AwtColor(0xdd, 0xdd, 0xee))
    g2.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 13))
    val lines = Seq(
      f"scenario : ${scenarios(scenarioIdx).name}",
      f"integrator: ${sim.integrator.name}  —  ${sim.integrator.blurb}",
      f"|ΔE/E₀|  : $drift%.3e        t = ${sim.time}%.1f        fps = $fps%.0f",
      if paused then "[PAUSED]" else "",
    )
    var y = 22
    for ln <- lines do
      if ln.nonEmpty then g2.drawString(ln, 14, y)
      y += 18

    g2.setColor(new AwtColor(0x88, 0x88, 0x99))
    val help = "drag: orbit   wheel: zoom   1-5: integrator   n: scenario   t: trails   space: pause   r: reset"
    g2.drawString(help, 14, getHeight - 14)

  // --- input ---------------------------------------------------------------

  private var lastX = 0
  private var lastY = 0

  addMouseListener(new MouseAdapter:
    override def mousePressed(e: MouseEvent): Unit =
      lastX = e.getX; lastY = e.getY; requestFocusInWindow())

  addMouseMotionListener(new MouseAdapter:
    override def mouseDragged(e: MouseEvent): Unit =
      val dx = e.getX - lastX
      val dy = e.getY - lastY
      lastX = e.getX; lastY = e.getY
      camera = camera.orbit(-dx * 0.01, dy * 0.01))

  addMouseWheelListener((e: MouseWheelEvent) => camera = camera.zoom(math.pow(1.1, e.getWheelRotation)))

  addKeyListener(new KeyAdapter:
    override def keyPressed(e: KeyEvent): Unit =
      e.getKeyCode match
        case KeyEvent.VK_SPACE => paused = !paused
        case KeyEvent.VK_R     => build()
        case KeyEvent.VK_T     => showTrails = !showTrails
        case KeyEvent.VK_N =>
          scenarioIdx = (scenarioIdx + 1) % scenarios.length
          build()
        case k if k >= KeyEvent.VK_1 && k <= KeyEvent.VK_5 =>
          val idx = k - KeyEvent.VK_1
          if idx < Integrator.all.length then
            integratorIdx = idx
            sim.integrator = Integrator.all(idx)
        case _ =>
      repaint())

object SwingApp:
  def launch(): Unit =
    SwingUtilities.invokeLater(() =>
      val frame = new JFrame("sim3d — pluggable integrators")
      val panel = new SimPanel
      frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      frame.setContentPane(panel)
      frame.pack()
      frame.setLocationRelativeTo(null)
      frame.setVisible(true)
      panel.requestFocusInWindow()
      val timer = new Timer(16, _ => panel.tick())
      timer.start())

@main def main(args: String*): Unit =
  if args.contains("headless") || args.contains("--headless") then Demo.run()
  else SwingApp.launch()
