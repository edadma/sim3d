package io.github.edadma.sim3d

/** A ready-to-run setup: the bodies, how to draw them, and sensible physics and
  * camera defaults. Front-ends pick a scenario, build a [[State]] and a
  * [[GravityField]] from it, and go.
  */
final case class Scenario(
    name: String,
    bodies: IndexedSeq[Body],
    styles: IndexedSeq[Style],
    g: Double,
    softening: Double,
    dt: Double,
    cameraDistance: Double,
    // How much simulated time to advance per displayed frame. Fast subsystems
    // (close-in moons) need a small value so they don't alias; the default suits
    // the toy scenarios. Front-ends derive their per-frame sub-step count from
    // this and `dt`, and let the user scale it live.
    simTimePerFrame: Double = 0.016,
):
  def state(): State          = State(bodies)
  def gravity(): GravityField = GravityField(g, softening)

  /** Sub-steps per displayed frame at unit speed: `simTimePerFrame / dt`. */
  def baseSubsteps: Int = math.max(1, math.round(simTimePerFrame / dt).toInt)

object Scenarios:

  /** The Chenciner–Montgomery figure-eight: three equal masses chasing each
    * other around a single figure-eight curve. A delicate periodic solution —
    * a poor integrator visibly wrecks it within a few periods while a symplectic
    * one traces it indefinitely. Planar (z = 0), G = 1, period ~ 6.33.
    */
  def figureEight: Scenario =
    val v = Vec3(0.466203685, 0.43236573, 0.0)
    val bodies = Vector(
      Body(1.0, Vec3(0.97000436, -0.24308753, 0.0), v),
      Body(1.0, Vec3(-0.97000436, 0.24308753, 0.0), v),
      Body(1.0, Vec3(0.0, 0.0, 0.0), Vec3(-0.93240737, -0.86473146, 0.0)),
    )
    val styles = Vector(
      Style(0.08, 0xff6b6b),
      Style(0.08, 0x4dabf7),
      Style(0.08, 0xffd43b),
    )
    Scenario("Figure-eight (3-body)", bodies, styles, g = 1.0, softening = 0.0, dt = 0.002, cameraDistance = 3.2)

  /** A star with several planets on inclined, near-circular orbits — a genuinely
    * three-dimensional system. Each planet is launched at the local circular
    * speed `sqrt(G M / r)`; the star is given the recoil velocity so the total
    * momentum is zero and the picture stays put.
    */
  def solarSystem: Scenario =
    val g    = 1.0
    val mSun = 1.0

    // (orbit radius, mass, inclination, starting phase, colour, display radius)
    val planets = Vector(
      (1.0, 1.0e-3, 0.10, 0.0, 0xffa94d, 0.06),
      (1.6, 2.0e-3, -0.15, 2.0, 0x4dabf7, 0.08),
      (2.4, 1.5e-3, 0.25, 4.0, 0x69db7c, 0.07),
      (3.3, 3.0e-3, 0.05, 5.5, 0xff8787, 0.09),
    )

    val planetBodies = planets.map { (r, m, inc, phase, _, _) =>
      val speed = math.sqrt(g * mSun / r)
      val ci    = math.cos(inc)
      val si    = math.sin(inc)
      // Circular orbit in the xy-plane, then tilted about the x-axis by `inc`.
      val pos = Vec3(r * math.cos(phase), r * math.sin(phase) * ci, r * math.sin(phase) * si)
      val vel = Vec3(-speed * math.sin(phase), speed * math.cos(phase) * ci, speed * math.cos(phase) * si)
      Body(m, pos, vel)
    }

    val netMomentum = planetBodies.foldLeft(Vec3.zero)((p, b) => p + b.vel * b.mass)
    val sun         = Body(mSun, Vec3.zero, -netMomentum / mSun)

    val bodies = sun +: planetBodies
    val styles = Style(0.30, 0xffe066) +: planets.map((_, _, _, _, c, dr) => Style(dr, c))

    Scenario("Star + planets (3D)", bodies, styles, g = g, softening = 1.0e-3, dt = 0.01, cameraDistance = 9.0)

  /** A self-gravitating cloud of `n` equal masses given a gentle overall spin —
    * it collapses, swings through, and settles into a churning bound cluster.
    * Softened gravity keeps close passes well-behaved. Deterministic in `seed`.
    */
  def cluster(n: Int = 40, seed: Long = 1L): Scenario =
    val rng    = new scala.util.Random(seed)
    val radius = 3.0
    val mass   = 1.0 / n
    val spin   = 0.25

    val bodies = Vector.tabulate(n) { _ =>
      // Rejection-sample a point inside the unit sphere, then scale.
      var p = Vec3(1, 1, 1)
      while p.lengthSq > 1.0 do
        p = Vec3(rng.between(-1.0, 1.0), rng.between(-1.0, 1.0), rng.between(-1.0, 1.0))
      val pos = p * radius
      // Tangential velocity about the y-axis gives the cloud net rotation.
      val vel = Vec3(-pos.z, 0.0, pos.x) * spin
      Body(mass, pos, vel)
    }

    val palette = Vector(0xff6b6b, 0xffd43b, 0x69db7c, 0x4dabf7, 0xda77f2, 0xff922b)
    val styles  = Vector.tabulate(n)(i => Style(0.06, palette(i % palette.length)))

    Scenario(s"Cluster ($n bodies)", bodies, styles, g = 1.0, softening = 0.05, dt = 0.005, cameraDistance = 11.0)

  // Realistic scenarios below use natural units: G = 1, distance in AU, mass in
  // solar masses. In these units a body at 1 AU around the Sun moves at speed 1,
  // so one year is 2π time units. Display radii are exaggerated for visibility —
  // the *orbits* (distances, periods, mass ratios) are to scale, the dots are
  // not. Position/velocity offset for a circular orbit of radius `a` at phase
  // `phase`, in a plane tilted `incl` about the x-axis:
  private def orbit(a: Double, phase: Double, incl: Double): (Vec3, Vec3) =
    val ci = math.cos(incl)
    val si = math.sin(incl)
    val pos = Vec3(a * math.cos(phase), a * math.sin(phase) * ci, a * math.sin(phase) * si)
    val dir = Vec3(-math.sin(phase), math.cos(phase) * ci, math.cos(phase) * si)
    (pos, dir)

  /** The real inner solar system to scale: Sun, Mercury, Venus, Earth with the
    * **Moon**, and Mars. Relative masses, orbital radii, and therefore periods
    * are accurate (Mercury's year really is ~88 days against Earth's 365). The
    * Moon orbits only 0.0026 AU from Earth, so focus the camera on Earth to see
    * it circle — at the full-system view it hides behind the planet.
    *
    * Mars's moons are too small and fast to resolve here; see [[marsSystem]].
    */
  def solarSystemRealistic: Scenario =
    import scala.collection.mutable.ArrayBuffer
    val g       = 1.0
    val sunMass = 1.0
    val bodies  = ArrayBuffer[Body]()
    val styles  = ArrayBuffer[Style]()

    def addPlanet(a: Double, m: Double, incl: Double, phase: Double, color: Int, dr: Double): (Vec3, Vec3) =
      val (p, d) = orbit(a, phase, incl)
      val v      = d * math.sqrt(g * sunMass / a)
      bodies += Body(m, p, v)
      styles += Style(dr, color)
      (p, v)

    def addMoon(pp: Vec3, pv: Vec3, pm: Double, a: Double, m: Double, incl: Double, phase: Double, color: Int, dr: Double): Unit =
      val (o, d) = orbit(a, phase, incl)
      bodies += Body(m, pp + o, pv + d * math.sqrt(g * pm / a))
      styles += Style(dr, color)

    bodies += Body(sunMass, Vec3.zero, Vec3.zero)
    styles += Style(0.10, 0xffd060)
    addPlanet(0.387, 1.66e-7, 0.122, 0.3, 0xb9a07f, 0.018) // Mercury
    addPlanet(0.723, 2.45e-6, 0.059, 2.4, 0xe6c77a, 0.028) // Venus
    val earthMass = 3.003e-6
    val (ep, ev)  = addPlanet(1.0, earthMass, 0.0, 4.1, 0x4d8fe0, 0.030)  // Earth
    addMoon(ep, ev, earthMass, 0.00257, 3.69e-8, 0.09, 1.0, 0xcfcfcf, 0.012) // Moon
    addPlanet(1.524, 3.23e-7, 0.0323, 5.6, 0xd9663f, 0.024) // Mars

    // Shift into the centre-of-mass frame so the whole system stays put.
    val totM    = bodies.map(_.mass).sum
    val comPos  = bodies.foldLeft(Vec3.zero)((s, b) => s + b.pos * b.mass) / totM
    val comVel  = bodies.foldLeft(Vec3.zero)((s, b) => s + b.vel * b.mass) / totM
    val recentered = bodies.map(b => Body(b.mass, b.pos - comPos, b.vel - comVel)).toVector

    Scenario(
      "Inner solar system + Moon (to scale)",
      recentered,
      styles.toVector,
      g = g,
      softening = 0.0,
      dt = 1.0e-4,
      cameraDistance = 3.6,
      simTimePerFrame = 0.02,
    )

  /** A close-up of Mars with **Phobos and Deimos**, at the system's own scale so
    * the two moons are actually visible. Phobos skims just 9,400 km out and laps
    * Mars roughly every 7.6 hours; Deimos, farther and slower, takes about 30.
    * That ~4:1 period ratio is on display when the camera sits close.
    */
  def marsSystem: Scenario =
    val g        = 1.0
    val marsMass = 3.23e-7

    def moon(a: Double, m: Double, incl: Double, phase: Double, color: Int, dr: Double): (Body, Style) =
      val (o, d) = orbit(a, phase, incl)
      (Body(m, o, d * math.sqrt(g * marsMass / a)), Style(dr, color))

    val (phobos, phobosStyle) = moon(6.27e-5, 5.4e-15, 0.019, 0.0, 0x9a8c7a, 2.5e-6)
    val (deimos, deimosStyle) = moon(1.57e-4, 7.5e-16, 0.031, 2.5, 0x8a8276, 2.5e-6)

    val bodies = Vector(Body(marsMass, Vec3.zero, Vec3.zero), phobos, deimos)
    val styles = Vector(Style(2.0e-5, 0xd9663f), phobosStyle, deimosStyle)

    Scenario(
      "Mars + Phobos & Deimos (close-up)",
      bodies,
      styles,
      g = g,
      softening = 0.0,
      dt = 2.0e-5,
      cameraDistance = 6.0e-4,
      simTimePerFrame = 2.2e-4,
    )

  /** All built-in scenarios, for UI cycling. */
  def all: Vector[Scenario] =
    Vector(figureEight, solarSystem, solarSystemRealistic, marsSystem, cluster())
