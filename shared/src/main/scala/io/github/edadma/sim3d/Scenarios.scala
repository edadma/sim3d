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
):
  def state(): State          = State(bodies)
  def gravity(): GravityField = GravityField(g, softening)

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

  /** All built-in scenarios, for UI cycling. */
  def all: Vector[Scenario] = Vector(figureEight, solarSystem, cluster())
