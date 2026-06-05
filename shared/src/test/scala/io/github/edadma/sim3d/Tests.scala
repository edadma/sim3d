package io.github.edadma.sim3d

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers:

  "Vec3" - {
    "arithmetic and geometry" in {
      val a = Vec3(1, 2, 3)
      val b = Vec3(4, 5, 6)
      (a + b) shouldBe Vec3(5, 7, 9)
      (b - a) shouldBe Vec3(3, 3, 3)
      (a * 2.0) shouldBe Vec3(2, 4, 6)
      a.dot(b) shouldBe 32.0
      Vec3(1, 0, 0).cross(Vec3(0, 1, 0)) shouldBe Vec3(0, 0, 1)
      Vec3(3, 4, 0).length shouldBe 5.0
      Vec3.zero.normalized shouldBe Vec3.zero
    }
  }

  "Gravity" - {
    "two equal masses pull toward each other, equal and opposite" in {
      val pos  = Array(Vec3(-1, 0, 0), Vec3(1, 0, 0))
      val mass = Array(1.0, 1.0)
      val acc  = new Array[Vec3](2)
      new GravityField(g = 1.0).accelerations(pos, mass, acc)
      // Separation 2 → |a| = G m / r^2 = 1/4, each toward the other.
      acc(0).x shouldBe (0.25 +- 1e-12)
      acc(1).x shouldBe (-0.25 +- 1e-12)
      (acc(0) + acc(1)) shouldBe Vec3.zero
    }
  }

  /** Maximum relative energy drift sampled along a circular two-body orbit. This
    * captures the *amplitude* of an integrator's energy error, which orders the
    * methods cleanly: explicit Euler grows without bound, symplectic methods
    * oscillate within a small envelope, and higher order shrinks that envelope.
    */
  def orbitMaxDrift(integ: Integrator, dt: Double, steps: Int): Double =
    val m = 1.0
    val r = 1.0
    // Circular orbit of two equal masses about their midpoint:
    // centripetal m v^2 / r = G m^2 / (2r)^2  ⇒  v = sqrt(G m / (4 r)).
    val v = math.sqrt(1.0 / (4.0 * r))
    val st = State(
      Vector(
        Body(m, Vec3(-r, 0, 0), Vec3(0, -v, 0)),
        Body(m, Vec3(r, 0, 0), Vec3(0, v, 0)),
      ),
    )
    val field  = new GravityField(g = 1.0)
    val e0     = Energy.total(st, field)
    val sim    = new Simulation(st, field, integ, dt)
    var maxRel = 0.0
    var i      = 0
    while i < steps do
      sim.step()
      if i % 10 == 0 then
        val rel = math.abs((Energy.total(st, field) - e0) / e0)
        if rel > maxRel then maxRel = rel
      i += 1
    maxRel

  "Integrators" - {
    "momentum is conserved by leapfrog" in {
      val sc    = Scenarios.solarSystem
      val st    = sc.state()
      val field = sc.gravity()
      val p0    = Energy.momentum(st)
      val sim   = new Simulation(st, field, Leapfrog, 0.01)
      sim.steps(500)
      (Energy.momentum(st) - p0).length shouldBe (0.0 +- 1e-9)
    }

    "symplectic methods bound energy error; explicit Euler does not" in {
      val dt    = 0.005
      val steps = 12000
      val euler = orbitMaxDrift(ExplicitEuler, dt, steps)
      val leap  = orbitMaxDrift(Leapfrog, dt, steps)
      val yosh  = orbitMaxDrift(Yoshida4, dt, steps)

      leap should be < euler
      yosh should be < leap
      leap should be < 1.0e-2
      yosh should be < 1.0e-3
    }

    "higher-order symplectic shrinks the error envelope" in {
      orbitMaxDrift(Yoshida4, 0.01, 4000) should be < orbitMaxDrift(SymplecticEuler, 0.01, 4000)
    }
  }

  "Scenarios" - {
    "realistic inner system is centred (zero net momentum) and bound" in {
      val sc    = Scenarios.solarSystemRealistic
      val st    = sc.state()
      val field = sc.gravity()
      Energy.momentum(st).length shouldBe (0.0 +- 1e-10)
      Energy.total(st, field) should be < 0.0
    }

    "Phobos and Deimos stay bound to Mars over many orbits" in {
      val sc  = Scenarios.marsSystem
      val st  = sc.state()
      val sim = new Simulation(st, sc.gravity(), Leapfrog, sc.dt)
      sim.steps(20000) // ~32 Phobos orbits, ~8 Deimos orbits
      (st.pos(1) - st.pos(0)).length should be < 1.5 // Phobos near Mars (orbit radius 1.0)
      (st.pos(2) - st.pos(0)).length should be < 3.5 // Deimos near Mars (orbit radius 2.5)
    }
  }

  "Camera" - {
    "projects a point in front of the eye onto the screen centre" in {
      val proj = Camera(distance = 10.0, yaw = 0.0, pitch = 0.0).projector(800, 600)
      proj.project(Vec3.zero) match
        case Some(p) =>
          p.x shouldBe (400.0 +- 1e-6)
          p.y shouldBe (300.0 +- 1e-6)
          p.depth shouldBe (10.0 +- 1e-6)
        case None => fail("origin should be visible")
    }
  }
