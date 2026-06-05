package io.github.edadma.sim3d

/** A headless, platform-independent demonstration: run the figure-eight under
  * every integrator for a fixed span of simulated time and report how far each
  * one let the total energy drift. The symplectic methods stay near zero; plain
  * Euler and RK4 do not. This `run` is what the Native build and a Node run of
  * the JS build execute.
  */
object Demo:
  def run(): Unit =
    val sc       = Scenarios.figureEight
    val field    = sc.gravity()
    val duration = 60.0
    val steps    = (duration / sc.dt).toInt

    println(s"sim3d — energy conservation of ${sc.name}")
    println(f"integrating to t = $duration%.0f with dt = ${sc.dt} ($steps steps)")
    println()
    println(f"${"integrator"}%-28s ${"|ΔE / E₀|"}%12s   character")
    println("-" * 78)

    for integ <- Integrator.all do
      val s   = sc.state()
      val e0  = Energy.total(s, field)
      val sim = new Simulation(s, field, integ, sc.dt)
      sim.steps(steps)
      val rel = math.abs((Energy.total(s, field) - e0) / e0)
      println(f"${integ.name}%-28s ${rel}%12.3e   ${integ.blurb}")

    println()
    println(s"platform: $platform")
