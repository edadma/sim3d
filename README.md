# sim3d

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/sim3d_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/sim3d)](https://github.com/edadma/sim3d/commits)
![GitHub](https://img.shields.io/github/license/edadma/sim3d)
![Scala Version](https://img.shields.io/badge/Scala-3.8.4-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.21.0-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.12-blue.svg)

A pluggable, graphics-agnostic 3D physics simulator in cross-platform Scala
(JVM / Scala.js / Scala Native). It starts with gravitational N-body dynamics
and a family of interchangeable time integrators, built so the *quality* of a
good (symplectic) integrator is something you can see and measure. Rigid-body
dynamics are planned on top of the same machinery.

## Why "a good integrator" matters

Integrating `x'' = a(x)` naively (forward Euler) injects energy every step, so
orbits spiral apart. *Symplectic* integrators preserve the geometric structure
of Hamiltonian flow, so energy stays bounded for as long as you run them.
Running the figure-eight three-body choreography to `t = 60` shows the gap:

```
integrator                      |ΔE / E₀|   character
------------------------------------------------------------------------------
Explicit Euler                  2.903e-01   1st order, non-symplectic — energy grows
Symplectic Euler                1.745e-04   1st order, symplectic — bounded but noisy
Velocity Verlet (leapfrog)      1.152e-07   2nd order, symplectic — the workhorse
Runge–Kutta 4                   8.028e-12   4th order, non-symplectic — accurate, slow drift
Yoshida 4 (symplectic)          1.406e-12   4th order, symplectic — accurate AND bounded
```

Note that RK4 is *accurate* but not symplectic: over very long runs its energy
drifts monotonically, while leapfrog and Yoshida only oscillate. Accuracy and
conservation are different properties.

## Architecture

The physics and rendering are completely independent of any graphics toolkit.
A platform backend implements one tiny interface — `Canvas` (clear, fill a
circle, stroke a line) — and provides an animation loop. Everything else (3D
projection, depth sorting, perspective sizing, trails) lives in `shared`.

```
shared/   Vec3, State, ForceField/GravityField, Integrator (+ 5 methods),
          Energy, Simulation, Scenarios, Camera/Projector, Trails, Scene, Demo
jvm/      SwingCanvas + interactive Swing window
js/       HtmlCanvas + requestAnimationFrame app (+ index.html)
native/   headless energy demo (GUI via macOS graphics is future work)
```

### Pluggable integrators

All integrators implement `Integrator.step(state, dt, field)` and are
interchangeable at runtime (press `1`–`5` in the GUI):

- `ExplicitEuler` — 1st order, non-symplectic (the cautionary baseline)
- `SymplecticEuler` — 1st order, symplectic
- `Leapfrog` (velocity Verlet) — 2nd order, symplectic; the workhorse
- `Rk4` — classic 4th-order Runge–Kutta, non-symplectic
- `Yoshida4` — 4th-order symplectic via the triple-jump composition

### Scenarios

- **Figure-eight** — the Chenciner–Montgomery three-body choreography
- **Star + planets** — inclined near-circular orbits, genuinely 3D
- **Cluster** — a self-gravitating, gently spinning cloud of bodies

## Running it

### Desktop (Swing)

```sh
sbt sim3dJVM/run              # interactive 3D window
sbt "sim3dJVM/run --headless" # print the energy-comparison table
```

Controls: drag to orbit, wheel to zoom, `1`–`5` switch integrator, `n` next
scenario, `t` toggle trails, `space` pause, `r` reset.

### Browser (Scala.js)

```sh
sbt sim3dJS/fastLinkJS
cd js && python3 -m http.server 8080   # ES modules need http://, not file://
# open http://localhost:8080/
```

### Native (headless for now)

```sh
sbt sim3dNative/run
```

## Tests

The test suite runs on all three platforms — it covers vector math, gravity
symmetry, momentum conservation, the energy-error ordering of the integrators,
and camera projection.

```sh
sbt sim3dJVM/test
sbt sim3dJS/test
sbt sim3dNative/test
```
