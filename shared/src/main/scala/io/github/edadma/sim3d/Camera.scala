package io.github.edadma.sim3d

/** A point's screen position plus its camera-space depth (distance in front of
  * the eye along the view direction). Depth drives both far→near draw order and
  * perspective sizing.
  */
final case class Projected(x: Double, y: Double, depth: Double)

/** An orbiting perspective camera. It looks at `target` from `distance` away,
  * with `yaw` (around the vertical y-axis) and `pitch` (elevation) set by the
  * user dragging. `fov` is the vertical field of view in radians.
  */
final case class Camera(
    target: Vec3 = Vec3.zero,
    distance: Double = 10.0,
    yaw: Double = 0.6,
    pitch: Double = 0.4,
    fov: Double = math.Pi / 3.0,
):
  /** Eye position derived from the orbit angles. */
  def eye: Vec3 =
    val offset = Vec3(
      math.cos(pitch) * math.cos(yaw),
      math.sin(pitch),
      math.cos(pitch) * math.sin(yaw),
    ) * distance
    target + offset

  /** Snapshot the view transform for a given surface size. Build one per frame
    * and project many points through it.
    */
  def projector(width: Double, height: Double): Projector =
    val e   = eye
    val fwd = (target - e).normalized
    // The up-vector is the tangent of the orbit in the pitch direction. It is
    // always unit length and perpendicular to the view direction — even looking
    // straight up or down — so the basis never collapses and the camera can roll
    // continuously over the poles. (In the range a fixed world-up handles, this
    // is the identical basis.)
    val up    = Vec3(-math.sin(pitch) * math.cos(yaw), math.cos(pitch), -math.sin(pitch) * math.sin(yaw))
    val right = fwd.cross(up).normalized
    val focal = (height / 2.0) / math.tan(fov / 2.0)
    new Projector(e, right, up, fwd, width, height, focal)

  /** Orbit by the given yaw/pitch deltas. Both angles wrap, so the camera rolls
    * over the poles and turns all the way around rather than stopping. */
  def orbit(dYaw: Double, dPitch: Double): Camera =
    copy(yaw = Camera.wrapAngle(yaw + dYaw), pitch = Camera.wrapAngle(pitch + dPitch))

  def zoom(factor: Double): Camera =
    copy(distance = math.max(0.05, distance * factor))

object Camera:
  /** Normalise an angle into `(-pi, pi]` so orbit angles roll over instead of
    * growing without bound across a long drag. */
  def wrapAngle(a: Double): Double =
    val twoPi = 2.0 * math.Pi
    val m     = a % twoPi
    if m > math.Pi then m - twoPi
    else if m <= -math.Pi then m + twoPi
    else m

/** Precomputed view transform: eye, orthonormal basis, focal length. Projecting
  * is then a couple of dot products per point.
  */
final class Projector(
    val eye: Vec3,
    right: Vec3,
    up: Vec3,
    fwd: Vec3,
    val width: Double,
    val height: Double,
    val focal: Double,
    near: Double = 1.0e-3,
):
  /** Project a world point; `None` if it is at or behind the near plane. */
  def project(p: Vec3): Option[Projected] =
    val rel = p - eye
    val cz  = rel.dot(fwd)
    if cz <= near then None
    else
      val cx = rel.dot(right)
      val cy = rel.dot(up)
      Some(Projected(width / 2.0 + focal * cx / cz, height / 2.0 - focal * cy / cz, cz))

  /** Screen-space radius of a world-space length `r` seen at `depth`. */
  def radiusAt(r: Double, depth: Double): Double = focal * r / depth
