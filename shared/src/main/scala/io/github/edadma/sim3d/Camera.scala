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
    // Guard the degenerate case of looking straight up/down the world-up axis.
    val seedRight = fwd.cross(Vec3(0.0, 1.0, 0.0))
    val right     = (if seedRight.lengthSq < 1e-9 then Vec3(1.0, 0.0, 0.0) else seedRight).normalized
    val up        = right.cross(fwd)
    val focal     = (height / 2.0) / math.tan(fov / 2.0)
    new Projector(e, right, up, fwd, width, height, focal)

  // Clamp pitch just shy of the poles so the up-vector never collapses.
  def orbit(dYaw: Double, dPitch: Double): Camera =
    val lim = math.Pi / 2.0 - 1e-3
    copy(yaw = yaw + dYaw, pitch = math.max(-lim, math.min(lim, pitch + dPitch)))

  def zoom(factor: Double): Camera =
    copy(distance = math.max(0.05, distance * factor))

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
