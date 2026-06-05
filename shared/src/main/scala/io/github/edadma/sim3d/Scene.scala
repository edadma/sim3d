package io.github.edadma.sim3d

/** Draws a set of bodies through a [[Camera]] onto any [[Canvas]]. This is the
  * whole renderer — fully platform-independent. It projects every body and trail
  * point, fades the trails from the background colour to each body's colour, and
  * paints the bodies back-to-front so nearer ones overlap farther ones.
  */
final class Scene(
    val styles: IndexedSeq[Style],
    val trails: Trails,
    val background: Int = 0x0a0a18,
    val minRadius: Double = 1.5,
):
  def render(c: Canvas, camera: Camera, pos: Array[Vec3]): Unit =
    val proj = camera.projector(c.width, c.height)
    c.clear(background)

    drawTrails(c, proj)
    drawBodies(c, proj, pos)

  private def drawTrails(c: Canvas, proj: Projector): Unit =
    var i = 0
    while i < styles.length do
      val color = styles(i).color
      val pts   = trails.points(i)
      val len   = pts.length
      if len >= 2 then
        var prev = proj.project(pts(0))
        var k    = 1
        while k < len do
          val cur = proj.project(pts(k))
          (prev, cur) match
            case (Some(a), Some(b)) =>
              // Fade older segments toward the background.
              val t = k.toDouble / (len - 1)
              c.strokeLine(a.x, a.y, b.x, b.y, Color.blend(background, color, t), 1.0)
            case _ =>
          prev = cur
          k += 1
      i += 1

  private def drawBodies(c: Canvas, proj: Projector, pos: Array[Vec3]): Unit =
    // Project all bodies, keep the visible ones, sort far → near.
    val visible = scala.collection.mutable.ArrayBuffer.empty[(Int, Projected)]
    var i       = 0
    while i < pos.length do
      proj.project(pos(i)) match
        case Some(p) => visible += ((i, p))
        case None    =>
      i += 1

    val ordered = visible.sortBy(-_._2.depth)
    var j       = 0
    while j < ordered.length do
      val (idx, p) = ordered(j)
      val style    = styles(idx)
      val r        = math.max(minRadius, proj.radiusAt(style.radius, p.depth))
      c.fillCircle(p.x, p.y, r, style.color)
      j += 1
