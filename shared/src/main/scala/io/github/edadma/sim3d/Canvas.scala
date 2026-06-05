package io.github.edadma.sim3d

/** The entire drawing surface the renderer needs — deliberately tiny.
  *
  * All projection, depth sorting, and sizing happen in shared code against this
  * interface, so a platform backend only has to fill a rectangle, a circle, and
  * a line. Swing implements it over `Graphics2D`; the browser over a 2D canvas
  * context. Colours are packed `0xRRGGBB` integers.
  *
  * The coordinate system is screen space: `(0,0)` top-left, `x` right, `y` down,
  * sizes in pixels.
  */
trait Canvas:
  def width: Double
  def height: Double
  def clear(color: Int): Unit
  def fillCircle(cx: Double, cy: Double, r: Double, color: Int): Unit
  def strokeLine(x1: Double, y1: Double, x2: Double, y2: Double, color: Int, width: Double): Unit

/** How a body is drawn: its display radius in world units (not its physical
  * size) and its `0xRRGGBB` colour.
  */
final case class Style(radius: Double, color: Int)

object Color:
  def r(c: Int): Int = (c >> 16) & 0xff
  def g(c: Int): Int = (c >> 8) & 0xff
  def b(c: Int): Int = c & 0xff

  def rgb(r: Int, g: Int, b: Int): Int = (r << 16) | (g << 8) | b

  /** Linear blend from `from` (t=0) to `to` (t=1), clamped. */
  def blend(from: Int, to: Int, t: Double): Int =
    val u  = if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t
    val rr = (r(from) + (r(to) - r(from)) * u).toInt
    val gg = (g(from) + (g(to) - g(from)) * u).toInt
    val bb = (b(from) + (b(to) - b(from)) * u).toInt
    rgb(rr, gg, bb)
