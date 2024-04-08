package util

class GridElement(val x: Int, val y: Int, val bomb: Boolean)

object GridElement:
  def apply(x: Int, y: Int, bomb: Boolean): GridElement = GridElement(x, y, bomb)