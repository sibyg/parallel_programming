def initialiseArray(inp: Array[Int])(v: Int) = {
  for (i <- inp.indices.par) {
    inp(i) = v
  }
}


private def computePixel(xc: Double, yc: Double, maxIterations: Int): Int = {
  var i = 0
  var x, y = 0.0
  while (x * x + y * y < 4 && i < maxIterations) {
    val xt = x * x - y * y + xc
    val yt = 2 * x * y + yc
    x = xt
    y = yt
    i = i + 1
  }
//  color(i)
}

def parRender(): Unit = {
  for(idx<-(0 until image.legth).par) {
    val (xc, yc) = coordinatesFor(idx)
    image(idx) = computePixel(xc, yc, maxIterations)
  }
}