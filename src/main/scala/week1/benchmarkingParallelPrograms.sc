import org.scalameter
import org.scalameter._

val time =
  config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.verbose -> true
  )
withWarmer(new scalameter.Warmer.Default)
measure {
  (0 until 1000000).toArray
}

println(s"Array initialisation time: $time ms")

withMeasurer(new Measurer.MemoryFootprint) measure {
  (0 until 1000000).toArray
}