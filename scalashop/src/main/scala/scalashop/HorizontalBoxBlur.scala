package scalashop

import org.scalameter._
import common._
import java.util.concurrent._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method

    for(i <- from to end-1)
    {
        for(j <- 0 to src.width-1)
        {
           dst(j, i) = boxBlurKernel(src, j, i, radius) 
        }
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    import scala.collection.mutable.ListBuffer
    val tasks = new ListBuffer[ForkJoinTask[Unit]]()
    val startPos = new Array[Int](numTasks)
    val endPos = new Array[Int](numTasks)
    val increment = src.height / numTasks
    var pos = 0
    for(i <- 0 to (numTasks-1))
    {
      startPos(i) = pos
      pos = pos+increment
      if(pos >= src.height - 1)
        pos = src.height
      if(i + 1 > numTasks-1)
        pos = src.height
      endPos(i) = pos
    }
    
    for(i <- 0 to  (numTasks-1))
    {
      tasks += task(blur(src, dst, startPos(i), endPos(i), radius))
    }
    for(i <- tasks.toList) i.join
  }

}
