package collections

import scala.util.Random

case class Element(item: Int)

case class Experiment(bucket: List[Element])

object Experiment {
  def generateBuckets(n: Int, acc: List[Experiment] = List.empty[Experiment]): List[Experiment] = {
    n match {
      case 0 => acc
      case _ => {
        val experiment: Experiment = Experiment(Element(0) :: Element(0) :: Element(0) :: Element(1) :: Element(1) :: Element(1) :: Nil)
        generateBuckets(n - 1, experiment :: acc)
      }
    }
  }

  def pickUp(experiment: Experiment): Boolean = {
    val random = new Random()
    val firstIndex: Int = random.nextInt(experiment.bucket.size -1)
    val first: Element = experiment.bucket(firstIndex)
    first match {
      case Element(1) => false
      case Element(0) => {
        val newBucket = experiment.bucket.take(firstIndex) ++ experiment.bucket.drop(firstIndex + 1)
        val secondIndex = random.nextInt(newBucket.size -1)
        val second = newBucket(secondIndex)
        second match {
          case Element(1) => true
          case Element(0) => false
        }
      }
    }
  }
}

object HwDatacollection {
  def main(args: Array[String]): Unit = {
    val n = 1000000
    val buckets = Experiment.generateBuckets(n)

    val countRightExp: Int = buckets.map(experiment => {
      val exp = Experiment.pickUp(experiment)
      if (exp) 1 else 0
    }).foldLeft(0)((a, b) => a + b)

    println(countRightExp)
    println(buckets.size)
    println((countRightExp.toDouble / n) * 2)
  }
}
