package module2

import module2.homework_hkt_impllicts.tupleF

object App {
  def main(args: Array[String]): Unit = {
    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)

    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)

    val r1: Unit = println(tupleF(optA, optB))
    val r2: Unit = println(tupleF(list1, list2))
  }
}
