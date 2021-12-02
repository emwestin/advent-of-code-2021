import Day1.input

import scala.io.Source

object Day2 {
  lazy val input: List[String] = Source.fromResource("day2.txt").mkString.replace('\n', ' ').split(' ').toList

  def main(args: Array[String]): Unit = {
    println(part1(input))
    println(part2(input))
  }

  def part1(vals: List[String]): Int = {
    input
      .grouped(2)
      .map(e => e.head match {
        case "forward" => e(1).toInt -> 0
        case "down" => 0 -> e(1).toInt
        case "up" => 0 -> (e(1).toInt * -1)
      })
      .toList
      .groupMapReduce(kv => kv._1)(kv => kv._2)(_ + _)
      .values
      .product
  }

  def part2(vals: List[String]): Int = {
    val intermediate = input
      .grouped(2)
      .map(e => e.head match {
        case "forward" => e(1).toInt -> 0
        case "down" => 0 -> e(1).toInt
        case "up" => 0 -> (e(1).toInt * -1)
      })
      .foldLeft(0, 0, 0)((p, c) => {
        (p._1 + c._1, p._2 + ((p._3 + c._2) * c._1), p._3 + c._2)
      })
      intermediate._1 * intermediate._2
  }
}
