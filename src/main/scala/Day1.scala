import scala.io.Source

object Day1 {
  lazy val input: Iterator[Int] = Source.fromResource("day1.txt").getLines.map(x => x.toInt)

  def main(args: Array[String]): Unit = {
    val (input1, input2) = input.duplicate
    println(part1(input1))
    println(part2(input2))
  }

  def part1(vals: Iterator[Int]): Int = {
    vals.sliding(2).count(x => x.head < x.tail.head)
  }

  def part2(vals: Iterator[Int]): Int = {
    part1(vals.sliding(3).map(x => x.sum))
  }
}
