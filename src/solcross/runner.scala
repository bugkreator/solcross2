package solcross

object runner extends App {
  val b = Board.initialBoard
  println("****")
  val startTime: Long = System.currentTimeMillis
  val l = b.getBestMoveList()
  Board.playMoves(b, l).foreach(println)
  val endTime: Long = System.currentTimeMillis()
  println(endTime - startTime)
}