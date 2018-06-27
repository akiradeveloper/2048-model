package model_2048

object Model {
  type Index = Int

  type Number = Option[Int]

  case class Move(from: Index, to: Index, conflict: Boolean)

  case class Swipe0Result(
    state: Array[Number],
    moves: Array[Move],
    doubles: Array[Index]
  )

  case class Acc(
    state: Array[Number],
    moves: Array[Move],
    doubles: Array[Index],
    cursor: Index
  )

  def swipe0(initState: Array[Number]): Swipe0Result = {

    val initAcc = Acc(
      state = Array.fill[Number](initState.length){ None },
      moves = Array.empty,
      doubles = Array.empty,
      cursor = 0
    )
    val acc = initState.zipWithIndex.foldLeft(initAcc){ (acc: Acc, x: (Number, Index)) =>
      val traverseAt = x._2
      val traverseNum0 = x._1
      traverseNum0 match {
        case None => acc
        case Some(traverseNum) =>
          val cursorAt = acc.cursor
          val cursorNum0 = acc.state(acc.cursor)
          cursorNum0 match {
            // cursor position is empty
            case None => Acc(
              state = acc.state.updated(cursorAt, Some(traverseNum)),
              moves = acc.moves :+ Move(traverseAt, cursorAt, false),
              doubles = acc.doubles,
              acc.cursor
            )

            // conflict
            case Some(cursorNum) if cursorNum == traverseNum => Acc(
              state = acc.state.updated(cursorAt, Some(traverseNum * 2)),
              moves = acc.moves :+ Move(traverseAt, cursorAt, true),
              doubles = acc.doubles :+ cursorAt,
              cursor = cursorAt + 1
            )

            // no conflict
            case Some(cursorNum) => Acc(
              state = acc.state.updated(cursorAt + 1, Some(traverseNum)),
              moves = acc.moves :+ Move(traverseAt, cursorAt + 1, false),
              doubles = acc.doubles,
              cursor = cursorAt + 1
            )
          }
      }
    }
    Swipe0Result(acc.state, acc.moves, acc.doubles)
  }
}