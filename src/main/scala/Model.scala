package model_2048

object Model0 {
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
  def swipe(initState: Array[Number], upward: Boolean): Swipe0Result = {
    if (!upward) {
      swipe0(initState)
    } else {
      val initStateRev = initState.reverse
      val result = swipe0(initStateRev)
      val N = initState.length
      def _invert(i: Index): Index = {
        N - 1 - i
      }
      def __invert(m: Move): Move = {
        Move(
          _invert(m.from),
          _invert(m.to),
          m.conflict
        )
      }
      Swipe0Result(
        state = result.state.reverse,
        moves = result.moves.map(__invert),
        doubles = result.doubles.map(_invert)
      )
    }
  }
  def swipe0(initState: Array[Number]): Swipe0Result = {
    val initAcc = Acc(
      state = Array.fill[Number](initState.length){ None },
      moves = Array.empty,
      doubles = Array.empty,
      cursor = 0
    )
    val acc = initState.zipWithIndex.foldLeft(initAcc){ case (acc: Acc, x: (Number, Index)) =>
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

object Model {
  type Index = (Int, Int)
  type Number = Option[Int]
  case class Move(from: Index, to: Index, conflict: Boolean)
  trait Swipe
  object Up extends Swipe
  object Down extends Swipe
  object Left extends Swipe
  object Right extends Swipe
  case class Matrix(n: Int, xAxis: Boolean, array: Array[Number]) {
    require(array.length == n * n)
    private def to1D(ij: Index): Int = {
      val (i, j) = ij
      if (xAxis) {
        n * j + i
      } else {
        n * i + j
      }
    }
    // very slow. sub-optimal
    def updated(ij: Index, x: Number): Matrix = {
      Matrix(n, xAxis, array.updated(to1D(ij), x))
    }
    def toArrays(xAxis: Boolean): Array[Array[Number]] = {
      val array1D: Seq[Number] = if (this.xAxis == xAxis) {
        array
      } else {
        (if (xAxis) {
          (0 until n).map(j => (0 until n).map(i => (i, j)))
        } else {
          (0 until n).map(i => (0 until n).map(j => (i, j)))
        }).flatten.map(ij => array(to1D(ij)))
      }
      array1D.sliding(size=n, step=n).toArray.map(_.toArray)
    }
  }
  def debug(arr: Array[Number]): String = {
    "[" + arr.map(_.toString).mkString(",") + "]"
  }
  case class SwipeResult0(
    state: Array[Number],
    moves: Array[Move],
    doubles: Array[Index]
  )
  def fold(n: Int, xAxis: Boolean, results: Seq[SwipeResult0]): SwipeResult = {
    SwipeResult(
      state = Matrix(n, xAxis, results.map(_.state).flatten.toArray),
      moves = results.map(_.moves).flatten.toArray,
      doubles = results.map(_.doubles).flatten.toArray
    )
  }
  case class SwipeResult(
    state: Matrix,
    moves: Array[Move],
    doubles: Array[Index]
  )
  def swipe(initState: Matrix, swipe: Swipe): SwipeResult = {
    swipe match {
      case Left =>
        val results: Seq[SwipeResult0] = initState.toArrays(xAxis = true).map(arr => Model0.swipe(arr, upward = false)).zipWithIndex.map { case (result: Model0.Swipe0Result, j: Int) =>
          SwipeResult0(
            state = result.state,
            moves = result.moves.map(m0 => Model.Move(from = (m0.from, j), to = (m0.to, j), m0.conflict)),
            doubles = result.doubles.map(d0 => (d0, j))
          )
        }
        fold(initState.n, xAxis = true, results)
    }
  }
}
