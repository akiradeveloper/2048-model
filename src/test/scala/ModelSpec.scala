import model_2048.Model._
import org.scalatest.FlatSpec

class ModelSpec extends FlatSpec {
  it should "swipe left" in {
    val result = swipe(
      Matrix(3,true,Array(None,None,None,None,Some(2),None,None,None,None)),
      Left
    )
    assert(result.state === Matrix(3,true,Array(None,None,None,Some(2),None,None,None,None,None)))
    assert(result.moves === Array(Move((1,1),(0,1),false)))
    assert(result.doubles === Array())
  }
}
