import model_2048.Model._
import org.scalatest.{FlatSpec, Matchers}

class ModelSpec extends FlatSpec with Matchers {
  it should "swipe left" in {
    val result = swipe(
      Matrix(3,true,Array(None,None,None,None,Some(2),None,None,None,None)),
      Left
    )
    println(debug(result.state.array))
    result.state shouldBe Matrix(3,true,Array(None,None,None,Some(2),None,None,None,None,None))
    result.moves shouldBe Array(Move((1,1),(0,1),false))
    result.doubles shouldBe Array()
  }
}
