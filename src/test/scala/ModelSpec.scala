import model_2048.Model._
import org.scalatest.{FlatSpec, Matchers}

class ModelSpec extends FlatSpec with Matchers {
  it should "swipe left" in {
    val result = swipe(
      Matrix(3,true,Seq(None,None,None,None,Some(2),None,None,None,None)),
      Left
    )
    result.state shouldBe Matrix(3,true,Seq(None,None,None,Some(2),None,None,None,None,None))
    result.moves shouldBe Seq(Move((1,1),(0,1),false))
    result.doubles shouldBe Seq.empty
  }
  it should "swipe right" in {
    val result = swipe(
      Matrix(3,true,Seq(None,None,None,None,Some(2),None,None,None,None)),
      Right
    )
    result.state shouldBe Matrix(3,true,Seq(None,None,None,None,None,Some(2),None,None,None))
    result.moves shouldBe Seq(Move((1,1),(2,1),false))
    result.doubles shouldBe Seq.empty
  }
  it should "swipe up" in {
    val result = swipe(
      Matrix(3,true,Seq(None,None,None,None,Some(2),None,None,None,None)),
      Up
    )
    result.state shouldBe Matrix(3,false,Seq(None,None,None,Some(2),None,None,None,None,None))
    result.moves shouldBe Seq(Move((1,1),(1,0),false))
    result.doubles shouldBe Seq.empty
  }
  it should "swipe down" in {
    val result = swipe(
      Matrix(3,true,Seq(None,None,None,None,Some(2),None,None,None,None)),
      Down
    )
    result.state shouldBe Matrix(3,false,Seq(None,None,None,None,None,Some(2),None,None,None))
    result.moves shouldBe Seq(Move((1,1),(1,2),false))
    result.doubles shouldBe Seq.empty
  }
}
