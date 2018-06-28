import model_2048.Model0
import org.scalatest.{FlatSpec, Matchers}

class Model0Spec extends FlatSpec with Matchers {
  import Model0._
  it should "swipe 2" in {
  val result = swipe0(Seq(Some(2),None,None))
    result.state shouldBe Seq(Some(2),None,None)
    result.moves shouldBe Seq(Move(0,0,false))
    result.doubles shouldBe Seq.empty
  }
  it should "swipe 2,4" in {
    val result = swipe0(Seq(Some(2),Some(4)))
    result.state shouldBe Seq(Some(2),Some(4))
    result.moves shouldBe Seq(Move(0,0,false),Move(1,1,false))
    result.doubles shouldBe Seq.empty
  }
  it should "swipe _,2" in {
    val result = swipe0(Seq(None, Some(2), None))
    result.state shouldBe Seq(Some(2),None,None)
    result.moves shouldBe Seq(Move(1,0,false))
    result.doubles shouldBe Seq.empty
  }
  it should "swipe 2,_,2" in {
    val result = swipe0(Seq(Some(2),None,Some(2)))
    result.state shouldBe Seq(Some(4),None,None)
    result.moves shouldBe Seq(Move(0,0,false),Move(2,0,true))
    result.doubles shouldBe Seq(0)
  }
  it should "swipe _,2,4" in {
    val result = swipe0(Seq(None,Some(2),Some(4)))
    result.state shouldBe Seq(Some(2),Some(4),None)
    result.moves shouldBe Seq(Move(1,0,false),Move(2,1,false))
    result.doubles shouldBe Seq.empty
  }
  it should "swipe _,2,2" in {
    val result = swipe0(Seq(None,Some(2),Some(2)))
    result.state shouldBe Seq(Some(4),None,None)
    result.moves shouldBe Seq(Move(1,0,false),Move(2,0,true))
    result.doubles shouldBe Seq(0)
  }
  it should "swipe 2,2,2" in {
    val result = swipe0(Seq(Some(2),Some(2),Some(2)))
    result.state shouldBe Seq(Some(4),Some(2),None)
    result.moves shouldBe Seq(Move(0,0,false),Move(1,0,true),Move(2,1,false))
    result.doubles shouldBe Seq(0)
  }
  it should "swipe 2,2,2,2" in {
    val result = swipe0(Seq(Some(2),Some(2),Some(2),Some(2)))
    result.state shouldBe Seq(Some(4),Some(4),None,None)
    result.moves shouldBe Seq(Move(0,0,false),Move(1,0,true),Move(2,1,false),Move(3,1,true))
    result.doubles shouldBe Seq(0,1)
  }
  it should "swipe 4,2,2" in {
    val result = swipe0(Seq(Some(4),Some(2),Some(2)))
    result.state shouldBe Seq(Some(4),Some(4),None)
    result.moves shouldBe Seq(Move(0,0,false),Move(1,1,false),Move(2,1,true))
    result.doubles shouldBe Seq(1)
  }
  it should "swipe 2,4,_,2,2" in {
    val result = swipe0(Seq(Some(2),Some(4),None,Some(2),Some(2)))
    result.state shouldBe Seq(Some(2),Some(4),Some(4),None,None)
    result.moves shouldBe Seq(Move(0,0,false),Move(1,1,false),Move(3,2,false),Move(4,2,true))
    result.doubles shouldBe Seq(2)
  }
  it should "swipe downward" in {
    val init = Seq(None,Some(2),Some(2),Some(2),None)
    val result = swipe(init, upward = false)
    result.state shouldBe Seq(Some(4),Some(2),None,None,None)
    result.moves shouldBe Seq(Move(1,0,false),Move(2,0,true),Move(3,1,false))
    result.doubles shouldBe Seq(0)
  }
  it should "swipe upward" in {
    val init = Seq(None, Some(2), Some(2), Some(2), None)
    val result = swipe(init, upward = true)
    result.state shouldBe Seq(None,None,None,Some(2),Some(4))
    result.moves shouldBe Seq(Move(3,4,false),Move(2,4,true),Move(1,3,false))
    result.doubles shouldBe Seq(4)
  }
}
