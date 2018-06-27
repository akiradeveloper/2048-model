import model_2048.Model
import org.scalatest.FlatSpec

class Swipe0Spec extends FlatSpec {
  import Model._
  it should "swipe 2" in {
    val result = swipe0(Array(Some(2),None,None))
    assert(result.state === Array(Some(2),None,None))
    assert(result.moves === Array(Move(0,0,false)))
    assert(result.doubles === Array())
  }
  it should "swipe _,2" in {
    val result = swipe0(Array(None, Some(2), None))
    assert(result.state === Array(Some(2),None,None))
    assert(result.moves === Array(Move(1,0,false)))
    assert(result.doubles === Array())
  }
  it should "swipe 2,_,2" in {
    val result = swipe0(Array(Some(2),None,Some(2)))
    assert(result.state === Array(Some(4),None,None))
    assert(result.moves === Array(Move(0,0,false),Move(2,0,true)))
    assert(result.doubles === Array(0))
  }
  it should "swipe _,2,4" in {
    val result = swipe0(Array(None,Some(2),Some(4)))
    assert(result.state === Array(Some(2),Some(4),None))
    assert(result.moves === Array(Move(1,0,false),Move(2,1,false)))
    assert(result.doubles === Array())
  }
  it should "swipe _,2,2" in {
    val result = swipe0(Array(None,Some(2),Some(2)))
    assert(result.state === Array(Some(4),None,None))
    assert(result.moves === Array(Move(1,0,false),Move(2,0,true)))
    assert(result.doubles === Array(0))
  }
  it should "swipe 2,2,2" in {
    val result = swipe0(Array(Some(2),Some(2),Some(2)))
    assert(result.state === Array(Some(4),Some(2),None))
    assert(result.moves === Array(Move(0,0,false),Move(1,0,true),Move(2,1,false)))
    assert(result.doubles === Array(0))
  }
  it should "swipe 2,2,2,2" in {
    val result = swipe0(Array(Some(2),Some(2),Some(2),Some(2)))
    assert(result.state === Array(Some(4),Some(4),None,None))
    assert(result.moves === Array(Move(0,0,false),Move(1,0,true),Move(2,1,false),Move(3,1,true)))
    assert(result.doubles === Array(0,1))
  }
  it should "swipe 4,2,2" in {
    val result = swipe0(Array(Some(4),Some(2),Some(2)))
    assert(result.state === Array(Some(4),Some(4),None))
    assert(result.moves === Array(Move(0,0,false),Move(1,1,false),Move(2,1,true)))
    assert(result.doubles === Array(1))
  }
  it should "swipe 2,4,_,2,2" in {
    val result = swipe0(Array(Some(2),Some(4),None,Some(2),Some(2)))
    assert(result.state === Array(Some(2),Some(4),Some(4),None,None))
    assert(result.moves === Array(Move(0,0,false),Move(1,1,false),Move(3,2,false),Move(4,2,true)))
    assert(result.doubles === Array(2))
  }
}
