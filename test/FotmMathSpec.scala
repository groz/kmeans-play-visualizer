import models.FotmMath
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class FotmMathSpec extends Specification {
  import FotmMath._

  "FotmMath" should {

    "add vectors correctly" in {
      add(Vector(1.0, 1.0), Vector(2.0, 2.0)) must equalTo(Vector(3.0, 3.0))
    }

    "subtract vectors correctly" in {
      sub(Vector(1.0, 1.0), Vector(2.0, 2.0)) must equalTo(Vector(-1.0, -1.0))
    }

    "calc dist between vectors" in {
      dist(Vector(1.0, 1.0), Vector(2.0, 2.0)) must equalTo(1)
    }

    "divide vector by number correctly" in {
      div(Vector(1.0, 1.0), 2) must equalTo(Vector(.5, .5))
    }

    "calc vector length correctly" in {
      len(Vector(3.0, 4.0)) must equalTo (5.0)
    }
  }
}
