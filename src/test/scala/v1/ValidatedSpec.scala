package v1

import org.scalatest.{FunSuite, FunSpec}

sealed trait Validated[+A]

case class Valid[A](value: A) extends Validated[A]

case class Invalid(message: String) extends Validated[Nothing]

class ValidatedSpec extends FunSpec {

  describe("construction") {
    it("should allow valid value construction") {
      Valid(20)
    }
    it("should allow invalid value construction") {
      Invalid("data is invalid")
    }
  }
}

class ValidatedSamples extends FunSuite {

  def addOne(validated: Validated[Int]) = validated match {
    case Valid(value) => Valid(value + 1)
    case i@Invalid(_) => i
  }

  def append(validated: Validated[String], string: String) = validated match {
    case Valid(value) => Valid(value ++ string)
    case i@Invalid(_) => i
  }

  test("addOne on valid is valid") {
    assert(addOne(Valid(1)) === Valid(2))
  }

  test("addOne on invalid is invalid") {
    assert(addOne(Invalid("ups")) === Invalid("ups"))
  }

  test("append on valid is valid") {
    assert(append(Valid("foo"), "bar") === Valid("foobar"))
  }

  test("append on invalid is invalid") {
    assert(addOne(Invalid("ups")) === Invalid("ups"))
  }

}
