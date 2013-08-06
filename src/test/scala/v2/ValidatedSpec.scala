package v2

import org.scalatest.{FunSuite, FunSpec}

sealed trait Validated[+A] {
  def map[B](f: A => B): Validated[B] = this match {
    case Valid(value) => Valid(f(value))
    case i@Invalid(_) => i
  }
}

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

  describe("map") {
    it("answers valid for valid with function applied") {
      assert((Valid(1) map (_ + 1)) === Valid(2))
    }

    it("answers invalid for invalid") {
      assert(((Invalid("ups"): Validated[Int]) map (_ + 1)) === Invalid("ups"))
    }
  }
}

class ValidatedSamples extends FunSuite {

  def addOne(validated: Validated[Int]) = validated.map(_ + 1)

  def append(validated: Validated[String], string: String) = validated map (_ ++ string)

  def concat(validated: Validated[String], other: Validated[String]) = (validated, other) match {
    case (Valid(v1), Valid(v2)) => Valid(v1 ++ v2)
    case (Invalid(m1), Invalid(m2)) => Invalid(s"$m1, $m2")
    case (i@Invalid(_), _) => i
    case (_, i) => i
  }

  def createCode(prefix: Validated[String], index: Validated[Int], suffix: Validated[String]) =
    (prefix, index, suffix) match {
      case (Valid(p), Valid(idx), Valid(s)) => Valid(s"$p:$idx:$s")
      case (Invalid(m1), Invalid(m2), Invalid(m3)) => Invalid(s"$m1, $m2, $m3")
      case (Invalid(m1), Invalid(m2), _) => Invalid(s"$m1, $m2")
      //and so on...
    }

  test("concat valids is valid") {
    assert(concat(Valid("foo"), Valid("bar")) === Valid("foobar"))
  }

  test("concat invalid is invalid with messages combined") {
    assert(concat(Invalid("ups"), Invalid("ouch")) === Invalid("ups, ouch"))
  }

  test("concat valids and invalid is invalid") {
    assert(concat(Invalid("ups"), Valid("bar")) === Invalid("ups"))
    assert(concat(Valid("foo"), Invalid("ouch")) === Invalid("ouch"))
  }

  test("createCode") {
    //problem: the logic of the create code function is mixed with the Validated handling
    assert(createCode(Valid("foo"), Valid(2), Valid("bar")) === Valid("foo:2:bar"))
    assert(createCode(Invalid("ups"), Invalid("ouch"), Valid("bar")) === Invalid("ups, ouch"))
  }

}
