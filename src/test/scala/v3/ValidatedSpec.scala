package v3

import org.scalatest.{FunSuite, FunSpec}

sealed trait Validated[+A] {
  def map[B](f: A => B): Validated[B] = this match {
    case Valid(value) => Valid(f(value))
    case i@Invalid(_) => i
  }
}

object Validated {
  def pure[A](value: A): Validated[A] = Valid(value)

  def pure2[A, B, C](f: (A, B) => C): Validated[A => B => C] = pure(f.curried)

  def pure3[A, B, C, D](f: (A, B, C) => D): Validated[A => B => C => D] = pure(f.curried)

  implicit class ValidatedFunction[A, B](self: Validated[A => B]) {
    def apply(other: Validated[A]) = (self, other) match {
      case (Valid(f), Valid(v)) => Valid(f(v))
      case (Invalid(m1), Invalid(m2)) => Invalid(s"$m1, $m2")
      case (i@Invalid(_), _) => i
      case (_, i@Invalid(_)) => i
    }
  }
}

case class Valid[A](value: A) extends Validated[A]

case class Invalid(message: String) extends Validated[Nothing]


class ValidatedSpec extends FunSpec {

  import Validated._

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

  describe("apply") {
    it("applies function to value if both are valid") {
      assert((pure { x:Int => x + 1 } (Valid(1))) === Valid(2))
    }

    it("answers second iff second is invalid") {
      assert((pure { x:Int => x + 1 } (Invalid("ups"))) === Invalid("ups"))
    }

    it("answers first iff first is invalid") {
      assert((Invalid("ups")(Valid(2))) === Invalid("ups"))
    }

    it("combines messages of both iff both are invalid are invalid") {
      assert((Invalid("ups")(Invalid("ouch"))) === Invalid("ups, ouch"))
    }
  }
}

class ValidatedSamples extends FunSuite {

  import Validated._

  def addOne(validated: Validated[Int]) = validated.map(_ + 1)

  def append(validated: Validated[String], string: String) = validated map (_ ++ string)

  def concat(validated: Validated[String], other: Validated[String]) =
    pure2 { (x: String, y: String) => x ++ y }(validated)(other)

  def createCodePure(p:String, idx:Int, s:String) = s"$p:$idx:$s"

  def createCode(prefix: Validated[String], index: Validated[Int], suffix: Validated[String]) =
    pure3(createCodePure)(prefix)(index)(suffix)

  test("createCodePure") {
    assert(createCodePure("foo", 2, "bar") === "foo:2:bar")
  }

}
