package nl.woupiestek.equalizer

import scalaz._
import Scalaz._
import org.scalatest._
import scala.util.control.TailCalls._
import scala.annotation.tailrec

class TrampTest extends FunSpec {
  private val many = 10000000

  describe("baseline") {
    @tailrec def even(n: Int, result: Boolean = true): Boolean = {
      if (n == 0) result
      else even(n - 1, !result)
    }

    it("passes the even/odd test") {
      info("start")
      val startTime = System.nanoTime
      info("result: " + even(many))
      info("millis: " + (System.nanoTime - startTime) / 1.0e6)
      succeed
    }

  }

  describe("TailRec") {
    def even(n: Int): TailRec[Boolean] = {
      if (n == 0) done(true)
      else tailcall(odd(n - 1))
    }

    def odd(n: Int): TailRec[Boolean] = {
      if (n == 0) done(false)
      else tailcall(even(n - 1))
    }

    it("passes the even/odd test") {
      info("start")
      val startTime = System.nanoTime
      info("result: " + even(many).result)
      info("millis: " + (System.nanoTime - startTime) / 1.0e6)
      succeed
    }
  }

  describe("While") {
    def even(n: Int): While[Boolean] = {
      if (n == 0) true.point[While]
      else While.suspend(odd(n - 1))
    }

    def odd(n: Int): While[Boolean] = {
      if (n == 0) false.point[While]
      else While.suspend(even(n - 1))
    }

    it("passes the even/odd test") {
      info("start")
      val startTime = System.nanoTime
      info("result: " + even(many).exhaust)
      info("millis: " + (System.nanoTime - startTime) / 1.0e6)
      succeed
    }
  }

  describe("While2") {
    def even(n: Int): While2[Boolean] = {
      if (n == 0) true.point[While2]
      else While2.suspend(odd(n - 1))
    }

    def odd(n: Int): While2[Boolean] = {
      if (n == 0) false.point[While2]
      else While2.suspend(even(n - 1))
    }

    it("passes countdown test") {
      def countdown(i: Int): While2[Assertion] = {
        println(i)
        if (i == 0) succeed.point[While2]
        else While2.suspend(countdown(i - 1))
      }
      info("start")
      val startTime = System.nanoTime
      While2.extract(countdown(100))
      info("millis: " + (System.nanoTime - startTime) / 1.0e6)
    }

    it("passes the even/odd test") {
      info("start")
      val startTime = System.nanoTime
      info("result: " + While2.extract(even(many)))
      info("millis: " + (System.nanoTime - startTime) / 1.0e6)
      succeed
    }
  }

}
