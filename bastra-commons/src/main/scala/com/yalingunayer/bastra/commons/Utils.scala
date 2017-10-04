package com.yalingunayer.bastra.commons

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.UUID

object Utils {
  /**
   * Asynchronously read a line from the standard input
   */
  def readResponse: Future[String] = Future {
    scala.io.StdIn.readLine()
  }
  
  /**
   * Try reading an integer from the standard input
   */
  def readNumericResponse: Future[Option[Int]] = {
    readResponse.map(s => {
      try {
        Some(s.toInt)
      } catch {
        case _: Throwable => None
      }
    })
  }
  
  /**
   * Convert a yes/no response to boolean for easier use
   */
  def readBooleanResponse: Future[Boolean] = {
    readResponse.map(s => s match {
      case "y" | "yes" | "1" | "" => true
      case _ => false
    })
  }
  
  /**
   * Remove the last occurrence of an item from a list
   * Note that this is a very naive implementation and is probably very inefficient
   */
  def removeLast[A](list: List[A], item: A): List[A] = {
    def remove(iter: List[A]): List[A] = iter match {
      case x :: y => {
        if (x == item) y
        else x :: remove(y)
      }
      case Nil => Nil
    }
    
    remove(list.reverse).reverse
  }
  
  /**
   * Generate a random UUID
   */
  def uuid(): String = UUID.randomUUID().toString()
}
