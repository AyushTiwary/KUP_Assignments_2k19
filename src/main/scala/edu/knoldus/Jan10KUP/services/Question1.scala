package edu.knoldus.Jan10KUP.services

import scala.annotation.tailrec

/**
  * Write a program using string interpolation and a list.*/

class Question1 {

    def printIndexWithEachElement(list: List[Int]): List[Int] = {
        @tailrec
        def printElement(elements: List[Int], list: List[Int]): List[Int] = {
            elements match {
                case Nil => println("List with index printed")
                    list
                case head :: tail => val index = indexOfElement(head, list)
                    println(s"$index = $head")
                    printElement(tail, list)
                case _ => throw new NoSuchElementException
            }
        }
        printElement(list, list)
    }

    private def indexOfElement(element: Int, list: List[Int]): Int = {
        @tailrec
        def findIndex(index: Int, list: List[Int]): Int = {
            list match {
                case head :: Nil => if(head == element) index
                else throw new NoSuchElementException
                case head :: tail => if(head == element) index
                else findIndex(index + 1, tail)
                case _ => throw new NoSuchElementException
            }
        }
        findIndex(0, list)
    }
}
