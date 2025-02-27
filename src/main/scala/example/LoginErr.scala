package example

import capture.Capture
import capture.Capture.Constructors

trait LoginErr[+A] {
  def tooLong(maxLength: Int): A
  def badFormat: A
}

//this probably should be generated by some macro
object LoginErr extends Constructors[LoginErr] {
  def tooLong(maxLength: Int) = Capture[LoginErr](_.tooLong(maxLength))
  val badFormat = Capture[LoginErr](_.badFormat)
}
