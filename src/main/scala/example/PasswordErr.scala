package example

import capture.Capture
import capture.Capture.Constructors

trait PasswordErr[+A] {
  def tooShort(required: Int): A
  def doesNotContain(chars: String): A
}

//this probably should be generated by some macro
object PasswordErr extends Constructors[PasswordErr] {
  def tooShort(required: Int) = Capture[PasswordErr](_.tooShort(required))
  def doesNotContain(chars: String) = Capture[PasswordErr](_.doesNotContain(chars))
}
