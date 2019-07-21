package example

import example.Validations.HasSignupConfig
import zio._
import zio.console._

object ValidationApp extends App {
  def validateUserString(user: User): ZIO[HasSignupConfig, Nothing, String] =
    Validations
      .validateUser(user)
      .const("succeded")
      .catchAll(c =>
        ZIO.succeed(c.continue(new LoginErr[String] with PasswordErr[String] {
          def tooLong(maxLength: Int): String = s"login should not be longer than $maxLength"
          def badFormat: String = s"login is unacceptable"
          def tooShort(required: Int): String = s"password is too short, required: $required"
          def doesNotContain(chars: String): String = s"password should contain one $chars"
        })))

  val users = List(User("oleg", "sdfsdfsdf"),
                   User("lolcat", "sdfdf123123"),
                   User("fedot kamil", "sdfsdfsdfsdfsdfss##11"),
                   User("yan", "dfgfdgfg123123(*(&"))

  val program =
      ZIO.foreach(users)(user => validateUserString(user) >>= (res => putStrLn(s"$user : $res")))

  def run(args: List[String]): ZIO[Console, Nothing, Int] = {
    program.const(0).provideSome(c ⇒ new HasLoginConfig with HasPasswordConfig with Console {
      val loginConfig = LoginConfig(
        maxLoginLength = 10,
        regex = "\\w+"
      )
      val passwordConfig = PasswordConfig(
        minLength = 10,
        groups = List(
          "digit" -> "\\d".r,
          "letter" -> "\\w".r,
          "special character" -> "@#$%^&*():;'<>".mkString("[\\", "\\", "]").r
        )
      )
      val console = c.console
    })
  }
}
