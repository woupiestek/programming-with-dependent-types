package nl.woupiestek.equalizer.parsing

object Tracer {

  val console = System.console()
  var counter = 1 << 16

  def log(msg: => String) =
    if (counter > 0) {
      counter -= 1
    } else {
      console.printf(msg)
      counter = 1 << 16
    }
}
