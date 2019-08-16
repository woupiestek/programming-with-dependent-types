package nl.woupiestek.equalizer.parsing

object Tracer {

  val console = System.console()
  val interval = 1 << 16
  var counter = interval

  def log(msg: => String) =
    if (counter > 0) {
      counter -= 1
    } else {
      console.printf(msg)
      counter = interval
    }
}
