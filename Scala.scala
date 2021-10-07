object CaesarCipher {

  def main(args: Array[String]) {
    var ciph: String = encrypt("This is a test string from Alan", 8)
    println(ciph)
    println(decrypt(ciph,8))
    solve("HAL", 26)
  }

  def encrypt(text: String, shift: Int): String = {
    var msg: String = text.toUpperCase()
    var cipher: String = ""

    for (i <- 0 to msg.length()-1) {
      var c: Int = msg.charAt(i).toInt
      if (c != ' '.toInt) {
        c = c + (shift % 26)
        if (c > 'Z') {
          c = c - 26
        }
      }
      cipher += c.toChar
    }
    return cipher
  }

  def decrypt(text: String, shift: Int): String = {
    var msg: String = text.toUpperCase()
    var cipher: String = ""

    for (i <- 0 to msg.length()-1) {
      var c: Int = msg.charAt(i).toInt
      if (c != ' '.toInt) {
        c = c - (shift % 26)
        if (c < 'A') {
          c = c + 26
        }
      }
      cipher += c.toChar
    }
    return cipher
  }

  def solve(text: String, maxVal: Int): Unit ={
    var msg: String = text.toUpperCase()
    var cipher: String = ""

    for (n <- 0 to maxVal){
      cipher = ""
      for (i <- 0 to msg.length()-1) {
        var c: Int = msg.charAt(i).toInt
        if (c != ' '.toInt) {
          c = c + (n % 26)
          if (c > 'Z') {
            c = c - 26
          }
        }
        cipher += c.toChar
      }
      println("Ceasar " + n + ": " + cipher)
    }
  }
}
