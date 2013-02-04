package com.jroesch.JSON

package object JSON {
/* easy parse function */
  def parse(s: String): com.jroesch.JSON.Parser.JSON = {
    import com.jroesch.JSON.{ Lexer, Parser }
    val parser = new Parser(new Lexer(s))
    parser.parseJSON()
  }
}
