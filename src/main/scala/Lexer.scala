package com.jroesch.JSON

object Lexer {
	sealed trait Token
	
	/* Structural Characters */
	sealed trait Bracket extends Token
	
	case object TLeftCurly extends Bracket {
		override def toString = "{"
	}
	
	case object TRightCurly extends Bracket {
		override def toString = "}"
	}
	
	case object TLeftSquare extends Bracket {
		override def toString = "["
	}
	
	case object TRightSquare extends Bracket {
		override def toString = "]"
	}
	
	case object TColon extends Token {
		override def toString = ":"
	}
	
	case object TComma extends Token {
		override def toString = ","
	}
	
	/* Keywords */
	case object TTrue extends Token {
		override def toString = "true"
	}
	
	case object TFalse extends Token {
		override def toString = "false"
	}
	
	case object TNull extends Token {
		override def toString = "null"
	}
	
	/* Literals */
	case class TNum(d: Double) extends Token {
		override def toString = d.toString
	}
	
	case class TString(s: String) extends Token {
		override def toString = s.toString
	}
	
	/* End of File Token */
	case object TEOF extends Token {
		override def toString = "EOF"
	}
}

class Lexer(val input: String) {
	/* Bring Token types into Scope */
	import Lexer._
	/* Uh-Oh icky mutable state */
	var position = 0
	var lineNumber = 1

	/* manipulate the stream */
	def nextCharacter = input(position)
	def eatCharacter()  = position += 1
	def streamAtHead = input.substring(position)

	//val terror = s"
 // class LexerException extends Exception(terror)

	/* Keywords Regex */
	/* can optimize with literal matches */
	val rfalse = "^false".r
	val rtrue = "^true".r
	val rnull = "^null".r

	/* Literal Regex */
	
	val rnumber = """^-?(0|[1-9]\d*)(\.\d+|(e|E)(-|\+)?\d+)?""".r
	
	/* RFC 4627
		 number = [ minus ] int [ frac ] [ exp ]

		 decimal-point = %x2E       ; .

		 digit1-9 = %x31-39         ; 1-9

		 e = %x65 / %x45            ; e E

		 exp = e [ minus / plus ] 1*DIGIT

		 frac = decimal-point 1*DIGIT

		 int = zero / ( digit1-9 *DIGIT )

		 minus = %x2D               ; -

		 plus = %x2B                ; +

		 zero = %x30                ; 0  
	*/

	val rstring = """"([^"\\\\]*|\\\\["\\\\bfnrt\/]|\\\\u[0-9a-f]{4})*"""".r
	
	/*  RFC 4627
			string = quotation-mark *char quotation-mark
	
			char = unescaped /
						 escape (
								 %x22 /          ; "    quotation mark  U+0022
								 %x5C /          ; \    reverse solidus U+005C
								 %x2F /          ; /    solidus         U+002F
								 %x62 /          ; b    backspace       U+0008
								 %x66 /          ; f    form feed       U+000C
								 %x6E /          ; n    line feed       U+000A
								 %x72 /          ; r    carriage return U+000D
								 %x74 /          ; t    tab             U+0009
								 %x75 4HEXDIG )  ; uXXXX                U+XXXX
			
			escape = %x5C              ; \
			
			quotation-mark = %x22      ; "
			
			unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
	*/

	def allTokens(): List[Token] = nextToken() match {
			case TEOF  => List(TEOF)
			case token => token :: allTokens()
	}

	def nextToken(): Token = {
		if (position >= input.length) return TEOF
		nextCharacter match {
			case '{' => { eatCharacter(); TLeftCurly    }
			case '}' => { eatCharacter(); TRightCurly   }
			case '[' => { eatCharacter(); TRightSquare  }
			case ']' => { eatCharacter(); TLeftSquare   }
			case ':' => { eatCharacter(); TColon        }
			case ',' => { eatCharacter(); TComma        }
			case ws if ws == ' '  || 
								 ws == '\t' || 
								 ws == '\n' || 
								 ws == '\r' 
							 => { eatCharacter(); nextToken() }
			case _   => regexMatch()
		}
	}
	
	/* can definitely be made cleaner */
	def regexMatch(): Token = {
		val t = rtrue findFirstIn streamAtHead match {
			case None => None
			case Some(tr) => { position += tr.length; Some(TTrue) }
		}

		val trueOrFalse = t orElse {
			rfalse findFirstIn streamAtHead match {
				case None => None
				case Some(fl) => { position += fl.length; Some(TFalse) }
			}
		}

		val keywords = trueOrFalse orElse { 
			rnull findFirstIn streamAtHead match {
				case None => None
				case Some(nl) => { position += nl.length; Some(TNull) }
			}
		}

		val keywordOrNum = keywords orElse {
			rnumber findFirstIn streamAtHead  match {
				case None    => None
				case Some(n) => { position += n.length; Some(TNum(n.toDouble)) }
			}
		}

		val keywOrNumOrString = keywordOrNum orElse { 
			rstring findFirstIn streamAtHead match {
				case None => None
				case Some(s) => { position += s.length; Some(TString(s)) }
			}
		}

		keywOrNumOrString getOrElse { throw new Exception("Lexer error") }
	}
}