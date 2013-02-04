package com.jroesch.JSON

object Parser {
	/* JSON Types */
	sealed trait JSON
	case class JObject(o: Map[JString, JSON]) extends JSON
	case class JArray(a: Vector[JSON]) extends JSON
	case class JString(s: String) extends JSON
	case class JNum(n: Double) extends JSON
	sealed trait JBool extends JSON
	case object JTrue extends JBool
	case object JFalse extends JBool
	case object JNull extends JSON
	/* look into using dynamic trait */
	//implicit def jobject2Map(jobj: JObject): Map[String,]
}

class Parser(val tk: Lexer) {
	import Lexer._ 
	import Parser._
	
	/* more mutable state */
	var tokens = tk.allTokens()
	
	def nextToken: Token = tokens match {
		case Nil     => throw new Exception("empty token stream")
		case t :: ts => t
	}
	
	def eatToken() = tokens match {
		case Nil => throw new Exception("empty token stream")
		case t :: ts => tokens = ts
	}
	
	def parseJSON(): Option[JSON] = pObject() match {
		case json@Some(_) => json
		case None         => pArray()
	}
		
	def pObject(): Option[JSON] = {
		nextToken match {
			case TLeftCurly => eatToken()
			case _          => return None
		}
		
		val obj = pObjectBody() match {
			case None      => Map[JString, JSON]()
			case Some(map) => map
		}
		
		nextToken match {
			case TRightCurly => eatToken()
			case _           => throw new Exception("parse error")
		}
		
		Some(JObject(obj))
	}
	
	def pObjectBody(): Option[Map[JString, JSON]] = pKeyValue() flatMap {
		case kv => nextToken match {
			case TComma => {
				eatToken()
				pObjectBody().map(_ + kv)
			}
			case _ => Some(Map(kv))
		}
	}
	
	def pKeyValue(): Option[(JString, JSON)] = {
		val key = pString() match {
			case Some(s@JString(_)) => s
			case _       => throw new Exception("parse Error")
		}
		
		nextToken match {
			case TColon => eatToken
			case _      => throw new Exception("parse error")
		}
		
		val value = 
			pString().orElse(pNum).orElse(pTrue).orElse(pFalse).orElse(pNull).getOrElse {
				throw new Exception("parse error")
			}
			
		Some((key, value))
	}

	def pArray(): Option[JSON] = {
		nextToken match {
			case TLeftSquare => eatToken()
			case _           => return None
		}
		
		val array = pArrayBody() match {
			case None      => Vector[JSON]()
			case Some(vec) => vec
		}
		
		nextToken match {
			case TRightSquare => eatToken()
			case _ => throw new Exception("parse error")
		}
		
		Some(JArray(array))
	}
	
	def pArrayBody(): Option[Vector[JSON]] = pObject() flatMap { 
		case obj => nextToken match {
			case TComma => {
				eatToken()
			  pArrayBody().map((v: Vector[JSON]) => obj +: v)
			}
			case _ => Some(Vector(obj))
		}
	}
	
	def pString(): Option[JSON] = nextToken match {
		case TString(s) => { eatToken(); Some(JString(s)) }
		case _       => None
	}
	
	def pNum(): Option[JSON] = nextToken match {
		case TNum(n) => { eatToken(); Some(JNum(n)) }
		case _       => None
	}
	
	def pTrue(): Option[JSON] = nextToken match {
		case TTrue => { eatToken(); Some(JTrue) }
		case _     => None
	}

	def pFalse(): Option[JSON] = nextToken match {
		case TFalse => { eatToken(); Some(JFalse) }
		case _      => None
	}

	def pNull(): Option[JSON] = nextToken match {
			case TNull => { eatToken(); Some(JNull) }
			case _     => None
	}
	
	/* def try(pf => Option[JSON]): Option[JSON] = {
		tk.try()
		pf match {
			case None => tk.fail()
		}
	} */
}