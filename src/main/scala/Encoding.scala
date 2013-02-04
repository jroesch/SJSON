package com.jroesch.JSON

object Encodings {
	trait Encoding
	case object UTF8 extends Encoding
	case object UTF16 extends Encoding
	case object UTF32 extends Encoding

	/* 00 00 00 xx */
	val utf32BEMask = 0xFFF0;
	/* 00 xx 00 xx */
	val utf16BEMask = 0xF0F0;
	/* xx 00 00 00 */
	val utf32LEMask = 0x0FFF;
	/* xx 00 xx 00 */
	val utf16LEMask = 0x0F0F;
	/* xx xx xx xx */
	val utf8Mask = 0x0000;

	def encoding(bs: Array[Byte]): Encoding = {
		val header = (bs(0) << 6) + (bs(1) << 4) + (bs(2) << 2) + bs(3)
		UTF8
	}
}
