package com.attolini.lexer
import scala.util.parsing.input.Positional

sealed trait Token extends Positional

case class IDENTIFIER(str: String)  extends Token
case class LITERAL(str: String)     extends Token
case class INDENTATION(spaces: Int) extends Token
case class INSERTINTO()             extends Token
case class VALUES()                 extends Token
case class DOT()                    extends Token
case class LBRACKET()               extends Token
case class RBRACKET()               extends Token
case class LSQBRACKET()             extends Token
case class RSQBRACKET()             extends Token
case class COMMA()                  extends Token
case class COLON()                  extends Token
case class SEMICOLON()              extends Token
case class INDENT()                 extends Token
case class DEDENT()                 extends Token
case class QUOTE()                  extends Token
