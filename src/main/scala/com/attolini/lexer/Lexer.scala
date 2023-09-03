package com.attolini.lexer

import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {

  sealed trait Token
  case class IDENTIFIER(str: String)  extends Token
  case class LITERAL(str: String)     extends Token
  case class INDENTATION(spaces: Int) extends Token

  override def skipWhitespace = true
  override val whiteSpace     = "[ \t\r\f]+".r

  def identifier: Parser[IDENTIFIER] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str =>
      IDENTIFIER(str)
    }
  }

  def literal: Parser[LITERAL] = {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

  def indentation: Parser[INDENTATION] = {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  case object INSERT     extends Token
  case object INTO       extends Token
  case object VALUES     extends Token
  case object DOT        extends Token
  case object LBRACKET   extends Token
  case object RBRACKET   extends Token
  case object LSQBRACKET extends Token
  case object RSQBRACKET extends Token
  case object COMMA      extends Token
  case object COLON      extends Token
  case object SEMICOLON  extends Token
  case object INDENT     extends Token
  case object DEDENT     extends Token
  case object QUOTE      extends Token

  def insert             = "(?i)insert".r ^^ (_ => INSERT)
  def into               = "(?i)into".r ^^ (_ => INTO)
  def values             = "(?i)values".r ^^ (_ => VALUES)
  def dot                = "." ^^ (_ => DOT)
  def leftBracket        = "(" ^^ (_ => LBRACKET)
  def rightBracket       = ")" ^^ (_ => RBRACKET)
  def leftSquareBracket  = "[" ^^ (_ => LSQBRACKET)
  def rightSquareBracket = "]" ^^ (_ => RSQBRACKET)
  def comma              = "," ^^ (_ => COMMA)
  def colon              = ":" ^^ (_ => COLON)
  def semicolon          = ";" ^^ (_ => SEMICOLON)
  def quote              = "'" ^^ (_ => QUOTE)

  def tokens: Parser[List[Token]] = {
    phrase(
      rep1(insert | into | values | dot | leftBracket | quote | rightBracket | leftSquareBracket | rightSquareBracket
        | comma | colon | semicolon | literal | identifier | indentation)) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  private def processIndentations(tokens: List[Token], indents: List[Int] = List(0)): List[Token] = {
    tokens.headOption match {

      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level,
      // producing a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT)) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => DEDENT)

    }
  }
  trait CompilationError
  case class LexerError(msg: String) extends CompilationError

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next)  => Left(LexerError(msg))
      case Success(result, next) => Right(result)
    }
  }

}
object Test extends App {
  val input: String =
    "INSERT INTO config_verticali_nfc.create_table (keyspace_name,table_name,primary_key,clustering_order,all_fields) " +
      "VALUES " +
      "('verticali_nfc','list_entita', ['end_sin_sinistro_id','end_por_portafoglio_id','end_en_dan_id'],['end_en_dan_id ASC'],['end_sin_sinistro_id bigint','end_por_portafoglio_id bigint','end_en_dan_id bigint','end_liq_liquidatore_id bigint']);"

  println(Lexer(input).getClass)
}
