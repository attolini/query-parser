package com.attolini.lexer

import com.attolini.compiler.{LexerError, Location}
import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {

  override def skipWhitespace = true
  override val whiteSpace     = "[ \t\r\f]+".r

  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str =>
      IDENTIFIER(str)
    }
  }

  def literal: Parser[LITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

  def number: Parser[NUMBER] = positioned {
    "[0-9]".r ^^ { num =>
      NUMBER(num)
    }
  }

  def indentation: Parser[INDENTATION] = positioned {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def insertInto         = positioned { "(?i)insert[ ]*(?i)into".r ^^ (_ => INSERTINTO()) }
  def values             = positioned { "(?i)values".r ^^ (_ => VALUES()) }
  def dot                = positioned { "." ^^ (_ => DOT()) }
  def leftBracket        = positioned { "(" ^^ (_ => LBRACKET()) }
  def rightBracket       = positioned { ")" ^^ (_ => RBRACKET()) }
  def leftSquareBracket  = positioned { "[" ^^ (_ => LSQBRACKET()) }
  def rightSquareBracket = positioned { "]" ^^ (_ => RSQBRACKET()) }
  def comma              = positioned { "," ^^ (_ => COMMA()) }
  def colon              = positioned { ":" ^^ (_ => COLON()) }
  def semicolon          = positioned { ";" ^^ (_ => SEMICOLON()) }
  def quote              = positioned { "'" ^^ (_ => QUOTE()) }

  def tokens: Parser[List[Token]] = {
    phrase(
      rep1(insertInto | values | dot | leftBracket | quote | rightBracket | leftSquareBracket | rightSquareBracket
        | comma | colon | semicolon | literal | identifier | indentation)) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  private def processIndentations(tokens: List[Token], indents: List[Int] = List(0)): List[Token] = {
    tokens.headOption match {

      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT() :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level,
      // producing a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => DEDENT())

    }
  }

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next)  => Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}

object TestLexer extends App {
  val input: String =
    "INSERT INTO keyspace.table (keyspace_name,table_name,primary_key) " +
      "VALUES " +
      "('ks','table_name', ['pk1','pk2','end_en_dan_id']);"

  println(Lexer(input))
}
