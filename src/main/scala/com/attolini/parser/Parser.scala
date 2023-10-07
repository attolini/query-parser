package com.attolini.parser

import com.attolini.compiler.{Location, ParserError}
import com.attolini.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class QueryReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token        = tokens.head
  override def rest: Reader[Token] = new QueryReader(tokens.tail)
  override def pos: Position       = tokens.headOption.map(_.pos).getOrElse(NoPosition)
  override def atEnd: Boolean      = tokens.isEmpty
}

class Parser extends Parsers {

  override type Elem = Token

  def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id @ IDENTIFIER(name) => id })
  }

  def literal: Parser[LITERAL] = positioned {
    accept("string literal", { case lit @ LITERAL(name) => lit })
  }

  def number: Parser[NUMBER] = positioned {
    accept("number", { case num @ NUMBER(name) => num })
  }

  def tableName: Parser[Table] = positioned {
    (identifier ~ DOT() ~ identifier) ^^ { case schema ~ s ~ name => Table(schema.str + "." + name.str) }
  }

  def columns: Parser[Columns] = positioned {
    (LBRACKET() ~ rep(identifier ~ COMMA()) ~ identifier ~ RBRACKET()) ^^ {
      case _ ~ inputs ~ IDENTIFIER(lastInput) ~ _ => Columns(inputs.map(_._1.str) ++ Seq(lastInput))
    }
  }

  def insertInto: Parser[InsertInto.type] = INSERTINTO() ^^ (_ => InsertInto)
  def valuesKey: Parser[ValuesKey.type]   = VALUES() ^^ (_ => ValuesKey)
  def endQuery: Parser[SemiColon.type]    = SEMICOLON() ^^ (_ => SemiColon)

  def stringValue: Parser[StringValue] = positioned {
    (QUOTE() ~ identifier ~ QUOTE()) ^^ { case _ ~ value ~ _ => StringValue(value.str) }
  }

  def stringValueWithOption: Parser[StringValue] = positioned {
    (QUOTE() ~ identifier ~ identifier ~ QUOTE()) ^^ {
      case _ ~ value ~ option ~ _ => StringValue(value.str + " " + option.str)
    }
  }

  def stringValueList: Parser[ValueList] = positioned {
    (LSQBRACKET() ~ rep(stringValue ~ COMMA()) ~ stringValue ~ RSQBRACKET()) ^^ {
      case _ ~ inputs ~ value ~ _ =>
        ValueList(inputs.map(_._1.str) ++ Seq(value.str))
    }
  }

  def stringValueWithOptionList: Parser[ValueList] = positioned {
    (LSQBRACKET() ~ rep(stringValueWithOption ~ COMMA()) ~ stringValueWithOption ~ RSQBRACKET()) ^^ {
      case _ ~ inputs ~ value ~ _ =>
        ValueList(inputs.map(_._1.str) ++ Seq(value.str))
    }
  }

  def query: Parser[AST] = positioned {
    endQuery
  }

  def instructions: Parser[AST] = positioned { rep1(query) ^^ { case stmtList => stmtList reduceRight Query } }

  def program: Parser[AST] = positioned { phrase(instructions) }

}

object Parser extends Parser {
  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new QueryReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next)  => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}
