package com.attolini.parser

import com.attolini.compiler.{Compiler, Location, ParserError}
import com.attolini.lexer._
import com.attolini.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Parser extends Parsers {

  override type Elem = Token

  class QueryReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token        = tokens.head
    override def rest: Reader[Token] = new QueryReader(tokens.tail)
    override def pos: Position       = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def atEnd: Boolean      = tokens.isEmpty
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id @ IDENTIFIER(name) => id })
  }

  private def literal: Parser[LITERAL] = positioned {
    accept("string literal", { case lit @ LITERAL(name) => lit })
  }

  def tableName: Parser[Table] = positioned {
    (identifier ~ DOT() ~ identifier) ^^ { case schema ~ s ~ name => Table(schema.str + "." + name.str) }
  }

  def columns: Parser[Columns] = positioned {
    (LBRACKET() ~ rep(identifier ~ COMMA()) ~ identifier ~ RBRACKET()) ^^ {
      case _ ~ inputs ~ IDENTIFIER(lastInput) ~ _ => Columns(inputs.map(_._1.str) ++ Seq(lastInput))
    }
  }
//  val fileReader = scala.io.Source.fromResource("nfc_populate.cql")
//
//  val fileContent = fileReader.mkString //getLines.toList.mkString
//  val queries     = fileContent.split(";")
  //  println(queries.length)
//  for (q <- queries) println(q)
//
//  trait Query {
//    def apply(): Map[Any, Any]
//  }
//
//  case class CreateTable(mappa: Map[String, String]) extends Query {
//    override def apply() =
//      Map(mappa("keyspace_name")    -> "",
//          mappa("table_name")       -> "",
//          mappa("primary_key")      -> "",
//          mappa("clustering_order") -> "",
//          mappa("all_fields")       -> "")
//  }
//
//  case class Operation(mappa: Map[String, String]) extends Query {
//    override def apply() =
//      Map(
//        "input_ks"   -> "",
//        "input_tb"   -> "",
//        "output_ks"  -> "",
//        "output_tb"  -> "",
//        "op_type"    -> "",
//        "prg_op"     -> "",
//        "input_ks2"  -> "",
//        "input_tb2"  -> "",
//        "key_table"  -> "",
//        "key_table2" -> ""
//      )
//  }
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

  def insertListValue: Parser[InsertListValue] = positioned {
    (LBRACKET() ~ stringValue ~ COMMA() ~ stringValue ~ COMMA() ~ stringValueList ~ COMMA() ~ stringValueWithOptionList ~ COMMA() ~ stringValueWithOptionList ~ RBRACKET()) ^^ {
      case _ ~ keySpace ~ _ ~ table ~ _ ~ primaryk ~ _ ~ clusterOrder ~ _ ~ allFields ~ _ =>
        InsertListValue(keySpace.str, table.str, primaryk.list, clusterOrder.list, allFields.list)
    }
  }

  def queryInsert: Parser[QueryInsert] = positioned {
    (insertInto ~ tableName ~ columns ~ valuesKey ~ insertListValue) ^^ {
      case insertKey ~ table ~ cols ~ vK ~ colsV => QueryInsert(insertKey, table, cols, vK, colsV)
    }
  }

  def query: Parser[AST] = positioned {
    queryInsert | endQuery
  }

  def instructions: Parser[AST] = positioned { rep1(query) ^^ { case stmtList => stmtList reduceRight NextStep } }

  def program: Parser[AST] = positioned { phrase(instructions) }

  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new QueryReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next)  => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}

object TestParser extends App {

  val input: String =
    "INSERT INTO config_verticali_nfc.create_table (keyspace_name,table_name,primary_key,clustering_order,all_fields) VALUES " +
      "('verticali_nfc','list_entita', ['end_sin_sinistro_id','end_por_portafoglio_id','end_en_dan_id'],['end_en_dan_id ASC'],['end_sin_sinistro_id bigint','end_por_portafoglio_id bigint','end_en_dan_id bigint','end_liq_liquidatore_id bigint']);"

//  val input =
//    "INSERT INTO config_verticali_nfc.create_table (keyspace_name,table_name,primary_key,clustering_order,all_fields) values ;"
  println(Compiler(input))

}
