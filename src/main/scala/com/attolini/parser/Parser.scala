package com.attolini.parser

import com.attolini.compiler.{Compiler, Location, ParserError}
import com.attolini.lexer.{DOT, IDENTIFIER, LITERAL, Token}
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Parser extends Parsers {

  override type Elem = Token

  class QueryReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token        = tokens.head
    override def rest: Reader[Token] = new QueryReader(tokens.tail)
    override def pos: Position       = NoPosition
    override def atEnd: Boolean      = tokens.isEmpty
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id @ IDENTIFIER(name) => id })
  }

  private def literal: Parser[LITERAL] = positioned {
    accept("string literal", { case lit @ LITERAL(name) => lit })
  }

  def tableName: Parser[Table] = positioned {
    (identifier ~ DOT() ~ identifier) ^^ { case schema ~ s ~ name => Table(schema.str + s.toString + name.str) }
  }

//  case class AndThen(step1: WorkflowAST, step2: WorkflowAST) extends WorkflowAST
//  case class ReadInput(inputs: Seq[String]) extends WorkflowAST
//  case class CallService(serviceName: String) extends WorkflowAST
//  case class Choice(alternatives: Seq[ConditionThen]) extends WorkflowAST
//  case object Exit extends WorkflowAST
//
//  sealed trait ConditionThen { def thenBlock: WorkflowAST }
//  case class IfThen(predicate: Condition, thenBlock: WorkflowAST) extends ConditionThen
//  case class OtherwiseThen(thenBlock: WorkflowAST) extends ConditionThen
//
//  sealed trait Condition
//  case class Equals(factName: String, factValue: String) extends Condition

  val fileReader = scala.io.Source.fromResource("nfc_populate.cql")

  val fileContent = fileReader.mkString //getLines.toList.mkString
  val queries     = fileContent.split(";")
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

  def program: Parser[AST] = { phrase(tableName) }

//  def statement: Parser[AST] = positioned {
//    insert
//  }
  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new QueryReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next)  => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}

object TestParser extends App {

//  val input: String =
//    "INSERT INTO config_verticali_nfc$create_table (keyspace_name,table_name,primary_key,clustering_order,all_fields) " +
//      "VALUES " +
//      "('verticali_nfc','list_entita', ['end_sin_sinistro_id','end_por_portafoglio_id','end_en_dan_id'],['end_en_dan_id ASC'],['end_sin_sinistro_id bigint','end_por_portafoglio_id bigint','end_en_dan_id bigint','end_liq_liquidatore_id bigint']);"

  val input = "INSERT INTO config_verticali_nfc$create_table"
  Compiler(input).getOrElse("")

}
