package com.attolini.parser
import scala.util.parsing.input.Positional

trait AST                                extends Positional
case object InsertInto                   extends AST
case class Table(tableName: String)      extends AST
case class Columns(cols: Seq[String])    extends AST
case object ValuesKey                    extends AST
case object SemiColon                    extends AST
case class ValueList(list: Seq[String])  extends AST
case class StringValue(str: String)      extends AST
case class Query(elem1: AST, elem2: AST) extends AST

case class QueryInsert(insertInto: AST, table: AST, cols: AST, valuesKey: AST) extends AST // , insertValues: AST
