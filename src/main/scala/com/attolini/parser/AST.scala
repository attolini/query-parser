package com.attolini.parser
import scala.util.parsing.input.Positional

sealed trait AST                      extends Positional
case object InsertInto                extends AST
case class Table(tableName: String)   extends AST
case class Columns(cols: Seq[String]) extends AST

case object Values extends AST

case class Query(q: AST) extends AST
