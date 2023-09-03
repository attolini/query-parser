package com.attolini.parser

object Parser extends App {

  val fileReader = scala.io.Source.fromResource("nfc_populate.cql")

  val fileContent = fileReader.mkString //getLines.toList.mkString
  val queries     = fileContent.split(";")
  //  println(queries.length)
  for (q <- queries) println(q)

  trait Query {
    def apply(): Map[Any, Any]
  }

  case class CreateTable(mappa: Map[String, String]) extends Query {
    override def apply() =
      Map(mappa("keyspace_name")    -> "",
          mappa("table_name")       -> "",
          mappa("primary_key")      -> "",
          mappa("clustering_order") -> "",
          mappa("all_fields")       -> "")
  }

  case class Operation(mappa: Map[String, String]) extends Query {
    override def apply() =
      Map(
        "input_ks"   -> "",
        "input_tb"   -> "",
        "output_ks"  -> "",
        "output_tb"  -> "",
        "op_type"    -> "",
        "prg_op"     -> "",
        "input_ks2"  -> "",
        "input_tb2"  -> "",
        "key_table"  -> "",
        "key_table2" -> ""
      )
  }
}
