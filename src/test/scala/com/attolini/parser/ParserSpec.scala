package com.attolini.parser
import com.attolini.lexer.Lexer
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserSpec extends AnyWordSpec with Matchers {

  val input: String =
    "INSERT INTO config_verticali_nfc.create_table (keyspace_name,table_name,primary_key,clustering_order,all_fields) " +
      "VALUES " +
      "('verticali_nfc','list_entita', ['end_sin_sinistro_id','end_por_portafoglio_id','end_en_dan_id'],['end_en_dan_id ASC'],['end_sin_sinistro_id bigint','end_por_portafoglio_id bigint','end_en_dan_id bigint','end_liq_liquidatore_id bigint']);"

  "query input should be correctly parsed" in {
    val result = Lexer(input)
    assert(result.isRight)
  }
}
