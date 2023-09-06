package com.attolini.compiler

import com.attolini.lexer.Lexer
import com.attolini.parser.{AST, Parser}

object Compiler {
  def apply(input: String): Either[CompilationError, AST] = {
    for {
      tokens <- Lexer(input)
      ast    <- Parser(tokens)
    } yield ast
  }
}
