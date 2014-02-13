package com.kschuetz.less

object syntax {



  sealed abstract trait VarRef {
    def varName: String
  }



  sealed abstract trait StringValue
  case class StringConstant(value: String) extends StringValue
  case class StringVarRef(varName: String) extends StringValue with VarRef


  sealed abstract trait QuoteDelimiter
  case object DoubleQuoteDelimiter extends QuoteDelimiter
  case object SingleQuoteDelimiter extends QuoteDelimiter
  case object TildeQuoteDelimiter extends QuoteDelimiter

  case class StringLiteral(delimiter: QuoteDelimiter,
                           chunks: Seq[StringValue])


  sealed abstract trait Directive
  case class ImportDirective(value: StringLiteral)


}
