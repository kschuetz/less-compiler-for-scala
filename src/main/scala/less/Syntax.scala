package com.kschuetz.less

object syntax {



  sealed abstract trait VarRef {
    def varName: String
  }



  sealed abstract trait StringValue
  case class StringConstant(value: String) extends StringValue
  
  
  
  case class DirectVarRef(varName: String) extends VarRef with StringValue
  case class IndirectVarRef(ref: VarRef) extends VarRef {
    def varName = ref.varName
  }

  sealed abstract trait QuoteDelimiter
  case object DoubleQuoteDelimiter extends QuoteDelimiter
  case object SingleQuoteDelimiter extends QuoteDelimiter
  case object TildeQuoteDelimiter extends QuoteDelimiter

  case class StringLiteral(delimiter: QuoteDelimiter,
                           chunks: Seq[StringValue])


  sealed abstract trait Directive
  case class ImportDirective(value: StringLiteral)

  sealed abstract trait UrlExpression
  case class UrlQuoted(value: StringLiteral) extends UrlExpression
  case class UrlUnquoted(value: String) extends UrlExpression

  sealed abstract trait DimensionUnit {
    def name: String
    def isKnown: Boolean = true
  }

  sealed abstract trait LengthUnit extends DimensionUnit

  sealed abstract trait RelativeLengthUnit extends LengthUnit

  case object EmUnit extends RelativeLengthUnit { val name = "em" }
  case object RemUnit extends RelativeLengthUnit { val name = "rem" }
  case object ExUnit extends RelativeLengthUnit { val name = "ex" }
  case object ChUnit extends RelativeLengthUnit { val name = "ch" }
  case object VwUnit extends RelativeLengthUnit { val name = "vw" }
  case object VhUnit extends RelativeLengthUnit { val name = "vh" }
  case object VminUnit extends RelativeLengthUnit { val name = "vmin" }
  case object VmaxUnit extends RelativeLengthUnit { val name = "vmax" }

  sealed abstract trait AbsoluteLengthUnit extends LengthUnit

  case object PxUnit extends AbsoluteLengthUnit { val name = "px" }
  case object CmUnit extends AbsoluteLengthUnit { val name = "cm" }
  case object MmUnit extends AbsoluteLengthUnit { val name = "mm" }
  case object InUnit extends AbsoluteLengthUnit { val name = "in" }
  case object PtUnit extends AbsoluteLengthUnit { val name = "pt" }
  case object PcUnit extends AbsoluteLengthUnit { val name = "pc" }

  sealed abstract trait AngleUnit extends DimensionUnit
  case object DegUnit extends AngleUnit { val name = "deg" }
  case object GradUnit extends AngleUnit { val name = "grad" }
  case object RadUnit extends AngleUnit { val name = "rad" }
  case object TurnUnit extends AngleUnit { val name = "turn" }

  sealed abstract trait TimeUnit extends DimensionUnit
  case object SUnit extends TimeUnit { val name = "s" }
  case object MsUnit extends TimeUnit { val name = "ms" }

  sealed abstract trait FreqUnit extends DimensionUnit
  case object HzUnit extends FreqUnit { val name = "Hz" }
  case object KHzUnit extends FreqUnit { val name = "kHz" }

  sealed abstract trait ResolutionUnit extends DimensionUnit
  case object DpiUnit extends ResolutionUnit { val name = "dpi" }
  case object DpcmUnit extends ResolutionUnit { val name = "dpcm" }
  case object DppxUnit extends ResolutionUnit { val name = "dppx" }

  case class UnknownUnit(name: String) extends DimensionUnit {
    override def isKnown: Boolean = false
  }

  object DimensionUnit {
    val knownUnits = Set(EmUnit, RemUnit, ExUnit, ChUnit, VwUnit, VhUnit, VminUnit, VmaxUnit,
                            PxUnit, CmUnit, MmUnit, InUnit, PtUnit, PcUnit,
                            DegUnit, GradUnit, RadUnit, TurnUnit,
                            SUnit, MsUnit,
                            HzUnit, KHzUnit,
                            DpiUnit, DpcmUnit, DppxUnit)

    val unitNameMap = {
      knownUnits.map(unit => (unit.name -> unit)) ++  Seq("hz" -> HzUnit, "khz" -> KHzUnit)
    }.toMap

    def byName(name: String): Option[DimensionUnit] = {
      if(name.nonEmpty) {
        val c = name.charAt(0)
        if(c.isDigit && c.isLower) {
          unitNameMap.get(name).orElse(Some(UnknownUnit(name)))
        } else {
          None
        }
      } else None
    }

  }


  sealed abstract trait NumericValue
  sealed abstract trait NumericConstant
  sealed abstract trait NumericLiteral
  sealed abstract trait TypedNumericValue

  case class WholeNumber(value: BigInt) extends NumericValue with NumericConstant with NumericLiteral with TypedNumericValue
  case class RealNumber(value: Double, sourceRepr: String) extends NumericValue with NumericConstant with NumericLiteral with TypedNumericValue

  case class Dimension(value: NumericConstant, units: DimensionUnit) extends NumericValue with TypedNumericValue
  case class Percentage(value: NumericConstant) extends NumericValue with TypedNumericValue

  object TypedNumericValue {
    def apply(nc: NumericConstant): TypedNumericValue =
      nc match {
        case n: WholeNumber => n
        case n: RealNumber => n
      }


  }

}
