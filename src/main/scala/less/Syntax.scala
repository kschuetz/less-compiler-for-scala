package com.kschuetz.less

object syntax {

  sealed abstract trait ComponentValue

  sealed abstract trait Expr extends ComponentValue

  case class BareIdentifier(name: String) extends ComponentValue

  sealed abstract trait VarRef extends Expr {
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
                           chunks: Seq[StringValue]) extends ComponentValue


  sealed abstract trait Directive
  case class ImportDirective(value: StringLiteral)

  sealed abstract trait UrlExpression extends ComponentValue
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
  sealed abstract trait TypedNumericValue extends Expr

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



      /*
  object Expr {
    def apply(item: Any): Expr =
      item match {
        case n: TypedNumericValue => n
        case r: VarRef => r
      }
  }
       */

  case class Add(x: Expr, y: Expr) extends Expr
  case class Subtract(x: Expr, y: Expr) extends Expr
  case class Multiply(x: Expr, y: Expr) extends Expr
  case class Divide(x: Expr, y: Expr) extends Expr


  object ComponentValue {
    //def apply(item: Any): ComponentValue
  }


  case class ValueVector(values: List[ComponentValue])

  object ValueVector {
    val empty = ValueVector(Nil)
  }


  case class Argument(name: Option[String],
                      value: Option[ValueVector])

  case class FunctionApplication(name: String,
                                 arguments: Seq[Argument]) extends Expr

  case class MixinApplication(name: String,
                              arguments: Seq[Argument])

  case class VarDeclaration(name: String, value: List[ValueVector])


  sealed abstract trait Priority
  case object Important extends Priority


  /**
   * An identifier that is potentially built using interpolated variables
   * @param segments
   */
  case class CompositeIdentifier(segments: Seq[StringValue])

  /*
  case class SimpleSelectorIdent(name: String) extends SelectorIdent
  case class InterpolatedSelectorIdent(chunks: Seq[StringValue]) extends SelectorIdent
  */

  sealed abstract trait NamespaceComponent
  case object DefaultNamespace extends NamespaceComponent   // no namespace component supplied
  case object NoNamespace extends NamespaceComponent        // explicitly empty namespace
  case object AnyNamespace extends NamespaceComponent
  case class Namespace(name: CompositeIdentifier) extends NamespaceComponent



  sealed abstract trait SelectorExpr
  case class SelectorExprIdent(name: String) extends SelectorExpr
  case class SelectorExprString(value: StringLiteral) extends SelectorExpr
  case class SelectorExprNumeric1(a: Expr) extends SelectorExpr
  case class SelectorExprNumeric2(a: Expr, b: Expr ) extends SelectorExpr  // an+b


  sealed abstract trait AttributeValue
  case class AttributeStringValue(value: StringLiteral) extends AttributeValue
  case class AttributeIdentValue(name: String) extends AttributeValue


  sealed abstract trait Selector
  sealed abstract trait SimpleSelector extends Selector
  case class UniversalSelector(namespace: NamespaceComponent) extends SimpleSelector
  case class TypeSelector(element: CompositeIdentifier, namespace: NamespaceComponent) extends SimpleSelector
  case class ClassSelector(name: CompositeIdentifier) extends SimpleSelector
  case class IDSelector(name: CompositeIdentifier) extends SimpleSelector
  case class PseudoElementSelector(name: CompositeIdentifier) extends SimpleSelector
  case class PseudoClassSelector(name: CompositeIdentifier) extends SimpleSelector
  case class FunctionalPseudoSelector(name: CompositeIdentifier, expr: SelectorExpr) extends SimpleSelector

  sealed abstract trait AttributeMatchOp
  case object AttributeEquals extends AttributeMatchOp
  case object AttributeIncludes extends AttributeMatchOp
  case object AttributePrefixMatch extends AttributeMatchOp
  case object AttributeSuffixMatch extends AttributeMatchOp
  case object AttributeDashMatch extends AttributeMatchOp
  case object AttributeSubstringMatch extends AttributeMatchOp

  sealed abstract trait AttributeSelector extends SimpleSelector
  case class HasAttribute(attributeName: CompositeIdentifier, namespace: NamespaceComponent) extends AttributeSelector
  case class AttributeMatch(matchOp: AttributeMatchOp, attributeName: CompositeIdentifier, namespace: NamespaceComponent, value: AttributeValue) extends AttributeSelector

  case class NegationSelector(argument: SimpleSelector) extends SimpleSelector

  case class SimpleSelectorSeq(items: Seq[SimpleSelector]) extends Selector

  sealed abstract trait SelectorCombinator extends Selector

  case class DescendantCombinator(first: SimpleSelectorSeq, second: SimpleSelectorSeq) extends SelectorCombinator
  case class ChildCombinator(first: SimpleSelectorSeq, second: SimpleSelectorSeq) extends SelectorCombinator
  case class AdjacentSiblingCombinator(first: SimpleSelectorSeq, second: SimpleSelectorSeq) extends SelectorCombinator
  case class GeneralSiblingCombinator(first: SimpleSelectorSeq, second: SimpleSelectorSeq) extends SelectorCombinator

}
