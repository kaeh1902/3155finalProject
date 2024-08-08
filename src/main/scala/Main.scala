//**TODO: Your code here**
// Define the abstract syntax
sealed trait Expr
sealed trait Value extends Expr

case class ManyExprs(myExprs: List[Expr]) extends Expr
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Not(e: Expr) extends Expr
case class Count(e1: Expr, e2: Expr) extends Expr

case object Cry extends Value
case object Happy extends Value
case object VeryHappy extends Value
case object Sleepy extends Value
case object Stun extends Value
case class ManyVals(myValues: List[Value]) extends Value
case object ErrorValue extends Value

// Implement the interpreter
object Interpreter {

  def eval(expr: Expr): Value = expr match {
    case v: Value => v
    case Plus(e1, e2) => plus(e1, e2)
    case Not(e) => not(e)
    case ManyExprs(exprs) => manyExprs(exprs)
    case Count(e1, e2) => count(e1, e2)
    case _ => ErrorValue
  }

  def manyExprs(exprs: List[Expr]): Value = {
    val evaluatedValues = exprs.map(eval)
    if (evaluatedValues.contains(ErrorValue)) ErrorValue
    else ManyVals(evaluatedValues)
  }

  // Implement the Plus operation with proper handling
  def plus(e1: Expr, e2: Expr): Value = {
    // Evaluate e1 and e2 to values
    val v1 = eval(e1)
    val v2 = eval(e2)

    // Plus operation logic
    (v1, v2) match {
      // ManyVals with ManyVals: Apply Plus element-wise
      case (ManyVals(vals1), ManyVals(vals2)) =>
        if (vals1.length != vals2.length) ErrorValue
        else {
          val combined = vals1.zip(vals2).map { case (v1, v2) => plus(v1, v2) }
          if (combined.contains(ErrorValue)) ErrorValue else ManyVals(combined)
        }

      // ManyVals with a single value: Apply Plus to each element
      case (ManyVals(vals), v) =>
        val results = vals.map(vi => plus(vi, v))
        if (results.contains(ErrorValue)) ErrorValue else ManyVals(results)

      // A single value with ManyVals: Apply Plus to each element
      case (v, ManyVals(vals)) =>
        val results = vals.map(vi => plus(v, vi))
        if (results.contains(ErrorValue)) ErrorValue else ManyVals(results)

      case (VeryHappy, _) => VeryHappy // VeryHappy dominates any other value
      case (_, VeryHappy) => VeryHappy // Any interaction with VeryHappy results in VeryHappy
      case (Cry, v) => v // Other value takes over Cry
      case (v, Cry) if v == Happy || v == Stun => Cry // Cry takes over Happy or Stun
      case (v1, v2) if v2 != VeryHappy || v2 != Cry => v1
      case _ => ErrorValue // Unsupported combinations return ErrorValue
    }
  }
  //Ex: has error

  // Implement the Not operation
  def not(e: Expr): Value = {
    // Evaluate the expression to a value
    val v = eval(e)

    v match {
      case Sleepy => Stun
      case Stun => Sleepy
      case Happy | VeryHappy => Cry
      case Cry => VeryHappy
      case ManyVals(vals) if vals.length >= 2 =>
        // If we have a list of values, we need to reduce it using the Plus logic
        // The reduction process applies Plus from left to right across the list
        vals.tail.foldLeft(vals.head) { (acc, vi) =>
          // Apply Plus to the accumulated value and the current value
          plus(acc, vi) match {
            case VeryHappy => return Sleepy // If any result is VerHappy, return VeryHappy immediately as it dominates
            case result => eval(Not(result)) // Otherwise, continue the reduction
          }
        }
      case _ => ErrorValue // If the value is not recognized, return ErrorValue as a default case
    }
  }

  // Implement the Count operation
  // def countCorrect(e1: Expr, e2: Expr): Int = {
  //   val v1 = eval(e1)
  //   val v2 = eval(e2)

  //   (v1, v2) match {
  //     case (ManyVals(vals1), ManyVals(vals2)) => 
  //       val v1Arr = vals1.filterNot(vals2.contains)
  //       v1Arr.length
  //     case _ =>
  //       0
  //     }
  //   }

  def count(e1: Expr, e2: Expr): Value = {
    // Evaluate the first expression to a value
    val v1 = eval(e1)

    // Evaluate the second expression to a value
    val v2 = eval(e2)

    // Apply the Count operation based on the first value
    v1 match {
      case ManyVals(vals) if v2 != ManyVals(vals) =>
        // If the first value is ManyVals (a list of values),
        // replace each element in the list with the second value (v2)
        ManyVals(vals.map(_ => v2))
      case _ =>
        // If the first value is not ManyVals, return an ErrorValue
        // indicating that the Count operation is not applicable
        ErrorValue
    }
  }
}