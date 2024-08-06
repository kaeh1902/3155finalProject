//**TODO: Your code here**
sealed trait Expr 
sealed trait Value extends Expr
case class ManyExprs(myExprs: List[Expr]) extends Expr 
case class Plus(e1: Expr, e2: Expr) extends Expr 
case class Not(e: Expr) extends Expr
case class Count(e1: Expr, e2: Expr) extends Expr

case object ErrorValue extends Value
case object Cry extends Value 
case object Happy extends Value
case object VeryHappy extends Value 
case object Sleepy extends Value
case object Stun extends Value 
case class ManyVals(myValues: List[Value]) extends Value

// Implement the interpreter
object Interpreter {
  def eval(expr: Expr): Value = expr match {
        case v: Value => v
        case Not(e): Value => e match {
            case Stun => Sleepy
            case Sleepy => Stun
            case Happy => Cry
            case VeryHappy => Cry
            case Cry => VeryHappy 
            case _ => ErrorValue
        }

        case ManyExprs(myExprs) => {
            val evalued = myExprs.map(eval)
            if (evalued.contains(ErrorValue)) ErrorValue
            else ManyVals(evalued)
        }
    }
  // implement the rest 
}








































































































































































































































































































































































































































































































