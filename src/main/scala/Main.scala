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
        case Not(e) => eval(e) match{
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
    def Plus(e1: Expr, e2: Expr) : Value = {
      // evaluate e1 and e2
      val v1 = eval(e1)
      val v2 = eval(e2)

      v1 match{
        /*
          evaluate every expression/value in v1 as plus(v1,v2)
          if v1.filter(error) != v1 then return error
        */
        case v1: ManyVals => { // implement many vals
          val new_list = v1.myValues.map(x => Plus(x,v2)) 
          val filtered_list = new_list.filter{
            case ErrorValue => false
            case _ => true
          } 
          if (filtered_list.length == new_list.length) then new_list
          else ErrorValue
        }
        case v1: VeryHappy => v1 // eval stay uwu
        case Cry => v2 // eval move on
        case Happy => {
          v2 match{
            case VeryHappy => v2 // eval become uwu
            case Cry => v2 // eval hard day
            case _ => v1 // eval meh
          }
        }
        case Stun =>  {
          v2 match{
            case VeryHappy => v2 // eval become uwu
            case Cry => v2 // eval hard day
            case _ => v1 // eval meh
          }
        }
        case ErrorValue => ErrorValue // error
        case _ => v1
      }
    }
  // implement the rest 
}








































































































































































































































































































































































































































































































