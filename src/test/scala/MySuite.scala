import munit.FunSuite

class MySuite extends FunSuite {

  import Interpreter._

  // Test case for simple value evaluations
  test("eval-value: Cry evaluates to Cry") {
    val obtained = eval(Cry)
    val expected = Cry
    assertEquals(obtained, expected)
  }

  test("eval-value: Happy evaluates to Happy") {
    val obtained = eval(Happy)
    val expected = Happy
    assertEquals(obtained, expected)
  }

  test("eval-value: VeryHappy evaluates to VeryHappy") {
    val obtained = eval(VeryHappy)
    val expected = VeryHappy
    assertEquals(obtained, expected)
  }

  test("eval-value: Sleepy evaluates to Sleepy") {
    val obtained = eval(Sleepy)
    val expected = Sleepy
    assertEquals(obtained, expected)
  }

  test("eval-value: Stun evaluates to Stun") {
    val obtained = eval(Stun)
    val expected = Stun
    assertEquals(obtained, expected)
  }

  // Test case for ManyExprs evaluation
  test("eval-many-exprs: List of Cry, Happy evaluates to ManyVals") {
    val expr = ManyExprs(List(Cry, Happy))
    val obtained = eval(expr)
    val expected = ManyVals(List(Cry, Happy))
    assertEquals(obtained, expected)
  }

  test("eval-many-exprs: List with ErrorValue results in Error") {
    val expr = ManyExprs(List(Cry, ErrorValue))
    val obtained = eval(expr)
    assertEquals(obtained, ErrorValue)
  }

  // Test case for Plus with VeryHappy
  test("eval-plus: VeryHappy plus Cry results in VeryHappy") {
    val expr = Plus(VeryHappy, Cry)
    val obtained = eval(expr)
    val expected = VeryHappy
    assertEquals(obtained, expected)
  }

  test("eval-plus: VeryHappy plus Happy results in VeryHappy") {
    val expr = Plus(VeryHappy, Happy)
    val obtained = eval(expr)
    val expected = VeryHappy
    assertEquals(obtained, expected)
  }

  // Test case for Plus with Cry
  test("eval-plus: Cry plus Happy results in Happy") {
    val expr = Plus(Cry, Happy)
    val obtained = eval(expr)
    val expected = Happy
    assertEquals(obtained, expected)
  }

  test("eval-plus: Happy plus Cry results in Cry") {
    val expr = Plus(Happy, Cry)
    val obtained = eval(expr)
    val expected = Cry
    assertEquals(obtained, expected)
  }

  // Test case for Plus with Sleepy
  test("eval-plus: Sleepy plus Happy results in Sleepy") {
    val expr = Plus(Sleepy, Happy)
    val obtained = eval(expr)
    val expected = Sleepy
    assertEquals(obtained, expected)
  }

  test("eval-plus: Happy plus Sleepy results in Happy") {
    val expr = Plus(Happy, Sleepy)
    val obtained = eval(expr)
    val expected = Happy
    assertEquals(obtained, expected)
  }

  // Test case for Plus with Stun
  test("eval-plus: Stun plus Cry results in Cry") {
    val expr = Plus(Stun, Cry)
    val obtained = eval(expr)
    val expected = Cry
    assertEquals(obtained, expected)
  }

  test("eval-plus: Stun plus Happy results in Stun") {
    val expr = Plus(Stun, Happy)
    val obtained = eval(expr)
    val expected = Stun
    assertEquals(obtained, expected)
  }

  // Test case for Plus with ManyVals and Single Value
  test("eval-plus: ManyVals with single value") {
    val expr = Plus(ManyVals(List(Happy, Cry, Stun)), VeryHappy) // This should apply VeryHappy to all the values in the list and retunr the modifuied lisyt.
    val obtained = eval(expr)
    val expected = ManyVals(List(VeryHappy, VeryHappy, VeryHappy))
    assertEquals(obtained, expected)
  }

  // Test case for Plus with Single Value and ManyVals
  test("eval-plus: Single value with ManyVals") {
    val expr = Plus(Cry, ManyVals(List(Happy, VeryHappy, Sleepy)))
    val obtained = eval(expr)
    val expected = ManyVals(List(Happy, VeryHappy, Sleepy))
    assertEquals(obtained, expected)
  }

  // Test case for ManyVals with ManyVals of equal length
  test("eval-plus: ManyVals with ManyVals of equal length") {
    val expr = Plus(
      ManyVals(List(Happy, Cry, VeryHappy)),
      ManyVals(List(Cry, Sleepy, Stun))
    )
    val obtained = eval(expr)
    val expected = ManyVals(List(Cry, Sleepy, VeryHappy))
    assertEquals(obtained, expected)
  }

  // Test case for ManyVals with ManyVals of unequal length
  test("eval-plus: ManyVals with ManyVals of unequal length") {
    val expr = Plus(
      ManyVals(List(Happy, Cry)),
      ManyVals(List(Cry, Sleepy, Stun))
    )
    val obtained = eval(expr)
    assertEquals(obtained, ErrorValue)
  }

  // Test case for Not operation
  test("eval-not: Not Happy results in Cry") {
    val expr = Not(Happy)
    val obtained = eval(expr)
    val expected = Cry
    assertEquals(obtained, expected)
  }

  test("eval-not: Not Cry results in VeryHappy") {
    val expr = Not(Cry)
    val obtained = eval(expr)
    val expected = VeryHappy
    assertEquals(obtained, expected)
  }

  test("eval-not: Not Sleepy results in Sleepy") {
    val expr = Not(Sleepy)
    val obtained = eval(expr)
    val expected = Stun
    assertEquals(obtained, expected)
  }

  test("eval-not: Not Stun results in Sleepy") {
    val expr = Not(Stun)
    val obtained = eval(expr)
    val expected = Sleepy
    assertEquals(obtained, expected)
  }

  test("eval-not: Not VeryHappy results in Cry") {
    val expr = Not(VeryHappy)
    val obtained = eval(expr)
    val expected = Cry
    assertEquals(obtained, expected)
  }


  // Test case for Not operation on ManyVals
  test("eval-not: Not ManyVals applies plus reduction resulting in Stun") {
    val expr = Not(ManyVals(List(Cry, Stun, Happy, Happy)))
    val obtained = eval(expr)
    val expected = Stun
    assertEquals(obtained, expected)
  }

  // Test case for Count operation
  test("eval-count: ManyVals with Cry results in all Cry") {
    val expr = Count(ManyVals(List(Happy, Stun, VeryHappy)), Cry)
    val obtained = eval(expr)
    val expected = ManyVals(List(Cry, Cry, Cry))
    assertEquals(obtained, expected)
  }

  test("eval-count: ManyVals with Happy results in all Happy") {
    val expr = Count(ManyVals(List(Sleepy, Stun, Cry)), Happy)
    val obtained = eval(expr)
    val expected = ManyVals(List(Happy, Happy, Happy))
    assertEquals(obtained, expected)
  }

  test("eval-count: Non-ManyVals results in Error") {
    val expr = Count(Happy, Cry)
    val obtained = eval(expr)
    assertEquals(obtained, ErrorValue)
  }

  // Additional test for empty ManyVals
  test("eval-count: Empty ManyVals results in empty ManyVals") {
    val expr = Count(ManyVals(List()), VeryHappy)
    val obtained = eval(expr)
    val expected = ManyVals(List())
    assertEquals(obtained, expected)
  }

  // Test for complex ManyVals with different value
  test("eval-count: ManyVals with complex value results in repeated value") {
    val expr = Count(ManyVals(List(Happy, VeryHappy)), Stun)
    val obtained = eval(expr)
    val expected = ManyVals(List(Stun, Stun))
    assertEquals(obtained, expected)
  }

  // Complex test case with nested ManyExprs and Plus
  test("Complex: Nested ManyExprs and Plus") {
    val expr = ManyExprs(List(
      Plus(Happy, Cry),          // Expected: Cry
      Plus(VeryHappy, Sleepy),   // Expected: VeryHappy
      Not(Plus(Sleepy, Happy)),  // Expected: Stun
      Plus(Sleepy, Cry),         // Expected: Sleepy
      Plus(Sleepy, VeryHappy)    // Expected: VeryHaappy
    ))
    val obtained = eval(expr)
    val expected = ManyVals(List(Cry, VeryHappy, Stun, Sleepy, VeryHappy))
    assertEquals(obtained, expected)
  }

  // Complex test case for ManyVals with reduced value from Not
  test("Complex: Interaction between ManyVals and reduced value from Not") {
    val left = ManyVals(List(Cry, VeryHappy, Stun, Sleepy, Happy))
    val right = Not(ManyVals(List(Cry, Stun, Sleepy, Happy))) // Reduced to Stun
    val expr = Plus(left, right)
    val obtained = eval(expr)
    val expected = ManyVals(List(Stun, VeryHappy, Stun, Sleepy, Happy))
    assertEquals(obtained, expected)
  }
}
