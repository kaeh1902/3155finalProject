class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  //**TODO: Your tests here
  test("Not tests") {
    val e1 = Stun
    val obtained = Interpreter.eval(Not(e1))
    val expected = Sleepy
    assertEquals(obtained, expected)
  }
}
