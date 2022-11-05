open Part0

module TestLanguage0 = {
  open Language0

  let testExpr = () => {
    open Expr
    list{
      (Mul(Add(Cst(1), Cst(1)), Cst(1)), 2),
      (Mul(Add(Cst(1), Cst(2)), Cst(3)), 9),
      (Mul(Add(Cst(1), Cst(2)), Cst(0)), 0),
    }->Belt.List.forEach(((expr, result)) => assert (eval(expr) == result))

    Js.log(`TestLanguage0:testExpr Passed`)
  }

  let testInstr = () => {
    open Instr
    list{
      (list{Cst(1), Cst(1), Add, Cst(1), Mul}, 2),
      (list{Cst(1), Cst(2), Add, Cst(3), Mul}, 9),
    }->Belt.List.forEach(((expr, result)) => assert (eval(expr, list{}) == result))

    Js.log(`TestLanguage0:testInstr Passed`)
  }

  let testCompile = () => {
    open Expr2Instr
    list{
      (Expr.Mul(Add(Cst(1), Cst(1)), Cst(3)), list{Instr.Cst(1), Cst(1), Add, Cst(3), Mul}),
      (Add(Mul(Cst(1), Cst(1)), Cst(3)), list{Cst(1), Cst(1), Mul, Cst(3), Add}),
    }->Belt.List.forEach(((expr, result)) => assert (compile(expr) == result))

    Js.log(`TestLanguage0:testCompile Passed`)
  }

  let testALL = () => {
    testExpr()
    testInstr()
    testCompile()
  }
}

module TestLanguage1 = {
  open Language1
  let testExpr = () => {
    open Expr
    list{
      (Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))), 9),
      (
        Let("y", Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))), Mul(Var("y"), Var("y"))),
        81,
      ),
    }->Belt.List.forEach(((expr, result)) => assert (eval(expr, list{}) == result))

    // Wrong case
    exception Unreached
    list{
      Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("y"))),
      Let("y", Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))), Mul(Var("y"), Var("x"))),
    }->Belt.List.forEach(expr =>
      try {
        eval(expr, list{})->ignore
        raise(Unreached)
      } catch {
      | Unreached => assert false
      | _ => 0
      }
    )

    Js.log(`TestLanguage1:testExpr Passed`)
  }

  let testInstr = () => {
    open Instr
    list{
      (list{Cst(1), Cst(2), Add, Var(0), Var(1), Mul, Swap, Pop}, 9),
      (list{Cst(1), Cst(2), Mul, Var(0), Var(1), Add, Swap, Pop}, 4),
    }->Belt.List.forEach(((expr, result)) => assert (eval(expr, list{}) == result))

    Js.log(`TestLanguage1:testInstr Passed`)
  }

  let testExpr2Instr = () => {
    open Expr2Instr

    list{
      (
        Expr.Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))),
        list{Instr.Cst(1), Cst(2), Add, Var(0), Var(1), Mul, Swap, Pop},
      ),
    }->Belt.List.forEach(((expr, result)) => assert (compile(expr) == result))

    Js.log(`TestLanguage1:testExpr2Instr Passed`)
  }

  let testNamelessExpr = () => {
    open NamelessExpr
    list{
      (Let(Add(Cst(1), Cst(2)), Mul(Var(0), Var(0))), 9),
      (Let(Mul(Cst(1), Cst(2)), Add(Var(0), Var(0))), 4),
    }->Belt.List.forEach(((expr, result)) => assert (eval(expr, list{}) == result))

    Js.log(`TestLanguage1:testNamelessExpr Passed`)
  }

  let testExpr2NamelessExpr = () => {
    open Expr2NamelessExpr

    list{
      (
        Expr.Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))),
        NamelessExpr.Let(Add(Cst(1), Cst(2)), Mul(Var(0), Var(0))),
      ),
      (
        Expr.Let("x", Cst(1), Let("y", Cst(2), Add(Var("x"), Var("y")))),
        NamelessExpr.Let(Cst(1), Let(Cst(2), Add(Var(1), Var(0)))),
      ),
      (
        Expr.Let(
          "x",
          Cst(1),
          Let("y", Cst(2), Let("z", Cst(3), Mul(Add(Var("x"), Var("y")), Var("z")))),
        ),
        NamelessExpr.Let(Cst(1), Let(Cst(2), Let(Cst(3), Mul(Add(Var(2), Var(1)), Var(0))))),
      ),
    }->Belt.List.forEach(((expr, result)) => assert (compile(expr) == result))

    Js.log(`TestLanguage1:testExpr2NamelessExpr Passed`)
  }

  let testNamelessExpr2Instr = () => {
    open NamelessExpr2Instr
    list{
      (
        NamelessExpr.Let(Add(Cst(1), Cst(2)), Mul(Var(0), Var(0))),
        list{Instr.Cst(1), Cst(2), Add, Var(0), Var(1), Mul, Swap, Pop},
      ),
      (
        NamelessExpr.Let(Cst(1), Mul(Cst(2), Add(Cst(3), Var(0)))),
        list{Instr.Cst(1), Cst(2), Cst(3), Var(2), Add, Mul, Swap, Pop},
      ),
      (
        NamelessExpr.Let(Cst(1), Let(Cst(2), Add(Var(1), Var(0)))),
        list{Instr.Cst(1), Cst(2), Var(1), Var(1), Add, Swap, Pop, Swap, Pop},
      ),
    }->Belt.List.forEach(((expr, result)) => assert (compile(expr) == result))

    Js.log(`TestLanguage1:testNamelessExpr2Instr Passed`)
  }

  let testALL = () => {
    testExpr()
    testInstr()
    testExpr2Instr()

    testNamelessExpr()
    testExpr2NamelessExpr()
    testNamelessExpr2Instr()
  }
}

let testALL = () => {
  TestLanguage0.testALL()
  TestLanguage1.testALL()
}

testALL()
