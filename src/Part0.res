let find_index = (cenv, x) => {
  let rec _find_index = (cenv, n) => {
    switch cenv {
    | list{} => raise(Not_found)
    | list{a, ...rest} =>
      if a == x {
        n
      } else {
        _find_index(rest, n + 1)
      }
    }
  }
  _find_index(cenv, 0)
}

// Tiny language 0
module Language0 = {
  module Expr = {
    type rec expr =
      | Cst(int) // i
      | Add(expr, expr) // a + b
      | Mul(expr, expr) // a * b

    // Interpreter
    let rec eval = expr => {
      switch expr {
      | Cst(i) => i
      | Add(a, b) => eval(a) + eval(b)
      | Mul(a, b) => eval(a) * eval(b)
      }
    }
  }

  // Machine Instructions
  module Instr = {
    type instr = Cst(int) | Add | Mul

    // Interpreter
    let rec eval = (instrs, stk) => {
      switch (instrs, stk) {
      | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stk})
      | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
      | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
      | (list{}, list{a, ..._stk}) => a
      | _ => assert false
      }
    }
  }

  // Compile expr0 to machine instructions
  module Expr2Instr = {
    let rec compile = (expr: Expr.expr): list<Instr.instr> => {
      switch expr {
      | Cst(i) => list{Cst(i)}
      | Add(a, b) => Belt.List.concatMany([compile(a), compile(b), list{Add}])
      | Mul(a, b) => Belt.List.concatMany([compile(a), compile(b), list{Mul}])
      }
    }
  }
}

// Tiny language 1
module Language1 = {
  // expr with variable names
  module Expr = {
    type rec expr =
      | Cst(int)
      | Add(expr, expr)
      | Mul(expr, expr)
      | Var(string)
      | Let(string, expr, expr)

    // Interpreter with an environment
    type env = list<(string, int)>
    let rec eval = (expr, env) => {
      switch expr {
      | Cst(i) => i
      | Add(a, b) => eval(a, env) + eval(b, env)
      | Mul(a, b) => eval(a, env) * eval(b, env)
      | Var(x) => List.assoc(x, env)
      | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
      }
    }
  }

  // expr with indices
  module NamelessExpr = {
    type rec expr =
      | Cst(int)
      | Add(expr, expr)
      | Mul(expr, expr)
      | Var(int)
      | Let(expr, expr)

    // Interpreter with a stack
    type s = list<int>
    let rec eval = (expr: expr, s) => {
      switch expr {
      | Cst(i) => i
      | Add(a, b) => eval(a, s) + eval(b, s)
      | Mul(a, b) => eval(a, s) * eval(b, s)
      | Var(n) => List.nth(s, n)
      | Let(e1, e2) => eval(e2, list{eval(e1, s), ...s})
      }
    }
  }

  module Instr = {
    // Machine Instructions with variables
    type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap

    // Interpreter
    let rec eval = (instrs, stk) => {
      switch (instrs, stk) {
      | (list{Cst(i), ...rest}, stk) => eval(rest, list{i, ...stk})
      | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
      | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
      | (list{Var(i), ...rest}, stk) => eval(rest, list{stk->Belt.List.getExn(i), ...stk})
      | (list{Pop, ...rest}, list{_, ...stk}) => eval(rest, stk)
      | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
      | (list{}, list{a, ..._stk}) => a
      | _ => assert false
      }
    }
  }

  module Expr2Instr = {
    type sv = Slocal(string) | Stmp
    type senv = list<sv>

    let sindex = (senv, s) => {
      let rec _compile = (senv, acc) => {
        switch senv {
        | list{} => raise(Not_found)
        | list{Slocal(x), ...rest} =>
          if x == s {
            acc
          } else {
            _compile(rest, acc + 1)
          }
        | list{Stmp, ...rest} => _compile(rest, acc + 1)
        }
      }
      _compile(senv, 0)
    }

    let compile = expr => {
      let rec _compile = (expr: Expr.expr, senv: senv): list<Instr.instr> => {
        switch expr {
        | Cst(i) => list{Cst(i)}
        | Var(s) => list{Var(sindex(senv, s))}
        | Add(e1, e2) =>
          Belt.List.concatMany([_compile(e1, senv), _compile(e2, list{Stmp, ...senv}), list{Add}])
        | Mul(e1, e2) =>
          Belt.List.concatMany([_compile(e1, senv), _compile(e2, list{Stmp, ...senv}), list{Mul}])
        | Let(x, e1, e2) =>
          Belt.List.concatMany([
            _compile(e1, senv),
            _compile(e2, list{Slocal(x), ...senv}),
            list{Swap, Pop},
          ])
        }
      }
      _compile(expr, list{})
    }
  }

  module Expr2NamelessExpr = {
    type cenv = list<string>

    let compile = expr => {
      let rec _compile = (expr: Expr.expr, cenv): NamelessExpr.expr => {
        switch expr {
        | Cst(i) => Cst(i)
        | Add(a, b) => Add(_compile(a, cenv), _compile(b, cenv))
        | Mul(a, b) => Mul(_compile(a, cenv), _compile(b, cenv))
        | Var(x) => Var(cenv->find_index(x))
        | Let(x, e1, e2) => Let(_compile(e1, cenv), _compile(e2, list{x, ...cenv}))
        }
      }
      _compile(expr, list{})
    }
  }

  module NamelessExpr2Instr = {
    type sv = Slocal | Stmp
    type senv = list<sv>

    let sindex = (senv, i) => {
      let rec _compile = (senv, i, acc) => {
        switch senv {
        | list{} => raise(Not_found)
        | list{Slocal, ...rest} =>
          if i == 0 {
            acc
          } else {
            _compile(rest, i - 1, acc + 1)
          }
        | list{Stmp, ...rest} => _compile(rest, i, acc + 1)
        }
      }
      _compile(senv, i, 0)
    }

    let compile = expr => {
      let rec _compile = (expr: NamelessExpr.expr, senv: senv): list<Instr.instr> => {
        switch expr {
        | Cst(i) => list{Cst(i)}
        | Var(s) => list{Var(sindex(senv, s))}
        | Add(e1, e2) =>
          Belt.List.concatMany([_compile(e1, senv), _compile(e2, list{Stmp, ...senv}), list{Add}])
        | Mul(e1, e2) =>
          Belt.List.concatMany([_compile(e1, senv), _compile(e2, list{Stmp, ...senv}), list{Mul}])
        | Let(e1, e2) =>
          Belt.List.concatMany([
            _compile(e1, senv),
            _compile(e2, list{Slocal, ...senv}),
            list{Swap, Pop},
          ])
        }
      }
      _compile(expr, list{})
    }
  }
}
