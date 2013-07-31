describe "The evaluator module", ->
  evaluator = undefined
  beforeEach ->
    evaluator = new Evaluator()

  afterEach ->
    iframe = evaluator.scope.frameElement
    iframe.parentElement.removeChild iframe

  it "creates an iframe to evaluate code in", ->
    iframe = evaluator.scope.frameElement
    
    # Need to test against both the global iframe and the iframe's iframe as
    # a workaround for PhantomJS. Really, we just care that it's an iframe.
    isIFrame = iframe instanceof HTMLIFrameElement or
               iframe instanceof evaluator.scope.HTMLIFrameElement
    expect(isIFrame).toBe true
    expect(iframe.height).toEqual "0"
    expect(iframe.width).toEqual "0"
    expect(iframe.style.visibility).toEqual "hidden"

  it "creates a temporary variable array in the global scope", ->
    expect(evaluator.scope["$__temp__"]).toEqual []

  describe "when compiling", ->
    beforeEach ->
      @addMatchers
        toHaveSameAST: (expected) ->
          actual = @actual
          @message = =>
            "Expected #{@actual} to have the same AST as #{expected}"
          if typeof @actual == typeof expected == 'string'
            actual = [actual]
            expected = [expected]
          if Array.isArray(actual) and Array.isArray(expected)
            if actual.length == expected.length
              for act, i in actual
                exp = expected[i]
                actualAST = esprima.parse(act).body
                expectedAST = esprima.parse(exp).body
                unless @env.equals_ actualAST, expectedAST
                  return false
              # Went through all strings, all had the same AST
              return true
          return false


        toBeFunctionDef: (name, params, body, tempVar) ->
          message = null
          unless @env.equals_ @actual, jasmine.any(Evaluator.Function)
            message = "Expected #{@actual} to be an instance of " +
            "the evaluator's Function class"
          else unless @actual.name == name
            message = "Expected function to have name #{name}, " +
            "instead had name #{@actual.name}"
          else unless @env.equals_ @actual.params, params
            message = "Expected function to have params #{params}, " +
            "instead had params #{@actual.params}"
          else unless @actual.tempVar == tempVar
            message = "Expected function to be stored in variable #{tempVar}, " +
            "instead stored in #{@actual.tempVar}"
          else
            functionBytecode = getExpressionBytecode("(function () {#{body}})")
            bodyInstructions = functionBytecode.preInstructions[0].body
            unless @env.equals_ @actual.body, bodyInstructions
              message = "Expected function body to have same instructions as #{body}"

          @message = -> message
          message == null

        toBeFunctionCall: (callee, args, tempVar, isObjectCreation=false) ->
          # If the function call is on a dictionary field (aka d.func()) the
          # callee should be an array of two elements where the first element
          # is the dictionary that will be the 'this' object, and the second
          # argument is the field on that dictionary that contains the
          # function. If the function call is not on a dictionary (eg func())
          # the callee can be just the string of the object containing the
          # function or an array of [null, <function>] where <function> is the
          # string of the object containing the function.
          unless Array.isArray(callee) or isObjectCreation
            callee = [null, callee]

          targetClass = if isObjectCreation then Evaluator.NewObject else Evaluator.FunctionCall
          message = null
          unless @env.equals_ @actual, jasmine.any(targetClass)
            message = "Expected #{@actual} to be instance of #{targetClass}."
          else unless @env.equals_ @actual.callee, callee
            message = "Expected to be calling #{jasmine.pp(callee)}, " +
                      "actually calling #{jasmine.pp(@actual.callee)}."
          else unless @actual.tempVar == tempVar
            message = "Expected function call result to be stored in #{tempVar}, " +
                      "actually stored in #{@actual.tempVar}."
          else unless @actual.args.length == args.length
            message = "Calling function with #{@actual.args.length} arguments, " +
                      "expected to be calling with #{args.length} arguments."
          else
            expect(@actual.args).toHaveSameAST(args)
          @message = -> message
          message == null

    getExpressionBytecode = (expressionString) ->
      body = esprima.parse(expressionString).body
      expect(body.length).toEqual 1
      expect(body[0].type).toEqual "ExpressionStatement"
      Evaluator.compileExpression body[0].expression

    getStatementsBytecode = (statementsString) ->
      ast = esprima.parse(statementsString).body
      Evaluator.compileStatements ast

    tempName = (index) ->
      "$__temp__[#{index}]"

    describe "expression bytecode", ->
      it "converts non-function calls to a string", ->
        original = [
          "'my string'", "true", "null", "123.45", "1.0e2", "/regex/", # literals
          "$my_id_", # identifier
          "this", # this keyword
          "[1, true, 'hi', foo]", # array
          "({a: 1})", "({msg: 'hello', arr: [1,2,3], obj: {foo: 'bar'}})", # Object literals
          "1, 2, red, blue", # sequence
          "-a", "+5", "!hello", "~true", "! 'hey'", # character unary operators
          "typeof obj", "void 'hi'", "delete foo", # string unary operators
          "a in b", "'0' == false", # binary operators
          "foo = 12", "i += 4", # assignment expression
          "a++", "b--", "++c", "--d", # update operators
          "foo[bar]", "arr[0]", "obj['key']", # membership access
          "(1 + 2) * 3", "a + (b = 2)", # order of operations
        ]
        for expression in original
          # Expect each expression to compile to something with the same AST
          try
            bytecode = getExpressionBytecode(expression)
            expect(bytecode.preInstructions).toEqual []
            expect(bytecode.expression).toHaveSameAST expression
          catch e
            throw new Error("Failed to compile #{expression}. Error is: #{e.toString()}")

      it "can define functions", ->
        temp = tempName(0)
        # Function should be pulled out into a separate statement and the
        # expression itself should be the temp var containing the function.

        body = "var a = {hi: 'hello'}; a.b = 123; var b = 'foo'"
        original = "(function () {#{body}})"
        bytecode = getExpressionBytecode(original)
        expect(bytecode.preInstructions.length).toEqual 1
        expect(bytecode.preInstructions[0]).toBeFunctionDef null, [], body, temp
        expect(bytecode.expression).toEqual temp

        original = "(function foo(a, b) {a + b})"
        bytecode = getExpressionBytecode(original)
        expect(bytecode.preInstructions.length).toEqual 1
        expect(bytecode.preInstructions[0]).toBeFunctionDef "foo", ["a", "b"], "a + b", temp
        expect(bytecode.expression).toEqual temp

      it "can call functions", ->
        temp = tempName(0)

        bytecode = getExpressionBytecode("a.b.c()")
        expect(bytecode.preInstructions.length).toEqual 1
        expect(bytecode.preInstructions[0]).toBeFunctionCall ["a.b", "'c'"], [], temp
        expect(bytecode.expression).toEqual temp

        bytecode = getExpressionBytecode("arr[i]()")
        expect(bytecode.preInstructions.length).toEqual 1
        expect(bytecode.preInstructions[0]).toBeFunctionCall ["arr", "i"], [], temp
        expect(bytecode.expression).toEqual temp

        bytecode = getExpressionBytecode("func(foo, bar)")
        expect(bytecode.preInstructions.length).toEqual 1
        expect(bytecode.preInstructions[0]).toBeFunctionCall "func", ["foo", "bar"], temp
        expect(bytecode.expression).toEqual temp

      it "can create new objects", ->
        temp = tempName(0)
        bytecode = getExpressionBytecode("new foo(a, b, c)")
        expect(bytecode.preInstructions.length).toEqual 1
        expect(bytecode.preInstructions[0]).toBeFunctionCall "foo", ["a", "b", "c"], temp, true
        expect(bytecode.expression).toEqual temp

      it "handles multiple pre-instructions", ->
        t0 = tempName(0)
        t1 = tempName(1)
        bytecode = getExpressionBytecode("[foo(), bar()]")
        expect(bytecode.preInstructions.length).toEqual 2
        expect(bytecode.preInstructions[0]).toBeFunctionCall "foo", [], t0
        expect(bytecode.preInstructions[1]).toBeFunctionCall "bar", [], t1
        expect(bytecode.expression).toHaveSameAST "[#{t0}, #{t1}]"

        bytecode = getExpressionBytecode("obj.baz(a).garply(b).length")
        expect(bytecode.preInstructions.length).toEqual 2
        expect(bytecode.preInstructions[0]).toBeFunctionCall ["obj", "'baz'"], ["a"], t0
        expect(bytecode.preInstructions[1]).toBeFunctionCall [t0, "'garply'"], ["b"], t1
        expect(bytecode.expression).toEqual "#{t1}.length"

        bytecode = getExpressionBytecode("outer(inner())")
        expect(bytecode.preInstructions.length).toEqual 2
        expect(bytecode.preInstructions[0]).toBeFunctionCall "inner", [], t0
        expect(bytecode.preInstructions[1]).toBeFunctionCall "outer", [t0], t1
        expect(bytecode.expression).toEqual t1

      # Note that this relies on the If statement
      it "desugars short circuiting logical expressions", ->
        bytecode = getExpressionBytecode('a && b')
        # This should desugar to:
        #   temp = a
        #   if (temp) temp = b
        #   temp

        temp = tempName(0)
        expect(bytecode.preInstructions.length).toEqual 2
        expect(bytecode.preInstructions[0]).toHaveSameAST "#{temp} = a"
        ifStatement = bytecode.preInstructions[1]
        expect(ifStatement).toEqual jasmine.any(Evaluator.If)
        expect(ifStatement.condition).toHaveSameAST temp
        expect(ifStatement.thenCase).toHaveSameAST ["#{temp} = b"]
        expect(ifStatement.elseCase).toEqual []

        expect(bytecode.expression).toEqual temp

        bytecode = getExpressionBytecode('f() || g()')
        # This should desugar to:
        #   temp1 = f()
        #   temp0 = temp0
        #   if (!temp0)
        #     temp0 = g()
        #     temp0 = temp0
        #   temp0

        t0 = tempName(0)
        t1 = tempName(1)
        expect(bytecode.preInstructions.length).toEqual 3
        expect(bytecode.preInstructions[0]).toBeFunctionCall 'f', [], t1
        expect(bytecode.preInstructions[1]).toHaveSameAST "#{t0} = #{t1}"
        ifStatement = bytecode.preInstructions[2]
        expect(ifStatement).toEqual jasmine.any(Evaluator.If)
        expect(ifStatement.condition).toHaveSameAST "!#{t0}"
        expect(ifStatement.thenCase.length).toEqual 2
        expect(ifStatement.thenCase[0]).toBeFunctionCall 'g', [], t0
        expect(ifStatement.thenCase[1]).toHaveSameAST "#{t0} = #{t0}"
        expect(ifStatement.elseCase).toEqual []

        expect(bytecode.expression).toEqual t0

      it "desugars ternary expressions", ->
        bytecode = getExpressionBytecode('a() ? b() : c()')
        # This should desugar to:
        #   temp1 = a()
        #   if (temp1)
        #     temp0 = b()
        #     temp0 = temp0
        #   else
        #     temp0 = c()
        #     temp0 = temp0
        #   temp0

        t0 = tempName(0)
        t1 = tempName(1)
        expect(bytecode.preInstructions.length).toEqual 2
        expect(bytecode.preInstructions[0]).toBeFunctionCall 'a', [], t1
        ifStatement = bytecode.preInstructions[1]
        expect(ifStatement).toEqual jasmine.any(Evaluator.If)
        expect(ifStatement.condition).toHaveSameAST t1
        expect(ifStatement.thenCase.length).toEqual 2
        expect(ifStatement.thenCase[0]).toBeFunctionCall 'b', [], t0
        expect(ifStatement.thenCase[1]).toHaveSameAST "#{t0} = #{t0}"
        expect(ifStatement.elseCase.length).toEqual 2
        expect(ifStatement.elseCase[0]).toBeFunctionCall 'c', [], t0
        expect(ifStatement.elseCase[1]).toHaveSameAST "#{t0} = #{t0}"

        expect(bytecode.expression).toEqual t0

    describe "statement bytecode", ->
      expectStatementsEqual = (program, expectedStatements, expectedVariables, expectedFunctions) ->
        expectedVariables = expectedVariables or []
        expectedFunctions = expectedFunctions or []
        
        # Parse and compile code
        bytecode = getStatementsBytecode(program)
        compiled = bytecode.instructions
        
        # Have to sort because these are order insensitive. The elements should
        # just be strings anyway, which has well defined sorting order.
        expect(bytecode.declaredVariables.sort()).toEqual expectedVariables.sort()
        
        # Check that declared functions are the same
        expect(bytecode.declaredFunctions.length).toEqual expectedFunctions.length
        for actualFunc, i in bytecode.declaredFunctions
          # functionInformation should be the arguments to pass into expectFunctionDef
          exp = expect(actualFunc)
          exp.toBeFunctionDef.apply(exp, expectedFunctions[i])

        # Check that AST's match
        expect(compiled.length).toEqual expectedStatements.length
        for c, i in compiled
          expect(c).toHaveSameAST expectedStatements[i]

      it "ignores empty statements", ->
        expectStatementsEqual ";", []

      it "puts expressions in their own instruction", ->
        expectStatementsEqual(
          "a; 1 + 2; foo = 'hi'"
          ["a", "1 + 2", "foo = 'hi'"]
        )

      it "puts an expression's pre instructions before the instruction", ->
        t0 = tempName(0)
        t1 = tempName(1)
        original = "var a = 5; b = f(a, g());"
        bytecode = getStatementsBytecode(original)
        expect(bytecode.declaredVariables).toEqual ["a"]
        expect(bytecode.instructions.length).toEqual 4
        expect(bytecode.instructions[0]).toHaveSameAST "a = 5"
        expect(bytecode.instructions[1]).toBeFunctionCall "g", [], t0
        expect(bytecode.instructions[2]).toBeFunctionCall "f", ["a", t0], t1
        expect(bytecode.instructions[3]).toHaveSameAST "b = #{t1}"

      it "extracts declared variables", ->
        expectStatementsEqual(
          "a = 1; var b = 2, c; var a; b++"
          ["a = 1", "b = 2", "b++"]
          ["a", "b", "c"]
        )

      it "extracts declared functions", ->
        bytecode = getStatementsBytecode("
          obj = {msg: 'hi'};
          function foo(a, b) {
            a[b] = 'foo'
          }
          var bar = function bar() {};
        ")

        expect(bytecode.declaredVariables).toEqual ["bar"]
        declaredFuncs = bytecode.declaredFunctions
        expect(declaredFuncs.length).toEqual 1
        expect(declaredFuncs[0]).toBeFunctionDef "foo", ["a", "b"], "a[b] = 'foo'", null
        expect(bytecode.instructions.length).toEqual 3
        expect(bytecode.instructions[0]).toHaveSameAST "obj = {msg: 'hi'}"
        expect(bytecode.instructions[1]).toBeFunctionDef "bar", [], "", tempName(0)
        expect(bytecode.instructions[2]).toHaveSameAST "bar = #{tempName(0)}"

      it "can return values from functions", ->
        bytecode = getExpressionBytecode("(function () {return 5})")
        func = bytecode.preInstructions[0]
        returnInst = func.body.instructions[0]
        expect(returnInst).toEqual jasmine.any(Evaluator.Return)
        expect(returnInst.value).toEqual "5"

      it "flattens block statements", ->
        expectStatementsEqual(
          "a = 1; {b = 2; var b, c = 3; function foo() {}}; var c = 4;"
          ["a = 1", "b = 2", "c = 3", "c = 4"]
          ["b", "c"]
          [["foo", [], "", null]]
        )

      it "creates if statements with function scope", ->
        bytecode = getStatementsBytecode("
          var myval = 'hi';
          if (!isHi(myval)) {
            var a = 1;
          } else {
            var b = 2;
            function inside() {}
          }
          function isHi(val) {return val === 'hi'}
          var c = 'hello';
        ")
        expect(bytecode.declaredVariables).toEqual ["myval", "a", "b", "c"]
        declaredFuncs = bytecode.declaredFunctions
        expect(declaredFuncs.length).toEqual 2
        expect(declaredFuncs[0]).toBeFunctionDef "inside", [], "", null
        expect(declaredFuncs[1]).toBeFunctionDef "isHi", ["val"], "return val === 'hi'", null
        expect(bytecode.instructions.length).toEqual 4
        expect(bytecode.instructions[0]).toHaveSameAST "myval = 'hi'"
        expect(bytecode.instructions[1]).toBeFunctionCall "isHi", ["myval"], tempName(0)
        ifStatement = bytecode.instructions[2]
        expect(ifStatement).toEqual jasmine.any(Evaluator.If)
        expect(ifStatement.condition).toHaveSameAST "!#{tempName(0)}"
        expect(ifStatement.thenCase).toHaveSameAST ["a = 1"]
        expect(ifStatement.elseCase).toHaveSameAST ["b = 2"]
        expect(bytecode.instructions[3]).toHaveSameAST "c = 'hello'"

      it "handles if statements without else blocks", ->
        bytecode = getStatementsBytecode("
          if (predicate) {
            a = 1;
          }
        ")
        expect(bytecode.instructions.length).toEqual 1
        ifStatement = bytecode.instructions[0]
        expect(ifStatement.thenCase).toHaveSameAST ["a = 1"]
        expect(ifStatement.elseCase).toEqual []

      it "ignores labels", ->
        original = "
          foo:
          {
            a = 1;
            bar: a++
          }"
        expectStatementsEqual original, ["a = 1", "a++"]

      it "creates With blocks with function scope", ->
        bytecode = getStatementsBytecode("
          with (getEnv()) {
            var a = 1;
            function b () {}
          }
        ")
        expect(bytecode.declaredVariables).toEqual ["a"]
        expect(bytecode.declaredFunctions.length).toEqual 1
        expect(bytecode.declaredFunctions[0]).toBeFunctionDef "b", [], "", null
        expect(bytecode.instructions[0]).toBeFunctionCall "getEnv", [], tempName(0)
        expect(bytecode.instructions[1]).toEqual jasmine.any(Evaluator.With)
        expect(bytecode.instructions[1].body).toHaveSameAST ["a = 1"]

      describe "creating Switch blocks", ->
        it "desugars case evaluation and flattens all statements", ->
          bytecode = getStatementsBytecode("
            switch (val()) {
            case first:
              a = 2;
              b(a);
            case 'second':
              a = 5;
            case third():
              a++;
            }
          ")
          # This should desugar to:
          #   temp1 = val()
          #   if temp1 == first
          #     temp0 = 0 // instruction index at "case first:"
          #   else
          #     if temp1 == 'second'
          #       temp0 = 3 // instruction index at "case 'second':"
          #     else
          #       temp2 = third()
          #       if temp1 == temp2
          #         temp0 = 4 // instruction index at "case third():"
          #       else
          #         temp0 = null // No default
          #   switch (start at temp0)
          #     a = 2 // case first
          #     temp = b(a)
          #     temp
          #     a = 5 // case 'second'
          #     a++ // case third()
          t0 = tempName(0)
          t1 = tempName(1)
          t2 = tempName(2)
          expect(bytecode.instructions.length).toEqual 3
          expect(bytecode.instructions[0]).toBeFunctionCall 'val', [], t1
          firstLevel = bytecode.instructions[1]
          expect(firstLevel).toEqual jasmine.any(Evaluator.If)
          expect(firstLevel.condition).toHaveSameAST "#{t1} == first"
          expect(firstLevel.thenCase).toHaveSameAST ["#{t0} = 0"]
          expect(firstLevel.elseCase.length).toEqual 1

          secondLevel = firstLevel.elseCase[0]
          expect(secondLevel).toEqual jasmine.any(Evaluator.If)
          expect(secondLevel.condition).toHaveSameAST "#{t1} == 'second'"
          expect(secondLevel.thenCase).toHaveSameAST ["#{t0} = 3"]
          expect(secondLevel.elseCase.length).toEqual 2
          expect(secondLevel.elseCase[0]).toBeFunctionCall 'third', [], t2

          thirdLevel = secondLevel.elseCase[1]
          expect(thirdLevel).toEqual jasmine.any(Evaluator.If)
          expect(thirdLevel.condition).toHaveSameAST "#{t1} == #{t2}"
          expect(thirdLevel.thenCase).toHaveSameAST ["#{t0} = 4"]
          expect(thirdLevel.elseCase).toHaveSameAST ["#{t0} = null"]

          switchStatement = bytecode.instructions[2]
          expect(switchStatement).toEqual jasmine.any(Evaluator.Switch)
          expect(switchStatement.startPCVar).toEqual t0
          inst = switchStatement.instructions
          expect(inst[0]).toHaveSameAST "a = 2"
          expect(inst[1]).toBeFunctionCall "b", ["a"], t0
          expect(inst[2]).toHaveSameAST t0
          expect(inst[3]).toHaveSameAST "a = 5"
          expect(inst[4]).toHaveSameAST "a++"

        it "has function scope", ->
          bytecode = getStatementsBytecode("
            switch (foo().val) {
            case first:
              var a = 2;
            case 'second':
              var b = 5;
              function c(d) {e}
            }
          ")
          expect(bytecode.declaredVariables).toEqual ["a", "b"]
          expect(bytecode.declaredFunctions.length).toEqual 1
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef "c", ["d"], "e", null
          inst = bytecode.instructions
          expect(inst.length).toEqual 3
          expect(inst[0]).toBeFunctionCall "foo", [], tempName(1)
          expect(inst[1]).toEqual jasmine.any(Evaluator.If)
          expect(inst[2]).toEqual jasmine.any(Evaluator.Switch)
          expect(inst[2].startPCVar).toEqual tempName(0)

        it "stores a default index", ->
          bytecode = getStatementsBytecode("
            switch (val) {
            case 'foo':
              a = 2;
            default:
              a = 4;
            case 'bar':
              a = 6;
            }
          ")
          # This should desugar to:
          #   if val == 'foo'
          #     temp = 0 // instruction index of "case 'foo':"
          #   else
          #     if val == 'bar'
          #       temp = 2 // instruction index of "case 'bar':"
          #     else
          #       temp = 1 // instruction index of "default"
          #   switch (start at temp)....
          temp = tempName(0)
          expect(bytecode.instructions.length).toEqual 2
          firstLevel = bytecode.instructions[0]
          expect(firstLevel).toEqual jasmine.any(Evaluator.If)
          expect(firstLevel.condition).toHaveSameAST "val == 'foo'"
          expect(firstLevel.thenCase).toHaveSameAST ["#{temp} = 0"]
          expect(firstLevel.elseCase.length).toEqual 1

          secondLevel = firstLevel.elseCase[0]
          expect(secondLevel).toEqual jasmine.any(Evaluator.If)
          expect(secondLevel.condition).toHaveSameAST "val == 'bar'"
          expect(secondLevel.thenCase).toHaveSameAST ["#{temp} = 2"]
          expect(secondLevel.elseCase).toHaveSameAST ["#{temp} = 1"]

          switchStatement = bytecode.instructions[1]
          expect(switchStatement).toEqual jasmine.any(Evaluator.Switch)
          expect(switchStatement.startPCVar).toEqual temp

      it "creates break statements", ->
        bytecode = getStatementsBytecode("
          switch (val) {
          case 'foo':
            break;
          }
        ")
        switchStatement = bytecode.instructions[1]
        breakStatement = switchStatement.instructions[0]
        expect(breakStatement).toEqual jasmine.any(Evaluator.Break)

      it "does not allow break statements to have labels", ->
        original = "
          switch (val) {
          case 'foo':
            lbl:break lbl;
          }"
        expect(->
          getStatementsBytecode original
        ).toThrow "Does not support labelled breaks."

      
      # Note that the tests for while and the tests for continue rely on each other
      it "creates continue statements", ->
        bytecode = getStatementsBytecode("
          while (true) {
            continue;
          };
        ")
        whileStatement = bytecode.instructions[0]
        continueStatement = whileStatement.instructions[1]
        expect(continueStatement).toEqual jasmine.any(Evaluator.Continue)

      it "doesn't allow labelled continues", ->
        original = "
          lbl:
          while (true) {
            continue lbl;
          };"
        expect(->
          getStatementsBytecode original
        ).toThrow "Does not support labelled continues."

      describe "creating While loops", ->
        it "desugars it into a loop", ->
          bytecode = getStatementsBytecode("
            while (!shouldStop()) {
              a = 1 + 2;
              a++;
            }
          ")
          
          # This should desugar to:
          #  temp = shouldStop()
          #  if (!(!temp))
          #    break;
          #  a = 1 + 2
          #  a++
          #  continue;

          expect(bytecode.instructions.length).toEqual 1
          loopInstr = bytecode.instructions[0]
          expect(loopInstr).toEqual jasmine.any(Evaluator.Loop)
          expect(loopInstr.instructions.length).toEqual 5

          expect(loopInstr.instructions[0]).toBeFunctionCall "shouldStop", [], tempName(0)

          ifStatement = loopInstr.instructions[1]
          expect(ifStatement).toEqual jasmine.any(Evaluator.If)
          expect(ifStatement.condition).toHaveSameAST "!!#{tempName(0)}"
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(Evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

          expect(loopInstr.instructions[2]).toHaveSameAST "a = 1 + 2"
          expect(loopInstr.instructions[3]).toHaveSameAST "a++"
          expect(loopInstr.instructions[4]).toEqual jasmine.any(Evaluator.Continue)

        it "uses function scope", ->
          bytecode = getStatementsBytecode("
            while (true) {
              var a = 1 + 2;
              function b(c) {d};
            }
          ")
          expect(bytecode.declaredVariables).toEqual ["a"]
          expect(bytecode.declaredFunctions.length).toEqual 1
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef "b", ["c"], "d", null


      describe "creating Do While loops", ->
        it "unrolls one execution and desugars it into a loop", ->
          bytecode = getStatementsBytecode("
            do {
              a = 1 + 2;
              a++;
            } while (!shouldStop());
          ")
          
          # This should desugar to:
          #  temp = shouldStop()
          #  if (!(!temp))
          #    break;
          #  a = 1 + 2
          #  a++
          #  continue;

          expect(bytecode.instructions.length).toEqual 1
          loopInstr = bytecode.instructions[0]
          expect(loopInstr).toEqual jasmine.any(Evaluator.Loop)
          expect(loopInstr.instructions.length).toEqual 5
          expect(loopInstr.initialPC).toEqual 2 # Do not want to check the predicate the first time

          expect(loopInstr.instructions[0]).toBeFunctionCall "shouldStop", [], tempName(0)
          ifStatement = loopInstr.instructions[1]
          expect(ifStatement).toEqual jasmine.any(Evaluator.If)
          expect(ifStatement.condition).toHaveSameAST "!!#{tempName(0)}"
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(Evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

          expect(loopInstr.instructions[2]).toHaveSameAST "a = 1 + 2"
          expect(loopInstr.instructions[3]).toHaveSameAST "a++"
          expect(loopInstr.instructions[4]).toEqual jasmine.any(Evaluator.Continue)

        it "uses function scope", ->
          bytecode = getStatementsBytecode("
            do {
              var a = 1 + 2;
              function b(c) {d};
            } while (true);
          ")
          expect(bytecode.declaredVariables).toEqual ["a"]
          expect(bytecode.declaredFunctions.length).toEqual 1
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef "b", ["c"], "d", null

      describe "creating For loops", ->
        it "desugars it into a flat loop", ->
          bytecode = getStatementsBytecode("
            for (i = initVar(); i !== lst().length; i++) {
              a.push(i + 1);
            }
          ")
          
          # The initializer should be pulled out before the loop. The loop
          # should desugar to:
          #  temp = lst();
          #  if (!(i !== temp.length))
          #    break;
          #  temp = a.push(i + 1);
          #  temp;
          #  i++;
          #  continue;

          t0 = tempName(0)
          expect(bytecode.instructions.length).toEqual 3
          expect(bytecode.instructions[0]).toBeFunctionCall "initVar", [], t0
          expect(bytecode.instructions[1]).toHaveSameAST "i = #{t0}"
          loopInstr = bytecode.instructions[2]
          expect(loopInstr).toEqual jasmine.any(Evaluator.Loop)
          expect(loopInstr.instructions.length).toEqual 6

          expect(loopInstr.instructions[0]).toBeFunctionCall "lst", [], t0

          ifStatement = loopInstr.instructions[1]
          expect(ifStatement).toEqual jasmine.any(Evaluator.If)
          expect(ifStatement.condition).toHaveSameAST "!(i !== #{t0}.length)"
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(Evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

          expect(loopInstr.instructions[2]).toBeFunctionCall ["a", "'push'"], ["i + 1"], t0
          expect(loopInstr.instructions[3]).toHaveSameAST t0
          expect(loopInstr.instructions[4]).toHaveSameAST "i++"
          expect(loopInstr.instructions[5]).toEqual jasmine.any(Evaluator.Continue)

        it "uses function scope", ->
          bytecode = getStatementsBytecode("
            for (var i = init; i != 0; --i) {
              var a = 1 + 2;
              function b(c) {d};
            };
          ")
          expect(bytecode.declaredVariables).toEqual ["i", "a"]
          expect(bytecode.declaredFunctions.length).toEqual 1
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef "b", ["c"], "d", null

        it "handles empty expressions in loop definition", ->
          # No initializer
          bytecode = getStatementsBytecode("for (; b; c) {}")
          expect(bytecode.instructions.length).toEqual 1 # Loop
          forLoop = bytecode.instructions[0]
          expect(forLoop).toEqual jasmine.any(Evaluator.Loop)
          expect(forLoop.instructions.length).toEqual 3 # Finished test, update, continue
          expect(forLoop.instructions[0]).toEqual jasmine.any(Evaluator.If)
          expect(forLoop.instructions[0].condition).toHaveSameAST "!b"
          expect(forLoop.instructions[0].thenCase).toEqual [jasmine.any(Evaluator.Break)]
          expect(forLoop.instructions[0].elseCase).toEqual []
          expect(forLoop.instructions[1]).toEqual "c"
          expect(forLoop.instructions[2]).toEqual jasmine.any(Evaluator.Continue)
          
          # No finished test
          bytecode = getStatementsBytecode("for (a; ; c) {}")
          expect(bytecode.instructions.length).toEqual 2 # Initializer and loop
          expect(bytecode.instructions[0]).toEqual "a"
          forLoop = bytecode.instructions[1]
          expect(forLoop).toEqual jasmine.any(Evaluator.Loop)
          expect(forLoop.instructions.length).toEqual 2 # Update and continue
          expect(forLoop.instructions[0]).toEqual "c"
          expect(forLoop.instructions[1]).toEqual jasmine.any(Evaluator.Continue)
          
          # No update
          bytecode = getStatementsBytecode("for (a; b; ) {}")
          expect(bytecode.instructions.length).toEqual 2 # Initializer and loop
          expect(bytecode.instructions[0]).toEqual "a"
          forLoop = bytecode.instructions[1]
          expect(forLoop).toEqual jasmine.any(Evaluator.Loop)
          expect(forLoop.instructions.length).toEqual 2 # Finished test and continue
          expect(forLoop.instructions[0]).toEqual jasmine.any(Evaluator.If)
          expect(forLoop.instructions[0].condition).toHaveSameAST "!b"
          expect(forLoop.instructions[0].thenCase).toEqual [jasmine.any(Evaluator.Break)]
          expect(forLoop.instructions[0].elseCase).toEqual []
          expect(forLoop.instructions[1]).toEqual jasmine.any(Evaluator.Continue)

      describe "creating Try Catch Finally blocks", ->
        it "stores the instructions in the try, catch and finally block", ->
          bytecode = getStatementsBytecode("
            try {
              a = 1; b = a + 'hi';
            } catch (err) {
              c = 2; d;
            } finally {
              e + f * g
            }
          ")
          expect(bytecode.instructions.length).toEqual 1
          tryStatement = bytecode.instructions[0]
          expect(tryStatement).toEqual jasmine.any(Evaluator.Try)
          expect(tryStatement.tryBlock).toHaveSameAST ["a = 1", "b = a + 'hi'"]
          expect(tryStatement.catchBlock).toHaveSameAST ["c = 2", "d"]
          expect(tryStatement.catchVariable).toEqual "err"
          expect(tryStatement.finallyBlock).toHaveSameAST ["e + f * g"]

        it "uses function scope", ->
          bytecode = getStatementsBytecode("
            try {
              var a;
              function b() {}
            } catch (e) {
              var c;
              function d() {}
            } finally {
              var e;
              function f() {}
            }
          ")
          expect(bytecode.declaredVariables).toEqual ['a', 'c', 'e']
          expect(bytecode.declaredFunctions.length).toEqual 3
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef 'b', [], '', null
          expect(bytecode.declaredFunctions[1]).toBeFunctionDef 'd', [], '', null
          expect(bytecode.declaredFunctions[2]).toBeFunctionDef 'f', [], '', null

        it "handles empty catch blocks", ->
          bytecode = getStatementsBytecode("
            try {
              a; b;
            } finally {
              c; d;
            }
          ")
          expect(bytecode.instructions.length).toEqual 1
          tryStatement = bytecode.instructions[0]
          expect(tryStatement).toEqual jasmine.any(Evaluator.Try)
          expect(tryStatement.tryBlock).toHaveSameAST ["a", "b"]
          expect(tryStatement.catchBlock).toBeNull()
          expect(tryStatement.catchVariable).toBeNull()
          expect(tryStatement.finallyBlock).toHaveSameAST ["c", "d"]

        it "handles empty finally blocks", ->
          bytecode = getStatementsBytecode("
            try {
              a; b;
            } catch(e) {
              c; d;
            }
          ")
          expect(bytecode.instructions.length).toEqual 1
          tryStatement = bytecode.instructions[0]
          expect(tryStatement).toEqual jasmine.any(Evaluator.Try)
          expect(tryStatement.tryBlock).toHaveSameAST ["a", "b"]
          expect(tryStatement.catchBlock).toHaveSameAST ["c", "d"]
          expect(tryStatement.catchVariable).toEqual "e"
          expect(tryStatement.finallyBlock).toEqual []

      it "creates Throw statements", ->
        bytecode = getStatementsBytecode("throw 1 + 2")
        expect(bytecode.instructions.length).toEqual 1
        expect(bytecode.instructions[0]).toEqual jasmine.any(Evaluator.Throw)
        expect(bytecode.instructions[0].error).toHaveSameAST "1 + 2"

  describe "when interpreting bytecode", ->
    beforeEach ->
      @addMatchers
        toEvaluateTo: (expected, shouldBeError=false) ->
          evaluated = false
          didError = false
          result = null
          evaluator.eval @actual, (res, didErr) ->
            evaluated = true
            result = res
            didError = didErr
          @message = =>
            expectErrStr = if shouldBeError then "" else "out"
            actualErrStr = if didError then "" else "out"
            if expected instanceof Error or expected instanceof evaluator.scope.Error
              expFormatted = expected.toString()
            else
              expFormatted = jasmine.pp(expected)
            if result instanceof Error or result instanceof evaluator.scope.Error
              resFormatted = result.toString()
            else
              resFormatted = jasmine.pp(result)
            program = @actual.replace(/\s+/g, " ") # Collapse white spaces
            "Expected '#{program}' to evaluate to '#{expFormatted}' with#{expectErrStr} errors, " +
            "actually evaluated to '#{resFormatted}' with#{actualErrStr} errors."
          evaluated and @env.equals_(result, expected) and didError == shouldBeError

    it "uses its context to evaluate non-function expressions", ->
      num = 5
      testId =
        foo: "foo value"
        bar: "bar value"
        0: "zero value"
        9: "num value"

      evaluator.scope.num = num
      evaluator.scope.testId = testId
      
      # each line may rely on the previous line
      expressions = [
        ["'my string'", "my string"],
        ["true", true],
        ["null", null],
        ["123.45", 123.45],
        ["1.0e2", 1.0e2],
        ["/regex/", /regex/],
        ["testId", testId],
        ["[1, true, 'hi', testId]", [1, true, "hi", testId]],
        ["({a: 1})", a: 1 ],
        [
          "({msg: 'hello', arr: [1,2,3], obj: {foo: 'bar'}})",
          msg: "hello", arr: [1, 2, 3], obj: {foo: "bar"}
        ],
        ["1, 2, 'red', 'blue'", `(1, 2, "red", "blue")`],
        ["-num", -5], # num = 5
        ["+5", 5],
        ["!'hey'", false],
        ["typeof testId", "object"],
        ["void 'hi'", undefined],
        ["num in testId", false],
        ["'0' == false", true],
        ["foo = 12", 12],
        ["num += 4", 9], # num starts at 5 => num + 4 = 9
        ["foo++", 12],
        ["++foo", 14],
        ["true || false", true],
        ["true ? ' ' : ''", " "],
        ["testId.foo", "foo value"],
        ["testId['bar']", "bar value"],
        ["testId[0]", "zero value"],
        ["testId[num]", "num value"],
        ["(1 + 2) * 3", 9],
        ["false && true || true", true],
        ["false && (true || true)", false]
      ]
      for pair in expressions
        [inputString, outputValue] = pair
        expect(inputString).toEvaluateTo outputValue

    it "conditionally executes code in If blocks", ->
      program = "
        a = 0;
         if (val) {
           a += 1;
         } else {
           a += 2;
         }
         a;
      "
      evaluator.eval "val = true"
      expect(program).toEvaluateTo 1
      evaluator.eval "val = false"
      expect(program).toEvaluateTo 2

    describe "in loops", ->
      it "can break out", ->
        program = "
           a = 0;
           while (true) {
             break;
             a = 1;
           }
           a;"
        expect(program).toEvaluateTo 0

      it "can break out of nested blocks", ->
        program = "
           a = 0;
           while (true) {
             if (true) break;
             a = 1;
           }
           a;"
        expect(program).toEvaluateTo 0
        program = "
           a = 0;
           do {
             while (true) {
               if (true) break;
               a += 1;
             }
             a += 2
           } while (false)
           a;"
        expect(program).toEvaluateTo 2

      it "can continue", ->
        program = "
           a = 0;
           while (true) {
             if (a > 0) break;
             a++;
             continue;
             a = 5;
           }
           a;"
        expect(program).toEvaluateTo 1

      it "can continue in nested blocks", ->
        program = "
           a = 0;
           while (true) {
             if (a > 0) break;
             a++;
             if (true) continue;
             a = 5;
           }
           a;"
        expect(program).toEvaluateTo 1

      it "repeats normally", ->
        program = "
           a = 0;
           while (a != 3) {
             a++;
           }
           a;"
        expect(program).toEvaluateTo 3
        program = "
           arr = [1,2,3];
           for (i = 0; i < arr.length; i++) {
             arr[i] *= arr[i];
           }
           arr;"
        expect(program).toEvaluateTo [1, 4, 9]

    it "checks the predicate when continuing in Do While loops", ->
      program = "
         a = 0;
         do {
           a++;
           if (a > 1) break;
           continue;
         } while (false)
         a"
      expect(program).toEvaluateTo 1

    it "will call native functions", ->
      myNativeFunc = jasmine.createSpy().andReturn(12)
      evaluator.scope.myNativeFunc = myNativeFunc
      expect("myNativeFunc('hi', false)").toEvaluateTo 12
      expect(myNativeFunc).toHaveBeenCalledWith "hi", false

    it "can define and call user functions", ->
      program = "
         o = {foo: 1};
         f = function () {
           o.foo += 1;
         };
         f();
         o;"
      expect(program).toEvaluateTo foo: 2

    it "puts a prototype object on user functions", ->
      program = "
        f = function () {};
        p = f.prototype;
        [p, Object.getPrototypeOf(p) === Object.prototype]; "
      expect(program).toEvaluateTo [{}, true]

    it "prepopulates prototypes on user functions with a construction field", ->
      program = "
        f = function () {};
        p = f.prototype;
        [Object.getOwnPropertyNames(p), p.constructor === f]; "
      expect(program).toEvaluateTo [["constructor"], true]

    it "uses function scope when calling functions", ->
      program = "
         a = 1;
         b = 2;
         c = 3;
         f = function (a) {
           a += 1;
           var b = 6;
           c = a + b;
         };
         f(4);
         [a, b, c];"
      evaluator.eval "val = 3"
      expect(program).toEvaluateTo [1, 2, 11]

    it "can return values", ->
      program = "
         increment = function (val) {
           return val + 1;
         };
         increment(5);"
      expect(program).toEvaluateTo 6

    it "handles nested functions and has closures", ->
      program = "
         counter = function () {
           var count = 0;
           return function () {
             return count++;
           };
         };
         firstCounter = counter();
         firstRes = [];
         firstRes.push(firstCounter());
         firstRes.push(firstCounter());
         secondCounter = counter();
         secondRes = [];
         secondRes.push(secondCounter());
         firstRes.push(firstCounter());
         secondRes.push(secondCounter());
         secondRes.push(secondCounter());
         firstRes.push(firstCounter());
         firstRes.push(firstCounter());
         [firstRes, secondRes]"
      expect(program).toEvaluateTo [[0, 1, 2, 3, 4], [0, 1, 2]]

    it "uses enclosing scopes for control blocks", ->
      program = "
         f = function (bool, secondBool) {
           var a;
           if (bool) {
             a = 'first';
           } else if (secondBool) {
             a = 'second';
           } else {
             a = 'third';
           }
           return a;
         };
         [f(true, true), f(true, false), f(false, true), f(false, false)];"
      expect(program).toEvaluateTo ['first', 'first', 'second', 'third']

    it "defines declared functions at the beginning of the scope", ->
      program = "
         function f() {return 1;}
         a = f;
         function f() {return 2;}
         [a === f, a(), f()];"
      expect(program).toEvaluateTo [true, 2, 2]

    it "defines declared functions locally within other functions", ->
      program = "
         a = 0;
         function f() {
           return a();
           function a() {return 1;}
         }
         [a, f()];"
      expect(program).toEvaluateTo [0, 1]

    describe "a switch statement", ->
      it "jumps to the correct case", ->
        program = "
           a = 0;
           switch (val) {
           case 0:
             a += 1;
             break;
           case 1:
             a += 2;
           case 2:
             a += 3;
             break;
           }
           a;"
        evaluator.eval "val = 0"
        expect(program).toEvaluateTo 1
        evaluator.eval "val = 1"
        expect(program).toEvaluateTo 5
        evaluator.eval "val = 2"
        expect(program).toEvaluateTo 3
        evaluator.eval "val = 3" # Does not match any case
        expect(program).toEvaluateTo 0

      it "will use the default case if no other cases match", ->
        program = "
          a = 0;
          switch (val) {
          case 0:
            a += 1;
          default:
            a += 2;
          case 1:
            a += 3;
          }
          a;"
        evaluator.eval "val = 0"
        expect(program).toEvaluateTo 6
        evaluator.eval "val = 1"
        expect(program).toEvaluateTo 3
        evaluator.eval "val = 2" # Default case
        expect(program).toEvaluateTo 5

      it "handles only a default case", ->
        program = "
          a = 0;
          switch (0) {
           default:
             a++;
          }
          a;"
        expect(program).toEvaluateTo 1

      it "can continue and return", ->
        program = "
          f = function () {
            var a = 0;
            while (true) {
              a++;
              switch (a) {
                case 1:
                  continue;
                case 2:
                  return a;
              }
            }
          };
          f();"
        expect(program).toEvaluateTo 2

    describe "short circuits", ->
      it "case expression evalution", ->
        program = "
          obj = {a: 0, b: 0, c: 0};
          incr = function (val) {obj[val]++; return val;};
          switch('b') {
            case incr('a'):
            case incr('b'):
            case incr('c'):
          }
          obj"
        expect(program).toEvaluateTo a: 1, b: 1, c: 0

      it "logical expression", ->
        program = "
          a = 0;
          increment = function (val) {a++; return val};
          res = increment('') && increment('hi');
          [a, res];"
        expect(program).toEvaluateTo [1, '']

        program = "
          a = 0;
          increment = function (val) {a++; return val};
          res = increment('') && increment('hi') || increment('hello');
          [a, res];"
        expect(program).toEvaluateTo [2, "hello"]

      it "ternary expressions", ->
        program = "
          obj = {a: 0, b: 0, c: 0};
          incr = function (val) {obj[val]++; return val;};
          res = incr('a') ? incr('b') : incr('c');
          [obj, res];"
        expect(program).toEvaluateTo [{a: 1, b: 1, c: 0}, 'b']

    it "can delete variables in a scope", ->
      program = "
        a = 0;
        f = function () {
          var arr = [];
          var a = 4;
          arr.push(a);
          delete a;
          arr.push(a);
          arr.push('a' in window);
          delete a;
          arr.push('a' in window);
          return arr;
        };
        f();"
      expect(program).toEvaluateTo [4, 0, true, false]

    it "prevents over writing of in use temporary variables", ->
      program = "
        function makeBools(first, second) {
          return [Boolean(first), Boolean(second)];
        }
        [makeBools(0, '0'), makeBools('null', null)];"
      expect(program).toEvaluateTo [[false, true], [true, false]]

    it "sets the function's name as a variable that points to the function inside " +
       "named function expressions", ->
      # This is a weird quirk of Javascript. If you have a named function expression
      # (not a declaration, but the function is assigned to a variable) then whenever
      # you call that function, the name of the function is a variable within the scope
      # of the function, but it is not available outside the function
      program = "
        function foo() {return foo;}
        a = foo;
        foo = 1;
        a();"
      expect(program).toEvaluateTo 1

      program = "
        delete foo;" + # Get rid of it from previous tests
        "bar = function foo() {return foo;};
        fooExists = 'foo' in window;
        foo = 1;
        [bar() === bar, fooExists];"
      expect(program).toEvaluateTo [true, false]

    it "extends the environment in With blocks", ->
      program = "
        myenv = {a: 1, b: 2};
        b = 6;
        f = function (env) {
          var a = 5;
          with (env) {
            env = {};
            a = 10;
            b = 20;
          }
          return [a, b];
        };
        [f(myenv), myenv];"
      expect(program).toEvaluateTo [[5, 6], {a: 10, b: 20}]

    it "can continue, break and return inside with blocks", ->
      program = "
        f = function () {
          var a = -1;
          e = {a: 0};
          while (true) {
            e.a++;
            with (e) {
              if (a == 1) continue;
              if (a == 2) break;
            }
          }
          with (e) return a;
        };
        f();"
      expect(program).toEvaluateTo 2

    describe "handling errors", ->
      it "evaluates code in Try blocks", ->
        program = "
          a = 0;
          try {
            a += 1;
          } catch (e) {
            a += 2;
          }
          a;"
        expect(program).toEvaluateTo 1

      it "evaluates code in Finally blocks", ->
        program = "
          a = 0;
          try {
            a += 1;
          } finally {
            a += 2;
          }
          a;"
        expect(program).toEvaluateTo 3

      it "can throw custom exceptions", ->
        # Not using the jasmine matcher to make what's happening in the callback
        # function more explicit
        callback = jasmine.createSpy()
        evaluator.eval "throw 'hello'", callback
        expect(callback).toHaveBeenCalledWith('hello', true)

      it "can throw exceptions from nested blocks", ->
        program = "
          function throwException() {throw 'my error';}
          if (true) {
            with ({i: 0}) {
              while (i === 0) {
                switch (0) {
                default:
                  throwException();
                }
                i++;
              }
            }
          }
          'foo';"
        expect(program).toEvaluateTo 'my error', true

      it "can catch thrown exceptions", ->
        program = "
          a = 0;
          try {
            throw 'my error';
            a += 1;
          } catch (e) {
            a += 2;
          }
          a;"
        expect(program).toEvaluateTo 2

      it "can catch exceptions thrown from nested blocks", ->
        program = "
          a = 0;
          function throwException() {throw 'my error';}
          try {
            if (true) {
              with ({i: 0}) {
                while (i === 0) {
                  switch (0) {
                  default:
                    throwException();
                  }
                  i++;
                }
              }
            }
            a += 1;
          } catch (e) {
            a += 2;
          }
          a;"
        expect(program).toEvaluateTo 2

      it "has a reference to the error in catch blocks", ->
        program = "
          a = 0;
          try {
            throw 2;
            a += 1;
          } catch (e) {
            a += e;
          }
          a;"
        expect(program).toEvaluateTo 2

      it "can throw from within a catch block", ->
        program = "
          count = 0;
          try {
            throw 'my error';
          } catch (e) {
            count++;
            if (count < 2) {
              throw 'error with count ' + count;
            }
          }
          'foo';"
        expect(program).toEvaluateTo 'error with count 1', true

      it "will run the finally block after catching an exception", ->
        program = "
          a = 0;
          try {
            throw 'error';
            a += 1;
          } catch (e) {
            a += 2;
          } finally {
            a += 4
          }
          a;"
        expect(program).toEvaluateTo 6

      it "will rethrow caught exceptions at the end of the finally if there is no catch block", ->
        program = "
          myObj = {val: 0};
          try {
            throw myObj;
            myObj.val += 1;
          } finally {
            myObj.val += 2;
          }
          'foo';"
        expect(program).toEvaluateTo {val: 2}, true

      it "does not catch exceptions thrown in the finally block", ->
        program = "
          a = 0;
          try {
          } finally {
            a++;
            throw a;
          }
          'foo';"
        expect(program).toEvaluateTo 1, true

      it "runs the finally block after the catch block throws an exception", ->
        program = "
          myObj = {val: 0};
          try {
            throw myObj;
            myObj.val += 1;
          } catch (e) {
            throw myObj;
            myObj.val += 2
          } finally {
            myObj.val += 4;
          }
          'foo';"
        expect(program).toEvaluateTo {val: 4}, true

      it "bubbles up exceptions in the finally block", ->
        program = "
          try {
            throw 'error in try';
          } catch (e) {
            throw 'error in catch';
          } finally {
            throw 'error in finally';
          }
          'foo';"
        expect(program).toEvaluateTo 'error in finally', true

        program = "
          try {
            throw 'error in try';
          } finally {
            throw 'error in finally';
          }
          'foo';"
        expect(program).toEvaluateTo 'error in finally', true

      it "can continue, break, and return from inside a try", ->
        program = "
          f = function () {
            a = 0;
            while (a < 3) {
              try {
                a++;
                if (a === 1) {
                  continue;
                } else if (a === 2) {
                  break;
                }
              } catch (e) {}
            }
            try {
              return a
            } catch (e) {}
          };
          f();"
        expect(program).toEvaluateTo 2

      it "can continue, break, and return from inside a catch", ->
        program = "
          f = function () {
            a = 0;
            while (a < 3) {
              try {
                throw 'foo'
              } catch (e) {
                a++;
                if (a === 1) {
                  continue;
                } else if (a === 2) {
                  break;
                }
              }
            }
            try {
              return a
            } catch (e) {}
          };
          f();"
        expect(program).toEvaluateTo 2

      it "can continue, break, and return from inside a finally", ->
        program = "
          f = function () {
            a = 0;
            while (a < 3) {
              try {
              } finally {
                a++;
                if (a === 1) {
                  continue;
                } else if (a === 2) {
                  break;
                }
              }
            }
            try {
              return a
            } catch (e) {}
          };
          f();"
        expect(program).toEvaluateTo 2

        program = "
          f = function () {
            a = 0;
            while (a < 3) {
              try {
                throw 'my error';
              } finally {
                a++;
                if (a === 1) {
                  continue;
                } else if (a === 2) {
                  break;
                }
              }
            }
            try {
              return a
            } catch (e) {}
          };
          f();"
        expect(program).toEvaluateTo 2

      it "can throw native exceptions", ->
        program = "
          delete foo;
          foo;"
        expect(program).toEvaluateTo jasmine.any(evaluator.scope.ReferenceError), true

        evaluator.scope.throwError = -> throw myerr
        for err in ["my error", {message: "my message"}, 5, true]
          myerr = err
          expect("throwError();").toEvaluateTo myerr, true

      it "can catch native exceptions", ->
        program = "
          delete foo;
          myerr = null;
          try {
            foo;
          } catch (e) {
            myerr = e;
          }
          myerr;"
        expect(program).toEvaluateTo jasmine.any(evaluator.scope.ReferenceError)

      it "doesn't have a stack trace", ->
        program = "
          delete foo;
          myerr = null;
          try {
            foo;
          } catch (e) {
            myerr = e;
          }
          [myerr.name, myerr.message, myerr.stack];"
        expect(program).toEvaluateTo ['ReferenceError', jasmine.any(String), null]

        evaluator.scope.throwReferenceError = -> foo
        program = "
          delete foo;
          try {
            throwReferenceError();
          } catch (e) {
            myerr = e;
          }
          [myerr.name, myerr.message, myerr.stack];"
        expect(program).toEvaluateTo ['ReferenceError', jasmine.any(String), null]

    describe "the 'this' keyword", ->
      it "has the proper value in user defined functions", ->
        program = "
          val = 0;
          inc = function () {this.val++};
          inc();
          val;"
        expect(program).toEvaluateTo 1

        program = "
          a = {
            val: 0,
            inc: function () {this.val++}
          };
          a.inc();
          a.val;"
        expect(program).toEvaluateTo 1

        program = "
          val = 0;
          a = {
            val: 0,
            inc: function () {this.val++}
          };
          a.inc();
          incCopy = a.inc;
          incCopy();
          incCopy();
          [val, a.val];"
        expect(program).toEvaluateTo [2, 1]

      it "has the proper value in native functions", ->
        inc = -> @val++
        evaluator.scope.inc = inc
        program = "
          val = 0;
          inc();
          val;"
        expect(program).toEvaluateTo 1

        program = "
          a = {
            val: 0,
            inc: inc
          };
          a.inc();
          a.val;"
        expect(program).toEvaluateTo 1

        program = "
          val = 0;
          a = {
            val: 0,
            inc: inc
          };
          a.inc();
          incCopy = a.inc;
          incCopy();
          incCopy();
          [val, a.val];"
        expect(program).toEvaluateTo [2, 1]

    describe "the 'arguments' variable", ->
      describe "in user defined functions", ->
        getArgs = null
        beforeEach ->
          evaluator.eval "getArgs = function () {return arguments;};"
          getArgs = evaluator.scope.getArgs
          expect(getArgs).not.toBeNull()

        it "acts as an array of the arguments passed in", ->
          expect("getArgs(1, 'hi', {}, false);").toEvaluateTo [1, 'hi', {}, false]

        it "defines the arguments even if there is a variable of the same name", ->
          program = "
            f = function () {
              var arguments;
              return arguments;
            };
            f(1, 2, 3); "
          expect(program).toEvaluateTo [1, 2, 3]

        it "has the correct length field", ->
          expect("getArgs(1, 2).length").toEvaluateTo 2
          expect("getArgs(1, 2, [], true).length").toEvaluateTo 4

        it "stores a reference to the function being called", ->
          expect("getArgs().callee").toEvaluateTo getArgs

      describe "in native functions", ->
        getArgs = -> arguments
        beforeEach ->
          evaluator.scope.getArgs = getArgs

        it "acts as an array of the arguments passed in", ->
          expect("getArgs(1, 'hi', {}, false);").toEvaluateTo [1, 'hi', {}, false]

        it "has the correct length field", ->
          expect("getArgs(1, 2).length").toEvaluateTo 2
          expect("getArgs(1, 2, [], true).length").toEvaluateTo 4

        it "stores a reference to the function being called", ->
          expect("getArgs().callee").toEvaluateTo getArgs

    it "works as expected inside method calls", ->
      # There was a strange implementation bug where many things broke inside
      # method calls (such as 'a.b()' as opposed to 'b()')
      program = "
        obj = {
          val: 'my val',
          getVal: function () {
            return this.val;
          },
          f: function (num) {
            a = [];
            a.push(new Object());
            if (num > 2) {
              a.push('yes');
            }
            switch (num) {
              case -1:
                a.push('no');
              default:
                a.push('switch');
            }
            a.push(this.getVal());
            throw a;
          }
        };
        obj.f(3); "
      expect(program).toEvaluateTo [{}, "yes", "switch", "my val"], true

    describe "creating instances", ->
      describe "from user defined functions", ->
        it "calls the constructor with the instance as 'this'", ->
          program = "
            function Cls(arg) {
              this.foo = 'hi';
              this.bar = 'there';
              this.myarg = arg;
            }
            instance = new Cls(5);
            [instance.foo, instance.bar, instance.myarg]; "
          expect(program).toEvaluateTo ["hi", "there", 5]

        it "handles constructors that return a value", ->
          program = "
            function Cls() {
              this.foo = 'bar';
              return 5;
            }
            instance = new Cls();
            instance.foo; "
          expect(program).toEvaluateTo "bar"

        it "bubbles up exceptions thrown in the constructor", ->
          program = "
            function Cls() {
              throw 'my error';
            }
            new Cls(); "
          expect(program).toEvaluateTo "my error", true

        it "sets the prototype properly", ->
          program = "
            function Cls() {}
            Cls.prototype.foo = 'hi';
            Cls.prototype.bar = 'there';
            a = new Cls();
            b = new Cls();
            initial = [a.foo, a.bar, b.foo, b.bar];
            a.foo = 1;
            a.bar = 2;
            Cls.prototype.foo = 3;
            Cls.prototype.bar = 4;
            [initial, [a.foo, a.bar, b.foo, b.bar]]; "
          expect(program).toEvaluateTo [["hi", "there", "hi", "there"], [1, 2, 3, 4]]

      describe "from native functions", ->
        it "calls the native function", ->
          evaluator.scope.Cls = (arg) ->
            @foo = 'hi'
            @bar = 'there'
            @myarg = arg
          program = "
            instance = new Cls(5);
            [instance.foo, instance.bar, instance.myarg]; "
          expect(program).toEvaluateTo ["hi", "there", 5]

        it "handles constructors that return a value", ->
          evaluator.scope.Cls = ->
            @foo = "bar"
            return 5
          program = "
            instance = new Cls();
            instance.foo; "
          expect(program).toEvaluateTo "bar"

        it "bubbles up exceptions thrown in the constructor", ->
          myError = "my error"
          evaluator.scope.Cls = -> throw myError
          program = "
            new Cls(); "
          expect(program).toEvaluateTo myError, true

        it "sets the prototype properly", ->
          evaluator.scope.Cls = ->
          program = "
            Cls.prototype.foo = 'hi';
            Cls.prototype.bar = 'there';
            a = new Cls();
            b = new Cls();
            initial = [a.foo, a.bar, b.foo, b.bar];
            a.foo = 1;
            a.bar = 2;
            Cls.prototype.foo = 3;
            Cls.prototype.bar = 4;
            [initial, [a.foo, a.bar, b.foo, b.bar]]; "
          expect(program).toEvaluateTo [["hi", "there", "hi", "there"], [1, 2, 3, 4]]

    describe "pausing execution", ->
      it "won't call onComplete before execution finishes", ->
        evaluator.scope.pauseExecFunc = ->
          evaluator.pause()
        program = "
          pauseExecFunc();
          5;"
        callback = jasmine.createSpy()
        evaluator.eval program, callback
        expect(callback).not.toHaveBeenCalled()

      it "requires a context to resume execution", ->
        context = null
        evaluator.scope.pauseExecFunc = ->
          context = evaluator.pause()

        evaluator.scope.myObject = myObject = val: 0
        program = "
          f = function () {
            myObject.val = 1;
            pauseExecFunc();
            myObject.val = 2;
          };
          f();
          pauseExecFunc();
          myObject.val = 3;"
        evaluator.eval program
        expect(myObject.val).toEqual 1

        errorMessage = "Resuming evaluation requires a context as returned by pause."
        expect(-> evaluator.resume()).toThrow(errorMessage)
        expect(myObject.val).toEqual 1

        evaluator.resume(context)
        expect(myObject.val).toEqual 2

        expect(-> evaluator.resume({})).toThrow("Invalid context given to resume.")
        expect(myObject.val).toEqual 2

        evaluator.resume(context)
        expect(myObject.val).toEqual 3

      it "can resume into different contexts", ->
        context = null
        evaluator.scope.pauseExecFunc = ->
          context = evaluator.pause()

        evaluator.scope.obj1 = obj1 = val: 0
        evaluator.scope.obj2 = obj2 = val: 0
        program1 =
          "f = function () {
            obj1.val = 1;
            pauseExecFunc();
            obj1.val = 2;
          };
          f();
          pauseExecFunc();
          obj1.val = 3;"
        program2 =
          "f = function () {
            obj2.val = 1;
            pauseExecFunc();
            obj2.val = 2;
          };
          f();
          pauseExecFunc();
          obj2.val = 3;"

        evaluator.eval program1
        context1 = context
        expect(obj1.val).toEqual 1
        expect(obj2.val).toEqual 0

        evaluator.eval program2
        context2 = context
        expect(obj1.val).toEqual 1
        expect(obj2.val).toEqual 1

        evaluator.resume(context2)
        expect(obj1.val).toEqual 1
        expect(obj2.val).toEqual 2

        evaluator.resume(context1)
        expect(obj1.val).toEqual 2
        expect(obj2.val).toEqual 2

        evaluator.resume(context1)
        expect(obj1.val).toEqual 3
        expect(obj2.val).toEqual 2

        evaluator.resume(context2)
        expect(obj1.val).toEqual 3
        expect(obj2.val).toEqual 3

      it "calls the onComplete function even after pausing and resuming", ->
        context = null
        evaluator.scope.pauseExecFunc = ->
          context = evaluator.pause()

        program = "
          pauseExecFunc();
          'foobar';"
        doneCallback = jasmine.createSpy()

        evaluator.eval program, doneCallback
        expect(doneCallback).not.toHaveBeenCalled()
        evaluator.resume(context)
        expect(doneCallback).toHaveBeenCalledWith "foobar", false

# Todo: If result is a promise (some other way to tell that it is our function?
#   our function will tell the evaluator that the next thing is a promise?)
#   then wait for it to be done and re-start evaluation.
# Todo: Handle everything defined on Function.prototype (eg call, apply, toString).
# Todo: Don't allow eval or eval-like functionality
# Todo: Caller property on functions (eg arguments.callee.caller)
# Todo: 'var' statements should evaluate to undefined
# Todo: Allow user defined functions to be called by native functions
# Todo: More scoping tests
# Todo: Flush out documentation
# Todo: Handle For ... In loops
# Todo: Object getter and setter literals
# Todo: Skipping labels, not allowing labelled breaks/continues for now
# Todo: Parentheses for order of operations, eg new (foo()) ();
# Todo: Use the field names the parser uses
# Todo: Functions should implicitly return undefined
