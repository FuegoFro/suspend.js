# Todo: Use the field names the parser uses
# Todo: Possibly use a better statement matching format?
describe "The evaluator module", ->
  evaluator = undefined
  beforeEach ->
    evaluator = new Evaluator()

  afterEach ->
    iframe = evaluator.context.scope.frameElement
    iframe.parentElement.removeChild iframe

  it "creates an iframe to evaluate code in", ->
    iframe = evaluator.context.scope.frameElement
    
    # Need to test against both the global iframe and the iframe's iframe as
    # a workaround for PhantomJS. Really, we just care that it's an iframe.
    isIFrame = iframe instanceof HTMLIFrameElement or
               iframe instanceof evaluator.context.scope.HTMLIFrameElement
    expect(isIFrame).toBe true
    expect(iframe.height).toEqual "0"
    expect(iframe.width).toEqual "0"
    expect(iframe.style.visibility).toEqual "hidden"

  it "creates a temporary variable array in the global scope", ->
    expect(evaluator.context.eval("$__temp__")).toEqual []

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
          unless @env.equals_ @actual, jasmine.any(evaluator.Function)
            message = "Expected #{@actual} to be an instance of " +
            "the evaluator's Function class"
          else unless @actual.name == name
            message = "Expected function to have name #{name}, " +
            "instead had name #{@actual.name}"
          else unless @env.equals_ @actual.params, params
            message = "Expected function to have params #{params}, " +
            "instead had params #{@actual.params}"
          else unless @actual.tempVar == tempVar
            message = "Expected function to stored in variable #{tempVar}, " +
            "instead stored in #{@actual.tempVar}"
          else
            functionBytecode = getExpressionBytecode("(function () {#{body}})")
            bodyInstructions = functionBytecode.preInstructions[0].body
            unless @env.equals_ @actual.body, bodyInstructions
              message = "Expected function body to have same instructions as #{body}"

          @message = -> message
          message == null

        toBeFunctionCall: (callee, args, tempVar, isObjectCreation=false) ->
          targetClass = if isObjectCreation then evaluator.NewObject else evaluator.FunctionCall
          message = null
          unless @env.equals_ @actual, jasmine.any(targetClass)
            message = "Expected #{@actual} to be instance of #{targetClass}."
          else unless @actual.callee == callee
            message = "Calling function #{@actual.callee}, expected to be calling #{callee}."
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
      evaluator.compileExpression body[0].expression

    getStatementsBytecode = (statementsString) ->
      ast = esprima.parse(statementsString).body
      evaluator.compileStatements ast

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
          # Todo: Object getter and setter literals
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
        expect(bytecode.preInstructions[0]).toBeFunctionCall "a.b.c", [], temp
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
        expect(bytecode.preInstructions[0]).toBeFunctionCall "obj.baz", ["a"], t0
        expect(bytecode.preInstructions[1]).toBeFunctionCall "#{t0}.garply", ["b"], t1
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
        expect(ifStatement).toEqual jasmine.any(evaluator.If)
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
        expect(ifStatement).toEqual jasmine.any(evaluator.If)
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
        expect(ifStatement).toEqual jasmine.any(evaluator.If)
        expect(ifStatement.condition).toHaveSameAST t1
        expect(ifStatement.thenCase.length).toEqual 2
        expect(ifStatement.thenCase[0]).toBeFunctionCall 'b', [], t0
        expect(ifStatement.thenCase[1]).toHaveSameAST "#{t0} = #{t0}"
        expect(ifStatement.elseCase.length).toEqual 2
        expect(ifStatement.elseCase[0]).toBeFunctionCall 'c', [], t0
        expect(ifStatement.elseCase[1]).toHaveSameAST "#{t0} = #{t0}"

        expect(bytecode.expression).toEqual t0

    
    # Todo: Parentheses for order of operations:
    #    new (foo()) ();
    describe "statement bytecode", ->
      ###
      Ensures that statements are compiled properly
      ###
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
        original =
          "obj = {msg: 'hi'};" +
          "function foo(a, b) {" +
          "  a[b] = 'foo'" +
          "}" +
          "var bar = function bar() {};"
        bytecode = getStatementsBytecode(original)
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
        expect(returnInst).toEqual jasmine.any(evaluator.Return)
        expect(returnInst.value).toEqual "5"

      it "flattens block statements", ->
        expectStatementsEqual(
          "a = 1; {b = 2; var b, c = 3; function foo() {}}; var c = 4;"
          ["a = 1", "b = 2", "c = 3", "c = 4"]
          ["b", "c"]
          [["foo", [], "", null]]
        )

      it "creates if statements with function scope", ->
        bytecode = getStatementsBytecode(
          "var myval = 'hi';" +
          "if (!isHi(myval)) {" +
          "  var a = 1;" +
          "} else {" +
          "  var b = 2;" +
          "  function inside() {}" +
          "}" +
          "function isHi(val) {return val === 'hi'}" +
          "var c = 'hello';"
        )
        expect(bytecode.declaredVariables).toEqual ["myval", "a", "b", "c"]
        declaredFuncs = bytecode.declaredFunctions
        expect(declaredFuncs.length).toEqual 2
        expect(declaredFuncs[0]).toBeFunctionDef "inside", [], "", null
        expect(declaredFuncs[1]).toBeFunctionDef "isHi", ["val"], "return val === 'hi'", null
        expect(bytecode.instructions.length).toEqual 4
        expect(bytecode.instructions[0]).toHaveSameAST "myval = 'hi'"
        expect(bytecode.instructions[1]).toBeFunctionCall "isHi", ["myval"], tempName(0)
        ifStatement = bytecode.instructions[2]
        expect(ifStatement).toEqual jasmine.any(evaluator.If)
        expect(ifStatement.condition).toHaveSameAST "!#{tempName(0)}"
        expect(ifStatement.thenCase).toHaveSameAST ["a = 1"]
        expect(ifStatement.elseCase).toHaveSameAST ["b = 2"]
        expect(bytecode.instructions[3]).toHaveSameAST "c = 'hello'"

      it "handles if statements without else blocks", ->
        bytecode = getStatementsBytecode(
          "if (predicate) {" +
          "  a = 1;" +
          "}"
        )
        expect(bytecode.instructions.length).toEqual 1
        ifStatement = bytecode.instructions[0]
        expect(ifStatement.thenCase).toHaveSameAST ["a = 1"]
        expect(ifStatement.elseCase).toEqual []

      it "ignores labels", ->
        original =
          "foo:" +
          "{" +
          "  a = 1;" +
          "  bar: a++" +
          "}"
        expectStatementsEqual original, ["a = 1", "a++"]

      
      # Todo: Skipping labels, not allowing labelled breaks/continues for now
      it "creates With blocks with function scope", ->
        bytecode = getStatementsBytecode(
          "with (getEnv()) {" +
          "  var a = 1;" +
          "  function b () {}" +
          "}"
        )
        expect(bytecode.declaredVariables).toEqual ["a"]
        expect(bytecode.declaredFunctions.length).toEqual 1
        expect(bytecode.declaredFunctions[0]).toBeFunctionDef "b", [], "", null
        expect(bytecode.instructions).toEqual [
          new evaluator.FunctionCall("getEnv", [], tempName(0)),
          new evaluator.With(tempName(0), ["a = 1"])
        ]

      describe "creating Switch blocks", ->
        it "desugars case evaluation and flattens all statements", ->
          bytecode = getStatementsBytecode(
            "switch (val()) {" +
            "case first:" +
            "  a = 2;" +
            "  b(a);" +
            "case 'second':" +
            "  a = 5;" +
            "case third():" +
            "  a++;" +
            "}"
          )
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
          expect(firstLevel).toEqual jasmine.any(evaluator.If)
          expect(firstLevel.condition).toHaveSameAST "#{t1} == first"
          expect(firstLevel.thenCase).toHaveSameAST ["#{t0} = 0"]
          expect(firstLevel.elseCase.length).toEqual 1

          secondLevel = firstLevel.elseCase[0]
          expect(secondLevel).toEqual jasmine.any(evaluator.If)
          expect(secondLevel.condition).toHaveSameAST "#{t1} == 'second'"
          expect(secondLevel.thenCase).toHaveSameAST ["#{t0} = 3"]
          expect(secondLevel.elseCase.length).toEqual 2
          expect(secondLevel.elseCase[0]).toBeFunctionCall 'third', [], t2

          thirdLevel = secondLevel.elseCase[1]
          expect(thirdLevel).toEqual jasmine.any(evaluator.If)
          expect(thirdLevel.condition).toHaveSameAST "#{t1} == #{t2}"
          expect(thirdLevel.thenCase).toHaveSameAST ["#{t0} = 4"]
          expect(thirdLevel.elseCase).toHaveSameAST ["#{t0} = null"]

          switchStatement = bytecode.instructions[2]
          expect(switchStatement).toEqual jasmine.any(evaluator.Switch)
          expect(switchStatement.startPCVar).toEqual t0
          inst = switchStatement.instructions
          expect(inst[0]).toHaveSameAST "a = 2"
          expect(inst[1]).toBeFunctionCall "b", ["a"], t0
          expect(inst[2]).toHaveSameAST t0
          expect(inst[3]).toHaveSameAST "a = 5"
          expect(inst[4]).toHaveSameAST "a++"

        it "has function scope", ->
          bytecode = getStatementsBytecode(
            "switch (foo().val) {" +
            "case first:" +
            "  var a = 2;" +
            "case 'second':" +
            "  var b = 5;" +
            "  function c(d) {e}" +
            "}"
          )
          expect(bytecode.declaredVariables).toEqual ["a", "b"]
          expect(bytecode.declaredFunctions.length).toEqual 1
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef "c", ["d"], "e", null
          inst = bytecode.instructions
          expect(inst.length).toEqual 3
          expect(inst[0]).toBeFunctionCall "foo", [], tempName(1)
          expect(inst[1]).toEqual jasmine.any(evaluator.If)
          expect(inst[2]).toEqual jasmine.any(evaluator.Switch)
          expect(inst[2].startPCVar).toEqual tempName(0)

        it "stores a default index", ->
          bytecode = getStatementsBytecode(
            "switch (val) {" +
            "case 'foo':" +
            "  a = 2;" +
            "default:" +
            "  a = 4;" +
            "case 'bar':" +
            "  a = 6;" +
            "}"
          )
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
          expect(firstLevel).toEqual jasmine.any(evaluator.If)
          expect(firstLevel.condition).toHaveSameAST "val == 'foo'"
          expect(firstLevel.thenCase).toHaveSameAST ["#{temp} = 0"]
          expect(firstLevel.elseCase.length).toEqual 1

          secondLevel = firstLevel.elseCase[0]
          expect(secondLevel).toEqual jasmine.any(evaluator.If)
          expect(secondLevel.condition).toHaveSameAST "val == 'bar'"
          expect(secondLevel.thenCase).toHaveSameAST ["#{temp} = 2"]
          expect(secondLevel.elseCase).toHaveSameAST ["#{temp} = 1"]

          switchStatement = bytecode.instructions[1]
          expect(switchStatement).toEqual jasmine.any(evaluator.Switch)
          expect(switchStatement.startPCVar).toEqual temp

      it "creates break statements", ->
        bytecode = getStatementsBytecode(
          "switch (val) {" +
          "case 'foo':" +
          "  break;" +
          "}"
        )
        switchStatement = bytecode.instructions[1]
        breakStatement = switchStatement.instructions[0]
        expect(breakStatement).toEqual jasmine.any(evaluator.Break)

      it "does not allow break statements to have labels", ->
        original =
          "switch (val) {" +
          "case 'foo':" +
          "  lbl:break lbl;" +
          "}"
        expect(->
          getStatementsBytecode original
        ).toThrow "Does not support labelled breaks."

      
      # Note that the tests for while and the tests for continue rely on each other
      it "creates continue statements", ->
        bytecode = getStatementsBytecode(
          "while (true) {" +
          "  continue;" +
          "};"
        )
        whileStatement = bytecode.instructions[0]
        continueStatement = whileStatement.instructions[1]
        expect(continueStatement).toEqual jasmine.any(evaluator.Continue)

      it "doesn't allow labelled continues", ->
        original =
          "lbl:" +
          "while (true) {" +
          "  continue lbl;" +
          "};"
        expect(->
          getStatementsBytecode original
        ).toThrow "Does not support labelled continues."

      describe "creating While loops", ->
        it "desugars it into a loop", ->
          bytecode = getStatementsBytecode(
            "while (!shouldStop()) {" +
            "  a = 1 + 2;" +
            "  a++;" +
            "}"
          )
          
          # This should desugar to:
          #  temp = shouldStop()
          #  if (!(!temp))
          #    break;
          #  a = 1 + 2
          #  a++
          #  continue;

          expect(bytecode.instructions.length).toEqual 1
          loopInstr = bytecode.instructions[0]
          expect(loopInstr).toEqual jasmine.any(evaluator.Loop)
          expect(loopInstr.instructions.length).toEqual 5

          expect(loopInstr.instructions[0]).toBeFunctionCall "shouldStop", [], tempName(0)

          ifStatement = loopInstr.instructions[1]
          expect(ifStatement).toEqual jasmine.any(evaluator.If)
          expect(ifStatement.condition).toHaveSameAST "!!#{tempName(0)}"
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

          expect(loopInstr.instructions[2]).toHaveSameAST "a = 1 + 2"
          expect(loopInstr.instructions[3]).toHaveSameAST "a++"
          expect(loopInstr.instructions[4]).toEqual jasmine.any(evaluator.Continue)

        it "uses function scope", ->
          bytecode = getStatementsBytecode(
            "while (true) {" +
            "  var a = 1 + 2;" +
            "  function b(c) {d};" +
            "}"
          )
          expect(bytecode.declaredVariables).toEqual ["a"]
          expect(bytecode.declaredFunctions.length).toEqual 1
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef "b", ["c"], "d", null


      describe "creating Do While loops", ->
        it "unrolls one execution and desugars it into a loop", ->
          bytecode = getStatementsBytecode(
            "do {" +
            "  a = 1 + 2;" +
            "  a++;" +
            "} while (!shouldStop());"
          )
          
          # This should desugar to:
          #  temp = shouldStop()
          #  if (!(!temp))
          #    break;
          #  a = 1 + 2
          #  a++
          #  continue;

          expect(bytecode.instructions.length).toEqual 1
          loopInstr = bytecode.instructions[0]
          expect(loopInstr).toEqual jasmine.any(evaluator.Loop)
          expect(loopInstr.instructions.length).toEqual 5
          expect(loopInstr.initialPC).toEqual 2 # Do not want to check the predicate the first time

          expect(loopInstr.instructions[0]).toBeFunctionCall "shouldStop", [], tempName(0)
          ifStatement = loopInstr.instructions[1]
          expect(ifStatement).toEqual jasmine.any(evaluator.If)
          expect(ifStatement.condition).toHaveSameAST "!!#{tempName(0)}"
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

          expect(loopInstr.instructions[2]).toHaveSameAST "a = 1 + 2"
          expect(loopInstr.instructions[3]).toHaveSameAST "a++"
          expect(loopInstr.instructions[4]).toEqual jasmine.any(evaluator.Continue)

        it "uses function scope", ->
          bytecode = getStatementsBytecode(
            "do {" +
            "  var a = 1 + 2;" +
            "  function b(c) {d};" +
            "} while (true);"
          )
          expect(bytecode.declaredVariables).toEqual ["a"]
          expect(bytecode.declaredFunctions.length).toEqual 1
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef "b", ["c"], "d", null

      describe "creating For loops", ->
        it "desugars it into a flat loop", ->
          bytecode = getStatementsBytecode(
            "for (i = initVar(); i !== lst().length; i++) {" +
            "  a.push(i + 1);" +
            "}"
          )
          
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
          expect(loopInstr).toEqual jasmine.any(evaluator.Loop)
          expect(loopInstr.instructions.length).toEqual 6

          expect(loopInstr.instructions[0]).toBeFunctionCall "lst", [], t0

          ifStatement = loopInstr.instructions[1]
          expect(ifStatement).toEqual jasmine.any(evaluator.If)
          expect(ifStatement.condition).toHaveSameAST "!(i !== #{t0}.length)"
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

          expect(loopInstr.instructions[2]).toBeFunctionCall "a.push", ["i + 1"], t0
          expect(loopInstr.instructions[3]).toHaveSameAST t0
          expect(loopInstr.instructions[4]).toHaveSameAST "i++"
          expect(loopInstr.instructions[5]).toEqual jasmine.any(evaluator.Continue)

        it "uses function scope", ->
          bytecode = getStatementsBytecode(
            "for (var i = init; i != 0; --i) {" +
            "  var a = 1 + 2;" +
            "  function b(c) {d};" +
            "};"
          )
          expect(bytecode.declaredVariables).toEqual ["i", "a"]
          expect(bytecode.declaredFunctions.length).toEqual 1
          expect(bytecode.declaredFunctions[0]).toBeFunctionDef "b", ["c"], "d", null

        it "handles empty expressions in loop definition", ->
          # No initializer
          bytecode = getStatementsBytecode("for (; b; c) {}")
          expect(bytecode.instructions.length).toEqual 1 # Loop
          forLoop = bytecode.instructions[0]
          expect(forLoop).toEqual jasmine.any(evaluator.Loop)
          expect(forLoop.instructions.length).toEqual 3 # Finished test, update, continue
          expect(forLoop.instructions[0]).toEqual jasmine.any(evaluator.If)
          expect(forLoop.instructions[0].condition).toHaveSameAST "!b"
          expect(forLoop.instructions[0].thenCase).toEqual [jasmine.any(evaluator.Break)]
          expect(forLoop.instructions[0].elseCase).toEqual []
          expect(forLoop.instructions[1]).toEqual "c"
          expect(forLoop.instructions[2]).toEqual jasmine.any(evaluator.Continue)
          
          # No finished test
          bytecode = getStatementsBytecode("for (a; ; c) {}")
          expect(bytecode.instructions.length).toEqual 2 # Initializer and loop
          expect(bytecode.instructions[0]).toEqual "a"
          forLoop = bytecode.instructions[1]
          expect(forLoop).toEqual jasmine.any(evaluator.Loop)
          expect(forLoop.instructions.length).toEqual 2 # Update and continue
          expect(forLoop.instructions[0]).toEqual "c"
          expect(forLoop.instructions[1]).toEqual jasmine.any(evaluator.Continue)
          
          # No update
          bytecode = getStatementsBytecode("for (a; b; ) {}")
          expect(bytecode.instructions.length).toEqual 2 # Initializer and loop
          expect(bytecode.instructions[0]).toEqual "a"
          forLoop = bytecode.instructions[1]
          expect(forLoop).toEqual jasmine.any(evaluator.Loop)
          expect(forLoop.instructions.length).toEqual 2 # Finished test and continue
          expect(forLoop.instructions[0]).toEqual jasmine.any(evaluator.If)
          expect(forLoop.instructions[0].condition).toHaveSameAST "!b"
          expect(forLoop.instructions[0].thenCase).toEqual [jasmine.any(evaluator.Break)]
          expect(forLoop.instructions[0].elseCase).toEqual []
          expect(forLoop.instructions[1]).toEqual jasmine.any(evaluator.Continue)

  # Todo: Handle labelled continues and breaks
  # Todo: Handle try, throw, catch
  # Todo: Handle For ... In loops
  # Todo: Handle object getters and setters

  describe "when interpreting bytecode", ->
    it "uses its context to evaluate non-function expressions", ->
      num = 5
      testId =
        foo: "foo value"
        bar: "bar value"
        0: "zero value"
        9: "num value"

      evaluator.context.setValue "num", num
      evaluator.context.setValue "testId", testId
      
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
        inputString = pair[0]
        outputValue = pair[1]
        evaluated = evaluator.eval(inputString)
        expect(evaluated).toEqual outputValue


    it "conditionally executes code in If blocks", ->
      program =
        "a = 0;" +
        "if (val) {" +
        "  a += 1;" +
        "} else {" +
        "  a += 2;" +
        "}" +
        "a;"
      evaluator.eval "val = true"
      expect(evaluator.eval(program)).toEqual 1
      evaluator.eval "val = false"
      expect(evaluator.eval(program)).toEqual 2

    it "can break out of loops", ->
      program =
        "a = 0;" +
        "while (true) {" +
        "  break;" +
        "  a = 1;" +
        "}" +
        "a;"
      expect(evaluator.eval(program)).toEqual 0

    it "can break out of nested blocks", ->
      program =
        "a = 0;" +
        "while (true) {" +
        "  if (true) break;" +
        "  a = 1;" +
        "}" +
        "a;"
      expect(evaluator.eval(program)).toEqual 0
      program =
        "a = 0;" +
        "do {" +
        "  while (true) {" +
        "    if (true) break;" +
        "    a += 1;" +
        "  }" +
        "  a += 2" +
        "} while (false)" +
        "a;"
      expect(evaluator.eval(program)).toEqual 2

    it "can continue in loops", ->
      program =
        "a = 0;" +
        "while (true) {" +
        "  if (a > 0) break;" +
        "  a++;" +
        "  continue;" +
        "  a = 5;" +
        "}" +
        "a;"
      expect(evaluator.eval(program)).toEqual 1

    it "can continue in nested blocks", ->
      program =
        "a = 0;" +
        "while (true) {" +
        "  if (a > 0) break;" +
        "  a++;" +
        "  if (true) continue;" +
        "  a = 5;" +
        "}" +
        "a;"
      expect(evaluator.eval(program)).toEqual 1

    it "repeats loops normally", ->
      program =
        "a = 0;" +
        "while (a != 3) {" +
        "  a++;" +
        "}" +
        "a;"
      expect(evaluator.eval(program)).toEqual 3
      program =
        "arr = [1,2,3];" +
        "for (i = 0; i < arr.length; i++) {" +
        "  arr[i] *= arr[i];" +
        "}" +
        "arr;"
      expect(evaluator.eval(program)).toEqual [1, 4, 9]

    it "checks the predicate when continuing in Do While loops", ->
      program =
        "a = 0;" +
        "do {" +
        "  a++;" +
        "  if (a > 1) break;" +
        "  continue;" +
        "} while (false)" +
        "a"
      expect(evaluator.eval(program)).toEqual 1

    it "will call native functions", ->
      myNativeFunc = jasmine.createSpy().andReturn(12)
      evaluator.context.setValue "myNativeFunc", myNativeFunc
      expect(evaluator.eval("myNativeFunc('hi', false)")).toEqual 12
      expect(myNativeFunc).toHaveBeenCalledWith "hi", false

    it "can define and call user functions", ->
      program =
        "o = {foo: 1};" +
        "f = function () {" +
        "  o.foo += 1;" +
        "};" +
        "f();" +
        "o;"
      expect(evaluator.eval(program)).toEqual foo: 2

    it "uses function scope when calling functions", ->
      program =
        "a = 1;" +
        "b = 2;" +
        "c = 3;" +
        "f = function (a) {" +
        "  a += 1;" +
        "  var b = 6;" +
        "  c = a + b;" +
        "};" +
        "f(4);" +
        "[a, b, c];"
      evaluator.eval "val = 3"
      expect(evaluator.eval(program)).toEqual [1, 2, 11]

    it "can return values", ->
      program =
        "increment = function (val) {" +
        "  return val + 1;" +
        "};" +
        "increment(5);"
      expect(evaluator.eval(program)).toEqual 6

    it "handles nested functions and has closures", ->
      program =
        "counter = function () {" +
        "  var count = 0;" +
        "  return function () {" +
        "    return count++;" +
        "  };" +
        "};" +
        "firstCounter = counter();" +
        "firstRes = [];" +
        "firstRes.push(firstCounter());" +
        "firstRes.push(firstCounter());" +
        "secondCounter = counter();" +
        "secondRes = [];" +
        "secondRes.push(secondCounter());" +
        "firstRes.push(firstCounter());" +
        "secondRes.push(secondCounter());" +
        "secondRes.push(secondCounter());" +
        "firstRes.push(firstCounter());" +
        "firstRes.push(firstCounter());" +
        "[firstRes, secondRes]"
      expect(evaluator.eval(program)).toEqual [[0, 1, 2, 3, 4], [0, 1, 2]]

    it "uses enclosing scopes for control blocks", ->
      program =
        "f = function (bool, secondBool) {" +
        "  var a;" +
        "  if (bool) {" +
        "    a = 'first';" +
        "  } else if (secondBool) {" +
        "    a = 'second';" +
        "  } else {" +
        "    a = 'third';" +
        "  }" +
        "  return a;" +
        "};" +
        "[f(true, true), f(true, false), f(false, true), f(false, false)];"
      expect(evaluator.eval(program)).toEqual ['first', 'first', 'second', 'third']

    # Todo: More scoping tests
    it "jumps to the correct case in a switch statement", ->
      program =
        "a = 0;" +
        "switch (val) {" +
        "case 0:" +
        "  a += 1;" +
        "  break;" +
        "case 1:" +
        "  a += 2;" +
        "case 2:" +
        "  a += 3;" +
        "  break;" +
        "}" +
        "a;"
      evaluator.eval "val = 0"
      expect(evaluator.eval(program)).toEqual 1
      evaluator.eval "val = 1"
      expect(evaluator.eval(program)).toEqual 5
      evaluator.eval "val = 2"
      expect(evaluator.eval(program)).toEqual 3
      evaluator.eval "val = 3" # Does not match any case
      expect(evaluator.eval(program)).toEqual 0

    it "will use the default case in a switch if no other cases match", ->
      program =
        "a = 0;" +
        "switch (val) {" +
        "case 0:" +
        "  a += 1;" +
        "default:" +
        "  a += 2;" +
        "case 1:" +
        "  a += 3;" +
        "}" +
        "a;"
      evaluator.eval "val = 0"
      expect(evaluator.eval(program)).toEqual 6
      evaluator.eval "val = 1"
      expect(evaluator.eval(program)).toEqual 3
      evaluator.eval "val = 2" # Default case
      expect(evaluator.eval(program)).toEqual 5

    it "can continue and return from within switch statements", ->
      program =
        "f = function () {" +
        "  var a = 0;" +
        "  while (true) {" +
        "    a++;" +
        "    switch (a) {" +
        "      case 1:" +
        "        continue;" +
        "      case 2:" +
        "        return a;" +
        "    }" +
        "  }" +
        "};" +
        "f();"
      expect(evaluator.eval(program)).toEqual 2

    it "short circuits case expression evalution", ->
      program =
        "obj = {a: 0, b: 0, c: 0};" +
        "incr = function (val) {obj[val]++; return val;};" +
        "switch('b') {" +
        "  case incr('a'):" +
        "  case incr('b'):" +
        "  case incr('c'):" +
        "}" +
        "obj"
      expect(evaluator.eval(program)).toEqual a: 1, b: 1, c: 0

    it "short circuits logical expression", ->
      program =
        "a = 0;" +
        "increment = function (val) {a++; return val};" +
        "res = increment('') && increment('hi');" +
        "[a, res];"
      expect(evaluator.eval(program)).toEqual [1, '']

      program =
        "a = 0;" +
        "increment = function (val) {a++; return val};" +
        "res = increment('') && increment('hi') || increment('hello');" +
        "[a, res];"
      expect(evaluator.eval(program)).toEqual [2, "hello"]

    it "short circuits ternary expressions", ->
      program =
        "obj = {a: 0, b: 0, c: 0};" +
        "incr = function (val) {obj[val]++; return val;};" +
        "res = incr('a') ? incr('b') : incr('c');" +
        "[obj, res];"
      expect(evaluator.eval(program)).toEqual [{a: 1, b: 1, c: 0}, 'b']

    it "can pause execution", ->
      pauseExecFunc = ->
        evaluator.pause()

      myObject = val: 0
      evaluator.context.setValue "pauseExecFunc", pauseExecFunc
      evaluator.context.setValue "myObject", myObject
      program =
        "f = function () {" +
        "  myObject.val = 1;" +
        "  pauseExecFunc();" +
        "  myObject.val = 2;" +
        "};" +
        "f();" +
        "pauseExecFunc();" +
        "myObject.val = 3;"
      evaluator.eval program
      expect(myObject.val).toEqual 1
      evaluator.resume()
      expect(myObject.val).toEqual 2
      evaluator.resume()
      expect(myObject.val).toEqual 3




# Execution to do's:
# Todo: Test function expressions
#    Handle creation of closures and proper prototype chains
#    Make sure function toString works
#    Make sure named function expressions work:
#      f = function a() {/* a defined */}; /* a not defined */
# Todo: Test 'new' object creation
#    See function/method calls below. Also need to handle object creation
#    ourselves using Object.create and then calling the constructor with
#    the created object as the 'this' parameter.
# Todo: Test function/method calls
#    If function is actually object and instance of our custom function
#    type, then execute our way. Otherwise execute normally. If result is
#    a promise (some other way to tell that it is our function? our
#    function will tell the evaluator that the next thing is a promise?)
#    then wait for it to be done and re-start evaluation.
#
#    Wrap in a function call to set 'this'
#
#    Also need to handle everything defined on Function.prototype
#      (eg call, apply, toString).
# Todo: Don't allow eval or eval-like functionality
# Todo: Test delete with scoping
# Todo: Make sure 'this' and 'arguments' work
# Todo: Make sure all 'hidden' variables are defined/work
# Todo: Make sure declared functions work
# Todo: Test when a function's name is re-defined within the function
# Todo: 'var' statements should evaluate to undefined
# Todo: After resuming, errors need to bubble up
#    - entire result passing system needs to be callback based
# Todo: Errors should clear the execution state/stack
# Todo: Allow user defined functions to be called by native functions
# Todo: Test With block evaluation
# Todo: Test object creation
