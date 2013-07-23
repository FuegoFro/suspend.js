# global describe, it, esprima, expect, Evaluator, jasmine, beforeEach, afterEach 

# global xit 

# jshint evil:true, sub:true 

# Todo: Use custom jasmine matchers
# Todo: Use the field names the parser uses
# Todo: Possibly use a better statement matching format?
# Todo: Convert everything to coffeescript
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
    getExpressionBytecode = (expressionString) ->
      body = esprima.parse(expressionString).body
      expect(body.length).toEqual 1
      expect(body[0].type).toEqual "ExpressionStatement"
      evaluator.compileExpression body[0].expression

    getStatementsBytecode = (statementsString) ->
      ast = esprima.parse(statementsString).body
      evaluator.compileStatements ast

    expectFunctionDef = (func, name, params, body, tempVar) ->
      expect(func).toEqual jasmine.any(evaluator.Function)
      expect(func.name).toEqual name
      expect(func.params).toEqual params
      expect(func.tempVar).toEqual tempVar
      functionBytecode = getExpressionBytecode("(function () {#{body}})")
      bodyInstructions = functionBytecode.preInstructions[0].body
      expect(func.body).toEqual bodyInstructions

    expectFunctionCall = (functionCall, callee, args, tempVar, isObjectCreation) ->
      isObjectCreation = !!isObjectCreation # Default to false
      targetClass = (if isObjectCreation then evaluator.NewObject else evaluator.FunctionCall)
      expect(functionCall).toEqual jasmine.any(targetClass)
      expect(functionCall.callee).toEqual callee
      expect(functionCall.args.length).toEqual args.length
      for arg, i in functionCall.args
        expectAstEqual arg, args[i]
      expect(functionCall.tempVar).toEqual tempVar

    expectAstEqual = (actual, expected) ->
      actualAst = esprima.parse(actual).body
      expectedAst = esprima.parse(expected).body
      expect(actualAst).toEqual expectedAst

    tempName = (index) ->
      "$__temp__[#{index}]"

    describe "expression bytecode", ->
      expressionExpectNoChange = (expressionString) ->
        try
          body = esprima.parse(expressionString).body
          expect(body.length).toEqual 1
          expect(body[0].type).toEqual "ExpressionStatement"
          bytecode = evaluator.compileExpression(body[0].expression)
          expect(bytecode.preInstructions).toEqual []
          
          # Compare AST to make sure is logically/syntactically the same,
          # even if formatting differs.
          reParsed = esprima.parse(bytecode.expression).body
          expect(reParsed).toEqual body
        catch e
          throw new Error("Failed to compile #{expressionString}. Error is: #{e.toString()}")
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
          "true || false", "a && b || c", # logical operators
          "useSpace ? ' ' : ''", "a && b ? 15 : 25", "a.b", # ternary expressions
          "foo[bar]", "arr[0]", "obj['key']", # membership access
          "(1 + 2) * 3", "a && (b || c)", # order of operations
        ]
        for expression in original
          expressionExpectNoChange expression

      it "can define functions", ->
        temp = tempName(0)

        original = "(function () {var a = {hi: 'hello'}; a.b = 123; var b = 'foo'})"
        bytecode = getExpressionBytecode(original)
        expect(bytecode.preInstructions.length).toEqual 1
        # Function should be pulled out into a separate statement
        func = bytecode.preInstructions[0]
        body = "var a = {hi: 'hello'}; a.b = 123; var b = 'foo'"
        expectFunctionDef func, null, [], body, temp
        # The expression itself should be the temp var containing the function
        expect(bytecode.expression).toEqual temp

        original = "(function foo(a, b) {a + b})"
        bytecode = getExpressionBytecode(original)
        expect(bytecode.preInstructions.length).toEqual 1
        # Function should be pulled out into a separate statement
        func = bytecode.preInstructions[0]
        expectFunctionDef func, "foo", ["a", "b"], "a + b", temp
        # The expression itself should be the temp var containing the function
        expect(bytecode.expression).toEqual temp

      it "can call functions", ->
        temp = tempName(0)

        bytecode = getExpressionBytecode("a.b.c()")
        expect(bytecode.preInstructions.length).toEqual 1
        funcCall = bytecode.preInstructions[0]
        expectFunctionCall funcCall, "a.b.c", [], temp
        expect(bytecode.expression).toEqual temp

        bytecode = getExpressionBytecode("func(foo, bar)")
        expect(bytecode.preInstructions.length).toEqual 1
        funcCall = bytecode.preInstructions[0]
        expectFunctionCall funcCall, "func", ["foo", "bar"], temp
        expect(bytecode.expression).toEqual temp

      it "can create new objects", ->
        temp = tempName(0)
        bytecode = getExpressionBytecode("new foo(a, b, c)")
        expect(bytecode.preInstructions.length).toEqual 1
        newObj = bytecode.preInstructions[0]
        expectFunctionCall newObj, "foo", ["a", "b", "c"], temp, true
        expect(bytecode.expression).toEqual temp

      it "handles multiple pre-instructions", ->
        t0 = tempName(0)
        t1 = tempName(1)
        bytecode = getExpressionBytecode("[foo(), bar()]")
        expect(bytecode.preInstructions.length).toEqual 2
        expectFunctionCall bytecode.preInstructions[0], "foo", [], t0
        expectFunctionCall bytecode.preInstructions[1], "bar", [], t1
        expectAstEqual bytecode.expression, "[" + t0 + ", " + t1 + "]"

        bytecode = getExpressionBytecode("obj.baz(a).garply(b).length")
        expect(bytecode.preInstructions.length).toEqual 2
        expectFunctionCall bytecode.preInstructions[0], "obj.baz", ["a"], t0
        expectFunctionCall bytecode.preInstructions[1], t0 + ".garply", ["b"], t1
        expect(bytecode.expression).toEqual t1 + ".length"

        bytecode = getExpressionBytecode("outer(inner())")
        expect(bytecode.preInstructions.length).toEqual 2
        expectFunctionCall bytecode.preInstructions[0], "inner", [], t0
        expectFunctionCall bytecode.preInstructions[1], "outer", [t0], t1
        expect(bytecode.expression).toEqual t1


    
    # Todo: Make sure ternary operators don't evaluate both 'then' and 'else'
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
          functionInformation = [actualFunc].concat expectedFunctions[i]
          expectFunctionDef.apply null, functionInformation

        
        # Check that AST's match
        expect(compiled.length).toEqual expectedStatements.length
        for c, i in compiled
          expectAstEqual c, expectedStatements[i]

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
        expectAstEqual bytecode.instructions[0], "a = 5"
        expectFunctionCall bytecode.instructions[1], "g", [], t0
        expectFunctionCall bytecode.instructions[2], "f", ["a", t0], t1
        expectAstEqual bytecode.instructions[3], "b = " + t1

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
        expectFunctionDef declaredFuncs[0], "foo", ["a", "b"], "a[b] = 'foo'", null
        expect(bytecode.instructions.length).toEqual 3
        expectAstEqual bytecode.instructions[0], "obj = {msg: 'hi'}"
        expectFunctionDef bytecode.instructions[1], "bar", [], "", tempName(0)
        expectAstEqual bytecode.instructions[2], "bar = " + tempName(0)

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
        expectFunctionDef declaredFuncs[0], "inside", [], "", null
        expectFunctionDef declaredFuncs[1], "isHi", ["val"], "return val === 'hi'", null
        expect(bytecode.instructions.length).toEqual 4
        expectAstEqual bytecode.instructions[0], "myval = 'hi'"
        expectFunctionCall bytecode.instructions[1], "isHi", ["myval"], tempName(0)
        ifStatement = bytecode.instructions[2]
        expect(ifStatement).toEqual jasmine.any(evaluator.If)
        expectAstEqual ifStatement.condition, "!" + tempName(0)
        expect(ifStatement.thenCase.length).toEqual 1
        expectAstEqual ifStatement.thenCase[0], "a = 1"
        expect(ifStatement.elseCase.length).toEqual 1
        expectAstEqual ifStatement.elseCase[0], "b = 2"
        expectAstEqual bytecode.instructions[3], "c = 'hello'"

      it "handles if statements without else blocks", ->
        bytecode = getStatementsBytecode(
          "if (predicate) {" +
          "  a = 1;" +
          "}"
        )
        expect(bytecode.instructions.length).toEqual 1
        ifStatement = bytecode.instructions[0]
        expect(ifStatement.thenCase.length).toEqual 1
        expectAstEqual ifStatement.thenCase[0], "a = 1"
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
        expectFunctionDef bytecode.declaredFunctions[0], "b", [], "", null
        expect(bytecode.instructions).toEqual [new evaluator.FunctionCall("getEnv", [], tempName(0)), new evaluator.With(tempName(0), ["a = 1"])]

      describe "creating Switch blocks", ->
        it "flattens all cases into a single array of statements", ->
          bytecode = getStatementsBytecode(
            "switch (val) {" +
            "case first:" +
            "  a = 2;" +
            "  a++;" +
            "case 'second':" +
            "  a = 5;" +
            "  b(a);" +
            "}"
          )
          expect(bytecode.instructions.length).toEqual 1
          switchStatement = bytecode.instructions[0]
          expect(switchStatement).toEqual jasmine.any(evaluator.Switch)
          expect(switchStatement.value).toEqual "val"
          inst = switchStatement.instructions
          expectAstEqual inst[0], "a = 2"
          expectAstEqual inst[1], "a++"
          expectAstEqual inst[2], "a = 5"
          expectFunctionCall inst[3], "b", ["a"], tempName(0)
          expectAstEqual inst[4], tempName(0)

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
          expectFunctionDef bytecode.declaredFunctions[0], "c", ["d"], "e", null
          inst = bytecode.instructions
          expect(inst.length).toEqual 2
          expectFunctionCall inst[0], "foo", [], tempName(0)
          expect(inst[1]).toEqual jasmine.any(evaluator.Switch)
          expect(inst[1].value).toEqual tempName(0) + ".val"

        it "maps between cases wrapped in functions and index into the statements", ->
          bytecode = getStatementsBytecode(
            "switch (val) {" +
            "case first:" +
            "  a = 2;" +
            "  a++;" +
            "case 'second':" +
            "case false:" +
            "  a = 8;" +
            "  a++;" +
            "case foo():" +
            "  a = 11;" +
            "}"
          )
          expect(bytecode.instructions.length).toEqual 1
          switchStatement = bytecode.instructions[0]
          expectFunctionDef switchStatement.cases[0].test, null, [], "return first", null
          expectFunctionDef switchStatement.cases[1].test, null, [], "return 'second'", null
          expectFunctionDef switchStatement.cases[2].test, null, [], "return false", null
          expectFunctionDef switchStatement.cases[3].test, null, [], "return foo()", null
          expect(switchStatement.cases[0].index).toEqual 0
          expect(switchStatement.cases[1].index).toEqual 2
          expect(switchStatement.cases[2].index).toEqual 2
          expect(switchStatement.cases[3].index).toEqual 4

        it "stores a default index", ->
          bytecode = getStatementsBytecode(
            "switch (val) {" +
            "case 'foo':" +
            "  a = 2;" +
            "}"
          )
          expect(bytecode.instructions[0].default).toBeNull()
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
          expect(bytecode.instructions[0].default).toEqual 1


      it "creates break statements", ->
        bytecode = getStatementsBytecode(
          "switch (val) {" +
          "case 'foo':" +
          "  break;" +
          "}"
        )
        switchStatement = bytecode.instructions[0]
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

          expectFunctionCall loopInstr.instructions[0], "shouldStop", [], tempName(0)

          ifStatement = loopInstr.instructions[1]
          expect(ifStatement).toEqual jasmine.any(evaluator.If)
          expectAstEqual ifStatement.condition, "!!" + tempName(0)
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

          expectAstEqual loopInstr.instructions[2], "a = 1 + 2"
          expectAstEqual loopInstr.instructions[3], "a++"
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
          expectFunctionDef bytecode.declaredFunctions[0], "b", ["c"], "d", null


      describe "creating Do While loops", ->
        it "desugars it into a loop", ->
          bytecode = getStatementsBytecode(
            "do {" +
            "  a = 1 + 2;" +
            "  a++;" +
            "} while (!shouldStop());"
          )
          
          # This should desugar to:
          #  a = 1 + 2
          #  a++
          #  temp = shouldStop()
          #  if (!(!temp))
          #    break;
          #  continue;

          expect(bytecode.instructions.length).toEqual 1
          loopInstr = bytecode.instructions[0]
          expect(loopInstr).toEqual jasmine.any(evaluator.Loop)
          expect(loopInstr.instructions.length).toEqual 5

          expectAstEqual loopInstr.instructions[0], "a = 1 + 2"
          expectAstEqual loopInstr.instructions[1], "a++"
          expectFunctionCall loopInstr.instructions[2], "shouldStop", [], tempName(0)

          ifStatement = loopInstr.instructions[3]
          expect(ifStatement).toEqual jasmine.any(evaluator.If)
          expectAstEqual ifStatement.condition, "!!" + tempName(0)
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

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
          expectFunctionDef bytecode.declaredFunctions[0], "b", ["c"], "d", null


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

          expect(bytecode.instructions.length).toEqual 3
          expectFunctionCall bytecode.instructions[0], "initVar", [], tempName(0)
          expectAstEqual bytecode.instructions[1], "i = " + tempName(0)
          loopInstr = bytecode.instructions[2]
          expect(loopInstr).toEqual jasmine.any(evaluator.Loop)
          expect(loopInstr.instructions.length).toEqual 6

          expectFunctionCall loopInstr.instructions[0], "lst", [], tempName(0)

          ifStatement = loopInstr.instructions[1]
          expect(ifStatement).toEqual jasmine.any(evaluator.If)
          expectAstEqual ifStatement.condition, "!(i !== " + tempName(0) + ".length)"
          expect(ifStatement.thenCase.length).toEqual 1
          expect(ifStatement.thenCase[0]).toEqual jasmine.any(evaluator.Break)
          expect(ifStatement.elseCase).toEqual []

          expectFunctionCall loopInstr.instructions[2], "a.push", ["i + 1"], tempName(0)
          expectAstEqual loopInstr.instructions[3], tempName(0)
          expectAstEqual loopInstr.instructions[4], "i++"
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
          expectFunctionDef bytecode.declaredFunctions[0], "b", ["c"], "d", null

        it "handles empty expressions in loop definition", ->
          # No initializer
          bytecode = getStatementsBytecode("for (; b; c) {}")
          expect(bytecode.instructions.length).toEqual 1 # Loop
          forLoop = bytecode.instructions[0]
          expect(forLoop).toEqual jasmine.any(evaluator.Loop)
          expect(forLoop.instructions.length).toEqual 3 # Finished test, update, continue
          expect(forLoop.instructions[0]).toEqual jasmine.any(evaluator.If)
          expectAstEqual forLoop.instructions[0].condition, "!b"
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
          expectAstEqual forLoop.instructions[0].condition, "!b"
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
        ["({msg: 'hello', arr: [1,2,3], obj: {foo: 'bar'}})", msg: "hello", arr: [1, 2, 3], obj: {foo: "bar"}],
        ["1, 2, 'red', 'blue'", `(1, 2, "red", "blue")`],
        ["-num", -5], # num = 5
        ["+5", 5],
        ["!'hey'", false],
        ["typeof testId", "object"],
        ["void 'hi'", `undefined`],
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

    
    # Todo: continue in do..while is wrong. The following is supposed to exit:
    #    do {
    #      continue;
    #    } while (false)
    # but right now it loops forever
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

    
    # Todo: More scoping tests
    xit "jumps to the correct case in a switch statement", ->
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

    xit "will use the default case in a switch if no other cases match", ->
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

    
    # Todo: Make sure switch doesn't evaluate all case expressions, just the ones it gets to
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
