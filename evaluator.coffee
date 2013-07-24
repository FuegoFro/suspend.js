#*
#* The evaluation takes place in 2 main steps:
#*   = Walk the ast, convert into 'bytecode'
#*     ~ Function definitions, function calls, control structures, returns are
#*       special cased into corresponding objects.
#*     ~ Pull out all var's to the front of the scope
#*       - At function definition, when we create it's environment, map all of
#*         the var's in the scope to undefined.
#*     ~ All others are strings to be eval'd.
#*     ~ Unpack inline or chained method calls and function definitions,
#*       introducing temporary variables as necessary.
#*   = Interpret the 'bytecode'
#*     ~ Keep track of environment.
#*       - When eval'ing statements, wrap the text being eval'd in 'with' blocks
#*       - On function invocation, use the closure as the environment.
#*       - On function return, use the previous environment (keep a stack)
#*     ~ Keep track of program counter
#*       - Handle all control flow, function invocation, function return
#*     ~ Be able to stop and restart, remembering state
#*       - On stop, save off all relevant state (call stack, program counter)
#*         and return from evaluation.
#*       - On resume, pick up where left off
#*     ~ Handle promises returned by functions.
#*
#*

compileStatements = (statements) ->
  subExpression = (toCompile, currentTemp) ->
    return null if toCompile is null
    compiled = compileExpression(toCompile, currentTemp)
    merge instructions, compiled.preInstructions
    compiled.expression

  subStatements = (toCompile) ->
    return [] if toCompile is null
    toCompile = [toCompile]  unless Array.isArray(toCompile)
    bytecode = compileStatements(toCompile)
    for name in bytecode.declaredVariables
      declaredVariables[name] = 1

    merge declaredFunctions, bytecode.declaredFunctions
    bytecode.instructions

  makeLoopTest = (test) ->
    testBytecode = compileExpression(test)
    instructions = testBytecode.preInstructions

    # We know that the expression will always be a string
    negatedCondition = "!(#{testBytecode.expression})"
    loopTest = new If(negatedCondition, [new Break()], [])
    merge instructions, [loopTest]

  instructions = []
  declaredVariables = {}
  declaredFunctions = []
  for statement in statements
    switch statement.type
      when "ExpressionStatement"
        instructions.push subExpression(statement.expression)
      when "VariableDeclaration"
        for declarator in statement.declarations
          name = subExpression(declarator.id)
          declaredVariables[name] = 1
          if declarator.init isnt null
            instructions.push "#{name} = #{subExpression(declarator.init)}"

      when "FunctionDeclaration"
        name = subExpression(statement.id)
        params = (subExpression(param) for param in statement.params)
        body = compileStatements([statement.body])
        declaredFunctions.push new FunctionDefinition(name, params, body)
      when "ReturnStatement"
        value = subExpression(statement.argument)
        instructions.push new Return(value)
      when "BlockStatement"
        merge instructions, subStatements(statement.body)
      when "IfStatement"
        condition = subExpression(statement.test)
        thenClause = subStatements(statement.consequent)
        elseClause = subStatements(statement.alternate)
        instructions.push new If(condition, thenClause, elseClause)
      when "LabeledStatement"
        merge instructions, subStatements(statement.body)
      when "BreakStatement"
        throw new Error("Does not support labelled breaks.")  if statement.label isnt null
        instructions.push new Break()
      when "ContinueStatement"
        throw new Error("Does not support labelled continues.")  if statement.label isnt null
        instructions.push new Continue()
      when "WithStatement"
        object = subExpression(statement.object)
        body = subStatements(statement.body)
        instructions.push new With(object, body)
      when "SwitchStatement"
        # Emulate temporary variable system used in expression compilation
        startPCTemp = "$__temp__[0]"
        currentTemp = val: 1
        value = subExpression(statement.discriminant, currentTemp)
        subInstructions = []
        caseMappings = []
        defaultIndex = null
        for switchCase in statement.cases
          if switchCase.test isnt null
            caseMappings.push
              test: switchCase.test
              index: subInstructions.length
          else
            defaultIndex = subInstructions.length
          merge subInstructions, subStatements(switchCase.consequent)

        makeSwitchCaseIndexChooser = (reverseMappings) ->
          # Returns an array of statements which are effectively an if/else if chain
          if reverseMappings.length == 0
            return ["#{startPCTemp} = #{defaultIndex}"]

          switchCase = reverseMappings.pop()
          caseIndexChooseInstructions = []
          testBytecode = compileExpression(switchCase.test, currentTemp)
          merge caseIndexChooseInstructions, testBytecode.preInstructions

          predicate = "#{value} == #{testBytecode.expression}"
          thenCase = ["#{startPCTemp} = #{switchCase.index}"]
          elseCase = makeSwitchCaseIndexChooser(reverseMappings)
          caseIndexChooseInstructions.push new If(predicate, thenCase, elseCase)
          return caseIndexChooseInstructions

        # Put if/else if's before the switch statement to choose which index to start at
        merge instructions, makeSwitchCaseIndexChooser(caseMappings.reverse())
        # Create the actual switch statement
        instructions.push new Switch(startPCTemp, subInstructions)
      when "WhileStatement"
        loopInstructions = []
        merge loopInstructions, makeLoopTest(statement.test)
        merge loopInstructions, subStatements(statement.body)
        instructions.push new Loop(loopInstructions)
      when "DoWhileStatement"
        bodyInstructions = subStatements(statement.body)
        testInstructions = makeLoopTest(statement.test)
        loopInstructions = testInstructions.concat bodyInstructions
        instructions.push new Loop(loopInstructions, testInstructions.length)
      when "ForStatement"

        # Initializers
        if statement.init isnt null
          if statement.init.type is "VariableDeclaration"
            merge instructions, subStatements(statement.init)
          else
            instructions.push subExpression(statement.init)
        loopInstructions = []

        # Test
        merge loopInstructions, makeLoopTest(statement.test)  if statement.test isnt null

        # Loop body
        merge loopInstructions, subStatements(statement.body)

        # Update
        if statement.update isnt null
          updateBytecode = compileExpression(statement.update)
          merge loopInstructions, updateBytecode.preInstructions
          loopInstructions.push updateBytecode.expression
        instructions.push new Loop(loopInstructions)

  return {
    instructions: instructions
    declaredVariables: Object.keys(declaredVariables)
    declaredFunctions: declaredFunctions
  }

compileExpression = (expression, currentTemp) ->
  subExpression = (toCompile) ->
    compiled = compileExpression(toCompile, currentTemp)
    merge extraInstructions, compiled.preInstructions
    compiled.expression

  statementsFromExpression = (toCompile, destination) ->
    bytecode = compileExpression(toCompile)
    return bytecode.preInstructions.concat ["#{destination} = #{bytecode.expression}"]

  getTempVariable = ->
    # Need currentTemp to be a reference to an object so that
    # incrementing it's value will affect parent/child calls
    "$__temp__[#{currentTemp.val++}]"
  extraInstructions = []
  currentTemp = currentTemp or val: 0
  switch expression.type
    when "ThisExpression"
      compiled = "this"
    when "Literal"
      compiled = expression.raw
    when "Identifier"
      compiled = expression.name
    when "ArrayExpression"
      elements = (subExpression(element) for element in expression.elements)
      compiled = "[#{elements.join(", ")}]"
    when "ObjectExpression"
      properties = []
      for property in expression.properties
        properties.push subExpression(property.key) + ": " + subExpression(property.value)
      compiled = "({" + properties.join(", ") + "})"
    when "FunctionExpression"
      name = (if expression.id isnt null then subExpression(expression.id) else null)
      params = (subExpression(param) for param in expression.params)
      body = compileStatements([expression.body])
      tempVar = getTempVariable()
      extraInstructions.push new FunctionDefinition(name, params, body, tempVar)
      compiled = tempVar
    when "SequenceExpression"
      elements = (subExpression(element) for element in expression.expressions)
      compiled = elements.join(", ")
    when "UnaryExpression"
      operator = expression.operator
      separator = (if operator.length > 1 then " " else "")
      compiled = operator + separator + subExpression(expression.argument)
    when "BinaryExpression", "AssignmentExpression"
      left = subExpression(expression.left)
      right = subExpression(expression.right)
      compiled = "(#{left} #{expression.operator} #{right})"
    when "LogicalExpression"
      tempVar = getTempVariable()
      extraInstructions.push("#{tempVar} = #{subExpression(expression.left)}")

      ifPredicate = tempVar
      ifPredicate = "!" + ifPredicate if expression.operator == "||"
      ifInstructions = statementsFromExpression(expression.right, tempVar)
      extraInstructions.push(new If(ifPredicate, ifInstructions, []))

      compiled = tempVar
    when "UpdateExpression"
      compiled = subExpression(expression.argument)
      if expression.prefix
        compiled = expression.operator + compiled
      else
        compiled += expression.operator
    when "ConditionalExpression"
      tempVar = getTempVariable()

      test = subExpression(expression.test)
      thenInstructions = statementsFromExpression(expression.consequent, tempVar)
      elseInstructions = statementsFromExpression(expression.alternate, tempVar)
      extraInstructions.push(new If(test, thenInstructions, elseInstructions))

      compiled = tempVar
    when "MemberExpression"
      property = subExpression(expression.property)
      object = subExpression(expression.object)
      if expression.computed
        compiled = object + "[#{property}]"
      else
        # Access with dot notation, assuming property is an identifier
        compiled = "#{object}.#{property}"

    # falls through
    when "CallExpression", "NewExpression"
      Cls = (if expression.type is "CallExpression" then FunctionCall else NewObject)
      callee = subExpression(expression.callee)
      args = (subExpression(arg) for arg in expression.arguments)
      tempVar = getTempVariable()
      extraInstructions.push new Cls(callee, args, tempVar)
      compiled = tempVar
  return {
    preInstructions: extraInstructions
    expression: compiled
  }


class ControlBlock
  updateState: ->
  canBreak: -> false
  canContinue: -> false
  canReturn: -> false

class Closure
  constructor: (func, env) ->
    @function = func
    @environment = env

  getInstructions: -> @function.body.instructions

  getEnvironment: (args) ->
    newEnvironmentFrame = {}
    for variable in @function.body.declaredVariables
      newEnvironmentFrame[variable] = undefined
    for param, i in @function.params
      newEnvironmentFrame[param] = args[i]
    return @environment.concat [newEnvironmentFrame]

# Todo: Make defined functions work
class FunctionDefinition
  constructor: (name, params, body, tempVar) ->
    @name = name
    @params = params
    @body = body
    @tempVar = tempVar or null

  updateState: (context) ->
    context.setValue @tempVar, new Closure(this, context.getEnvironment())

class FunctionCall
  constructor: (callee, args, tempVar) ->
    @callee = callee
    @args = args
    @tempVar = tempVar

  updateState: (context) ->
    func = context.eval(@callee)
    if func instanceof Closure
      argValues = (context.eval(arg) for arg in @args)
      context.pushState func.getInstructions(), this, func.getEnvironment(argValues)
    else
      context.eval @tempVar + " = " + @callee + "(" + @args.join(", ") + ")"

  canReturn: -> true

  handleReturn: (context, value) ->
    context.setValue @tempVar, value

class NewObject
  constructor: (callee, args, tempVar) ->
    @callee = callee
    @args = args
    @tempVar = tempVar

class Return
  constructor: (value) -> @value = value

  updateState: (context) ->
    returnValue = context.eval(@value)
    # Find the next scope which can return
    context.popState() until context.getControlObject().canReturn()
    # Store off the object which can return and exit this scope
    returner = context.getControlObject()
    context.popState()
    # Let the object which is returning handle the value
    returner.handleReturn context, returnValue


class If extends ControlBlock
  constructor: (condition, thenClause, elseClause) ->
    @condition = condition
    @thenCase = thenClause
    @elseCase = elseClause

  updateState: (context) ->
    instructions = (if context.eval(@condition) then @thenCase else @elseCase)
    context.pushState instructions, this

class With
  constructor: (object, body) ->
    @object = object
    @body = body


class Switch extends ControlBlock
  constructor: (startPCVar, instructions) ->
    @startPCVar = startPCVar
    @instructions = instructions
  updateState: (context) ->
    startPC = context.eval @startPCVar
    # If startPC is null, there is no default and the discriminant did not
    # match any of the cases.
    if startPC != null
      context.pushState @instructions, this
      context.setPC startPC
  canBreak: -> true

class Break
  updateState: (context) ->
    # Pop off non-loop scopes (eg nested if's)
    context.popState() until context.getControlObject().canBreak()
    # Pop off the loop scope
    context.popState()


class Continue
  updateState: (context) ->
    # Pop off non-loop scopes (eg nested if's)
    context.popState()  until context.getControlObject().canContinue()
    # Move to beginning of loop
    context.setPC 0


class Loop extends ControlBlock
  constructor: (instructions, initialPC) ->
    @instructions = instructions
    @instructions.push new Continue()
    @initialPC = initialPC or 0
  updateState: (context) ->
    context.pushState @instructions, this
    context.setPC(@initialPC)
  canContinue: -> true
  canBreak: -> true

###
Context class, contains the execution context and wrappers to interact
with the various elements of the execution context. An execution context
is comprised of a series of states, accessed in a first-in-first-out order,
or in other words, a stack of states. Each state consists of a list of
instructions, a program counter (pc) which is the index of the next
instruction, a control object, which is the control structure that is
responsible for the current block of instructions, and an environment,
which is an array of dictionaries that represent the lookup chain for
variables in the state.

The 'scope' argument in the constructor should be an instance of a window
object, which has an eval function that can be used to evaluate code in a
sandboxed fashion.
###
class Context
  constructor: (scope) ->
    @scope = scope
    @state = []

  eval: (command) ->
    # Wrap command in 'with' blocks for scoping
    environment = @getEnvironment()
    @scope["$__env__"] = environment
    preWrap = ""
    postWrap = ""
    for i in [0...environment.length]
      preWrap += "with($__env__[#{i}]){"
      postWrap += "}"

    command = preWrap + command + postWrap
    @scope.eval command

  pushState: (instructions, controlObject, environment) ->
    @state.push
      instructions: instructions
      pc: 0
      controlObject: controlObject or null
      environment: environment or @getEnvironment()

  popState: ->
    @state.pop()

  getCurrentState: ->
    if @state.length > 0
      @state[@state.length - 1]
    else
      instructions: []
      pc: 0
      controlObject: null
      environment: []

  hasMoreInstructions: ->
    currentState = @getCurrentState()
    currentState.instructions.length > currentState.pc

  getNextInstruction: ->
    currentState = @getCurrentState()
    instruction = currentState.instructions[currentState.pc]
    currentState.pc++
    instruction

  finishedExecution: ->
    @state.length is 0

  getControlObject: ->
    @getCurrentState().controlObject

  setPC: (newPC) ->
    @getCurrentState().pc = newPC

  getEnvironment: ->
    @getCurrentState().environment

  setValue: (name, value) ->
    @scope["$__result__"] = value
    @eval name + " = $__result__"

class EvaluatorClass
  constructor: ->
    iframe = document.createElement("iframe")
    iframe.height = iframe.width = 0
    iframe.style["visibility"] = "hidden"
    document.body.appendChild(iframe)
    @context = new Context(iframe.contentWindow)
    iframe.contentWindow["$__temp__"] = []


  eval: (string) ->
    @isRunning = true
    ast = esprima.parse(string).body
    bytecode = compileStatements(ast)
    @context.pushState bytecode.instructions
    @execute()

  execute: ->
    instruction = undefined
    lastResult = undefined
    until @context.finishedExecution()
      while @context.hasMoreInstructions()
        return unless @isRunning
        instruction = @context.getNextInstruction()
        if typeof instruction is "string"
          lastResult = @context.eval(instruction)
        else
          instruction.updateState @context
      # Reached end of current set of instructions, pop up to previous set.
      @context.popState()
    lastResult

  pause: ->
    @isRunning = false

  resume: ->
    @isRunning = true
    @execute()

EvaluatorClass::compileStatements = compileStatements
EvaluatorClass::compileExpression = compileExpression
EvaluatorClass::Function = FunctionDefinition
EvaluatorClass::FunctionCall = FunctionCall
EvaluatorClass::NewObject = NewObject
EvaluatorClass::Return = Return
EvaluatorClass::Continue = Continue
EvaluatorClass::Break = Break
EvaluatorClass::If = If
EvaluatorClass::With = With
EvaluatorClass::Switch = Switch
EvaluatorClass::Loop = Loop
window.Evaluator = EvaluatorClass

# Helper functions

unless Array.isArray
  Array.isArray = (vArg) ->
    Object.prototype.toString.call(vArg) is "[object Array]"

merge = (first, second) ->
  # Taken from jQuery
  i = first.length
  l = second.length
  j = 0

  if typeof l is "number"
    for j in [0...l]
      first[i++] = second[j]
  else
    while second[j] != undefined
      first[i++] = second[j++]

  first.length = i
  return first