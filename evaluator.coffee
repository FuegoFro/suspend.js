###
The purpose of this project is to be able to pause arbitrary Javascript
interpretation during any function call and replace the result of that call
with an arbitrary value. The problem we are solving is simulating a blocking
paradigm without locking up the browser. For instance suppose a function
getData needs to fetch JSON from a remote server and you want the interaction
to look like:
  data = getData();
Normally the only way to do this is to have the ajax request performed in
getData to be synchronous so that the getData call doesn't return until the
data has been fetched. However, a synchronous request will lock up the browser,
preventing the user from interacting with the page at all and creating a poor
experience. One solution is to have an interpreter that can pause execution at
any time and pick up execution from that same point later. That way getData
could pause interpretation, make an asynchronous request, and allow the
function to return, freeing up the Javascript thread. When the data gets back
from the server, execution can be resumed and the recieved data can be dropped
in as the result of the most recent function call. In this way, we allow the
execution of 'blocking' code without blocking the browser.
###

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

###
This takes in an array of statement nodes produced by Esprima. These get
compiled to a series of bytecode instructions that are in a format that is
easier to interpret. These instructions are either strings, which are safe
to be 'eval'ed directly, or instances of special classes defined below
which indicated changes in control flow or environment. More detail on how
these classes are used can be found in description above the ControlBlock class.

This function returns an object with three keys:
  instructions: An array of compiled bytecode statements, as described above.
  declaredVariables: A unique array (no duplicates) of strings that are the
    variables that were declared with 'var' in this scope (including
    sub-blocks, such as a nested 'if').
  declaredFunctions: An array of FunctionDefinition objects that correspond
    to functions declared in this scope (eg 'function foo() {}').
###
compileStatements = (statements) ->
  subExpression = (toCompile, currentTemp) ->
    # Compiles an expression, merging the resulting preInstructions into the
    # current list of instructions, returning the string of the expression.
    return null if toCompile is null
    compiled = compileExpression(toCompile, currentTemp)
    merge instructions, compiled.preInstructions
    compiled.expression

  subStatements = (toCompile) ->
    # Compiles a statement or array of statements, merging the declared
    # variables and functions into the current sets of declared variables and
    # functions, returning the array of compiled instructions.
    return [] if toCompile is null
    toCompile = [toCompile]  unless Array.isArray(toCompile)
    bytecode = compileStatements(toCompile)
    for name in bytecode.declaredVariables
      declaredVariables[name] = 1

    merge declaredFunctions, bytecode.declaredFunctions
    bytecode.instructions

  makeLoopTest = (test) ->
    # Creates an array of instructions that will execute a 'break' if the
    # expression given in the test argument evaluates to false.
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
      when "ThrowStatement"
        instructions.push new Throw(subExpression(statement.argument))
      when "WithStatement"
        object = subExpression(statement.object)
        body = subStatements(statement.body)
        instructions.push new With(object, body)
      when "SwitchStatement"
        # Emulate temporary variable system used in expression compilation.
        # This breaks the encapsulation a bit but until something else in
        # compileStatements needs to use temporary variables this is probably
        # the cleanest solution.
        startPCTemp = "$__temp__[0]"
        currentTemp = val: 1
        value = subExpression(statement.discriminant, currentTemp)
        subInstructions = []

        # First unroll/preprocess the cases to be in a more useful format.
        # This gives us two useful lists. First a single list of instructions
        # that make up the body of the switch (imagine just removing all 'case'
        # and 'default' labels). Second a mapping between uncompiled test
        # expressions and corresponding starting program counter (the index
        # into the switch body instructions that a given case corresponds to).
        # This also extracts a starting program counter (index into the switch
        # body) for the default case, if one exists.
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

        # The next step is to make an if/else-if chain that one at a time
        # compares the determinant of the switch is equal to the value of the
        # case statement (in the order that the case statements were written)
        # and if so sets a temporary variable to the starting program counter
        # (again, the index into the switch body instructions) that the matched
        # case corresponds to. If none of the values match the discriminant,
        # the default index is used. This recursive helper function makes this
        # if/else-if chain out of if/else blocks.
        makeSwitchCaseIndexChooser = (reverseMappings) ->
          # Returns an array of statements which are effectively an if/else-if
          # chain. Mappings are in reverse order purely so that I can use 'pop'
          # to get the next mapping.
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

        # Put the if/else-if's before the switch statement.
        merge instructions, makeSwitchCaseIndexChooser(caseMappings.reverse())
        # Create the actual switch statement, letting it know where to look to
        # find the starting index.
        instructions.push new Switch(startPCTemp, subInstructions)
      when "TryStatement"
        tryBlock = subStatements statement.block
        catchBlock = catchVariable = finalBlock = null
        unless statement.handlers.length is 0
          # Assuming single handler, no guarding
          catchBlock = subStatements statement.handlers[0].body
          catchVariable = subExpression statement.handlers[0].param
        finalBlock = subStatements statement.finalizer

        instructions.push new Try(
          tryBlock
          catchBlock
          catchVariable
          finalBlock
        )
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

###
This function takes in an AST node representing a Javascript expression as
output by Esprima.
The second optional argument is used recursively by the function as a
mechanisim to ensure that temporary variables used do not collide. This is
only needed within expression not compilation (and not across different
statements) is because we only make assumptions about the existence of
temporary variables within a single statement. After that the temporary
variables are not needed and it is okay for the next statement to overwrite
them. This should not be used by other functions (except for one special
case when compiling 'switch' blocks).

It returns an object with two keys:
  preInstructions: An array of any statements/instructions that will need
    to be executed before this expression. This allows us to do things
    like pull out function calls to be their own statements. For more on
    the format of these statements, see the description of compileStatements.
  expression: A string that can be 'eval'ed to get the value of the
    expression after any instructions in preInstructions have been run.
###
compileExpression = (expression, currentTemp=val: 0) ->

  subExpression = (toCompile) ->
    # Compiles an expression, merging the preInstructions from that compiliation
    # into the current list of preInstructions.
    compiled = compileExpression(toCompile, currentTemp)
    merge extraInstructions, compiled.preInstructions
    compiled.expression

  statementsFromExpression = (toCompile, destination) ->
    # Returns an array of statements that when run will put the value of the
    # expression in the destination location.
    bytecode = compileExpression(toCompile)
    return bytecode.preInstructions.concat ["#{destination} = #{bytecode.expression}"]

  getTempVariable = ->
    # Need currentTemp to be a reference to an object so that
    # incrementing it's value will affect parent/child calls
    "$__temp__[#{currentTemp.val++}]"

  extraInstructions = []
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
    when "CallExpression", "NewExpression"
      if expression.type is "CallExpression"
        Cls = FunctionCall
        # See the description on the FunctionCall constructor for an explanation
        # of the callee value.
        if expression.callee.type is "MemberExpression"
          thisValue = subExpression(expression.callee.object)
          functionName = subExpression(expression.callee.property)
          if expression.callee.property.type is "Identifier" and not expression.callee.computed
            # This of the form a.b and we want to rewrite to be a["b"] so we
            # need to wrap in quotes
            functionName = "'#{functionName}'"
        else
          thisValue = null
          functionName = subExpression(expression.callee)
        callee = [thisValue, functionName]
      else
        Cls = NewObject
        callee = subExpression(expression.callee)
      args = (subExpression(arg) for arg in expression.arguments)
      tempVar = getTempVariable()
      extraInstructions.push new Cls(callee, args, tempVar)
      compiled = tempVar
  return {
    preInstructions: extraInstructions
    expression: compiled
  }

###
Control Objects
These classes each have an updateState method that, given a Context object,
will perform any actions and state changes necessary to carry out its job.

Control Blocks
These are the classes that can be provided as a controlObject for a state (see
Context) and are a subset of control objects. They should implement the set of
predicates found on ControlBlock unless it is ensured that a certain action will
never reach a given control block (a function call should never have a bare
break or continue in it). Returning true for canReturn or canCatch implies that
the control object has a handleReturn or handleError function respectively.
###

class ControlBlock
  updateState: ->
  canBreak: -> false
  canContinue: -> false
  canReturn: -> false
  canCatch: -> false

class Closure
  constructor: (@function, @environment) ->
  getInstructions: ->
    @function.body.declaredFunctions.concat @function.body.instructions

  getEnvironment: (args) ->
    newEnvironmentFrame = $__temp__: []
    for variable in @function.body.declaredVariables
      newEnvironmentFrame[variable] = undefined
    for func in @function.body.declaredFunctions
      newEnvironmentFrame[func.name] = undefined
    for param, i in @function.params
      newEnvironmentFrame[param] = args[i]
    Object.defineProperty args, 'callee',
      value: this
      enumerable: false
    newEnvironmentFrame.arguments = args
    if @function.tempVar isnt null and @function.name isnt null
      # this is not a declared function but it has a name, need to define
      # the name of the function to be the function itself
      newEnvironmentFrame[@function.name] = this

    return @environment.concat [newEnvironmentFrame]

class FunctionDefinition
  constructor: (@name, @params, @body, @tempVar=null) ->
  updateState: (context) ->
    targetLocation = @tempVar or @name
    closure = new Closure(this, context.getEnvironment())
    closure.prototype = new context.scope.Object()
    closure.prototype.constructor = closure
    context.setValue targetLocation, closure

class FunctionCall
  # Callee is an array of length two. The first element is a string that points
  # to the 'this' parameter (the dictionary that the function is stored on) and
  # the second element is a string that contains the value of the property with
  # the function. If the first element is null, it is assumed that the second
  # parameter is the fully qualified path to the function. Below are three
  # examples mapping function calls to this array format:
  #
  # a.b()  -> ["a",  "'b'"]
  # a[b]() -> ["a",  "b"  ]
  # f()    -> [null, "f"  ]
  constructor: (@callee, @args, @tempVar) ->
  updateState: (context) ->
    if @callee[0] is null
      functionLocation = @callee[1]
    else
      functionLocation = "#{@callee[0]}[#{@callee[1]}]"

    func = context.eval functionLocation
    if func instanceof Closure
      thisObject = if @callee[0] is null then null else context.eval @callee[0]
      argValues = (context.eval(arg) for arg in @args)
      context.pushState func.getInstructions(), this, func.getEnvironment(argValues), thisObject
    else
      context.eval "#{@tempVar} = #{functionLocation}(#{@args.join(", ")})"

  canReturn: -> true
  canCatch: -> false

  handleReturn: (context, value) ->
    context.setValue @tempVar, value

class NewObject
  # Unlike in the FunctionCall class, callee is just a string that points to
  # location of the function that is the class for the object.
  constructor: (@callee, @args, @tempVar) ->
  updateState: (context) ->
    func = context.eval @callee
    if func instanceof Closure
      instance = Object.create(func.prototype)
      context.setValue @tempVar, instance
      argValues = (context.eval(arg) for arg in @args)
      context.pushState func.getInstructions(), this, func.getEnvironment(argValues), instance
    else
      context.eval "#{@tempVar} = new #{@callee}(#{@args.join(", ")})"

  canCatch: -> false
  canReturn: -> true
  handleReturn: ->

class Return
  constructor: (@value) ->
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
  constructor: (@condition, @thenCase, @elseCase) ->

  updateState: (context) ->
    instructions = (if context.eval(@condition) then @thenCase else @elseCase)
    context.pushState instructions, this

class With extends ControlBlock
  constructor: (@object, @body) ->
  updateState: (context) ->
    environmentFrame = context.eval @object
    newEnvironment = context.getEnvironment().concat [environmentFrame]
    context.pushState @body, this, newEnvironment

class Switch extends ControlBlock
  constructor: (@startPCVar, @instructions) ->
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

class Throw
  # Computed == false means that the error is a string that should be evaluated
  # to obtain the actual error object.
  # Computed == true means that the error is the actual object that was thrown.
  constructor: (@error, @evaluated=false) ->
  updateState: (context) ->
    if @evaluated
      errorObject = @error
    else
      errorObject = context.eval @error

    while context.hasMoreStates() and not context.getControlObject().canCatch()
      context.popState()
    if context.hasMoreStates()
      # Store off reference to current control object (probably a try) and
      # remove the current set of instructions from the state stack (probably
      # the try block)
      controlObject = context.getControlObject()
      context.popState()
      # Allow the controlling object to update the state
      controlObject.handleError(context, errorObject)
    else
      context.done(errorObject, true)

class Try extends ControlBlock
  constructor: (@tryBlock, @catchBlock, @catchVariable, @finallyBlock) ->
    # Keep track of which block we're executing.
    @_isInTry = false
    @_isInCatch = false
  updateState: (context) ->
    # We're entering the try block.
    @_isInTry = true
    # Create a unique object that we can compare against.
    @_endOfBlock = {}
    # This is a hack to transfer control back to this object if the try block
    # executes without throwing any exceptions. This allows us to run the
    # finally block.
    instructions = @tryBlock.concat [new Throw(@_endOfBlock, true)]
    context.pushState instructions, this
  # We only want to catch exceptions that occur in the try and catch blocks,
  # not the finally blocks
  canCatch: -> @_isInTry or @_isInCatch
  handleError: (context, error) ->
    if error is @_endOfBlock
      @_isInTry = @_isInCatch = false
      # The error thrown is the special object we created to mark the end of
      # the try or catch block. This means we got through the block without any
      # exceptions. If we have a finally block, we should add that state now.
      if @finallyBlock.length > 0
        context.pushState @finallyBlock, this
    # Actual error
    else if @_isInTry and @catchBlock isnt null
      # Exception thrown in try, continue on to catch
      @_isInTry = false
      @_isInCatch = true
      # Want to transfer control back to this object if no exceptions are
      # thrown in catch in order to run the finally block if it exists.
      instructions = @catchBlock.concat [new Throw(@_endOfBlock, true)]
      newEnvironmentFrame = {}
      newEnvironmentFrame[@catchVariable] = error
      newEnvironment = context.getEnvironment().concat [newEnvironmentFrame]
      context.pushState instructions, this, newEnvironment
    else
      # There is no catch block or exception thrown in catch block, run the
      # finally block and re-throw the error when done.
      @_isInTry = @_isInCatch = false
      instructions = @finallyBlock.concat [new Throw(error, true)]
      context.pushState instructions, this

class Loop extends ControlBlock
  constructor: (@instructions, @initialPC=0) ->
    @instructions.push new Continue()
  updateState: (context) ->
    context.pushState @instructions, this
    context.setPC(@initialPC)
  canContinue: -> true
  canBreak: -> true

###
The Context class contains the execution context and wrappers to interact with
the various elements of the execution context. The 'scope' argument in the
constructor should be an instance of a window object, which has an eval
function that can be used to evaluate code in a sandboxed fashion. The
onComplete argument should be a callback as given to Evaluator::eval

An execution context is comprised of a stack (an array) of states, accessed in
a first-in-last-out order. A state is an object with five keys:
  instructions: An array of compiled instructions. Each instruction is either a
    string which can be 'eval'ed or an instance of one of the control classes
    above. See the description just above the ControlBlock class for more details.
  pc: An index into the instructions array, indicating which instruction will be
    run next.
  controlObject: An instance of one of the control classes above. These are used
    to figure out how to perform control flow operations that may span multiple
    scopes, such as a return or continue. See the description just above the
    ControlBlock class for more details.
  environment: An array of objects (mappings) that represents the lookup chain
    for variables in the state. The first mapping in the array will be the last
    one checked when looking up a variable name.
  thisObject: An aribrary object that is the current 'this' binding.
###
class Context
  constructor: (@scope, @onComplete) ->
    @stateStack = []
    @isDone = false

  eval: (command) ->
    preWrap = ""
    postWrap = ""

    # Wrap command in 'with' blocks for scoping
    environment = @getEnvironment()
    @scope["$__env__"] = environment
    for i in [0...environment.length]
      preWrap += "with($__env__[#{i}]){"
      postWrap = "}" + postWrap

    # Wrap in a function call to set 'this' object
    thisObject = @getThisObject()
    if thisObject isnt null
      @scope["$__this__"] = thisObject
      preWrap = "(function () {#{preWrap}return "
      postWrap += "}).call($__this__)"

    command = preWrap + command + postWrap
    try
      @scope.eval command
    catch e
      if e instanceof @scope.Error or e instanceof Error
        # Need to get rid of stack trace, but some browsers don't let you modify
        # the .stack field of error objects, so we are wrapping this object.
        e = Object.create(e, stack: {value: null})
      @pushState [new Throw(e, true)]

  done: (value, isError) ->
    unless @isDone
      @isDone = true
      @onComplete?(value, isError)

  pushState: (instructions,
              controlObject=new ControlBlock(),
              environment=@getEnvironment(),
              thisObject=@getThisObject()) ->
    @stateStack.push
      instructions: instructions
      pc: 0
      controlObject: controlObject
      environment: environment
      thisObject: thisObject

  popState: ->
    @stateStack.pop()

  getCurrentState: ->
    if @stateStack.length > 0
      @stateStack[@stateStack.length - 1]
    else
      instructions: []
      pc: 0
      controlObject: null
      environment: [$__temp__: []]
      thisObject: null

  stateHasMoreInstructions: ->
    currentState = @getCurrentState()
    currentState.instructions.length > currentState.pc

  getNextInstruction: ->
    currentState = @getCurrentState()
    instruction = currentState.instructions[currentState.pc]
    currentState.pc++
    instruction

  getPreviousInstruction: ->
    currentState = @getCurrentState()
    currentState.instructions[currentState.pc - 1]

  hasMoreStates: ->
    @stateStack.length isnt 0

  getControlObject: ->
    @getCurrentState().controlObject

  setPC: (newPC) ->
    @getCurrentState().pc = newPC

  getEnvironment: ->
    @getCurrentState().environment

  setValue: (name, value) ->
    @scope["$__result__"] = value
    @eval name + " = $__result__"

  getThisObject: ->
    @getCurrentState().thisObject

class Evaluator
  constructor: ->
    iframe = document.createElement("iframe")
    iframe.height = iframe.width = 0
    iframe.style["visibility"] = "hidden"
    document.body.appendChild(iframe)
    @scope = iframe.contentWindow

  ###
  Takes in the string of the code to be evaluated and a callback that has two
  parameters. If the evaluation does not produce an error, the first argument
  to the callback will be the result of the evaluation as expected normally
  (an arbitrary Javascript value), and the second value will be the boolean
  `false`. If an uncaught error occurs while evaluating the code, the first
  argument will be the error object (again an arbitrary Javascript value) and
  the second will be the boolean `true`.
  ###
  eval: (string, onComplete) ->
    @isRunning = true
    ast = esprima.parse(string).body
    bytecode = compileStatements(ast)
    instructions = bytecode.declaredFunctions.concat bytecode.instructions
    @context = new Context(@scope, onComplete)
    @context.pushState instructions
    @_execute()

  _execute: ->
    lastResult = undefined
    while @context.hasMoreStates()
      while @context.stateHasMoreInstructions()
        return unless @isRunning
        instruction = @context.getNextInstruction()
        if typeof instruction is "string"
          lastResult = @context.eval(instruction)
        else
          instruction.updateState @context
      # Reached end of current set of instructions, pop up to previous set.
      @context.popState()
    @context.done(lastResult, false)

  pause: ->
    @isRunning = false
    @context

  resume: (context, returnValue) ->
    unless context
      throw new Error("Resuming evaluation requires a context as returned by pause.")
    else unless context instanceof Context
      throw new Error("Invalid context given to resume.")

    if returnValue
      lastInstruction = context.getPreviousInstruction()
      lastInstruction.handleReturn(context, returnValue)
    @context = context
    @isRunning = true
    @_execute()

Evaluator.compileStatements = compileStatements
Evaluator.compileExpression = compileExpression
Evaluator.Function = FunctionDefinition
Evaluator.FunctionCall = FunctionCall
Evaluator.NewObject = NewObject
Evaluator.Return = Return
Evaluator.Continue = Continue
Evaluator.Break = Break
Evaluator.Throw = Throw
Evaluator.If = If
Evaluator.With = With
Evaluator.Switch = Switch
Evaluator.Try = Try
Evaluator.Loop = Loop
window.Evaluator = Evaluator

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