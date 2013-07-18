/* global Evaluator:true, esprima */
/* jshint evil:true, sub:true */
/*
*
* The evaluation takes place in 2 main steps:
*   = Walk the ast, convert into 'bytecode'
*     ~ Function definitions, function calls, control structures, returns are
*       special cased into corresponding objects.
*     ~ Pull out all var's to the front of the scope
*       - At function definition, when we create it's environment, map all of
*         the var's in the scope to undefined.
*     ~ All others are strings to be eval'd.
*     ~ Unpack inline or chained method calls and function definitions,
*       introducing temporary variables as necessary.
*   = Interpret the 'bytecode'
*     ~ Keep track of environment.
*       - When eval'ing statements, wrap the text being eval'd in 'with' blocks
*       - On function invocation, use the closure as the environment.
*       - On function return, use the previous environment (keep a stack)
*     ~ Keep track of program counter
*       - Handle all control flow, function invocation, function return
*     ~ Be able to stop and restart, remembering state
*       - On stop, save off all relevant state (call stack, program counter)
*         and return from evaluation.
*       - On resume, pick up where left off
*     ~ Handle promises returned by functions.
*
* */

Evaluator = (function () {
  function compileStatements(statements) {
    function subExpression(toCompile) {
      if (toCompile === null) {
        return null;
      }
      var compiled = compileExpression(toCompile);
      $.merge(instructions, compiled.preInstructions);
      return compiled.expression;
    }

    function subStatements(toCompile) {
      if (toCompile === null) {
        return [];
      }
      if (!$.isArray(toCompile)) {
        toCompile = [toCompile];
      }
      var bytecode = compileStatements(toCompile);
      $.each(bytecode.declaredVariables, function (i, name) {
        declaredVariables[name] = 1;
      });
      $.merge(declaredFunctions, bytecode.declaredFunctions);
      return bytecode.instructions;
    }

    function makeLoopTest(test) {
      testBytecode = compileExpression(test);
      var instructions = testBytecode.preInstructions;
      // We know that the expression will always be a string
      negatedCondition = '!(' + testBytecode.expression + ')';
      var loopTest = new If(negatedCondition, [new Break()], []);
      return $.merge(instructions, [loopTest]);
    }

    var instructions = [];
    var declaredVariables = {};
    var declaredFunctions = [];
    var body, value;
    var loopInstructions, testBytecode, negatedCondition;

    $.each(statements, function (i, statement) {
      switch (statement.type) {
      case 'EmptyStatement':
        break;
      case 'ExpressionStatement':
        instructions.push(subExpression(statement.expression));
        break;
      case 'VariableDeclaration':
        $.each(statement.declarations, function (i, declarator) {
          var name = subExpression(declarator.id);
          declaredVariables[name] = 1;
          if (declarator.init !== null) {
            instructions.push(name + ' = ' + subExpression(declarator.init));
          }
        });
        break;
      case 'FunctionDeclaration':
        var name = subExpression(statement.id);
        var params = $.map(statement.params, subExpression);
        body = compileStatements([statement.body]);
        declaredFunctions.push(new FunctionDefinition(name, params, body));
        break;
      case 'ReturnStatement':
        value = subExpression(statement.argument);
        instructions.push(new Return(value));
        break;
      case 'BlockStatement':
        $.merge(instructions, subStatements(statement.body));
        break;
      case 'IfStatement':
        var condition = subExpression(statement.test);
        var thenClause = subStatements(statement.consequent);
        var elseClause = subStatements(statement.alternate);
        instructions.push(new If(condition, thenClause, elseClause));
        break;
      case 'LabeledStatement':
        $.merge(instructions, subStatements(statement.body));
        break;
      case 'BreakStatement':
        if (statement.label !== null) {
          throw new Error('Does not support labelled breaks.');
        }
        instructions.push(new Break());
        break;
      case 'ContinueStatement':
        if (statement.label !== null) {
          throw new Error('Does not support labelled continues.');
        }
        instructions.push(new Continue());
        break;
      case 'WithStatement':
        var object = subExpression(statement.object);
        body = subStatements(statement.body);
        instructions.push(new With(object, body));
        break;
      case 'SwitchStatement':
        value = subExpression(statement.discriminant);
        var subInstructions = [];
        var caseMappings = [];
        var defaultIndex = null;
        $.each(statement.cases, function (i, switchCase) {
          // Do not call subExpression, do not want instructions merged into
          // current scope (before switch)
          if (switchCase.test !== null) {
            var testBytecode = compileExpression(switchCase.test);
            var testInstructions = $.merge(
              testBytecode.preInstructions,
              [new Return(testBytecode.expression)]
            );
            var body = {
              declaredFunctions: [],
              declaredVariables: [],
              instructions: testInstructions
            };
            caseMappings.push({
              test: new FunctionDefinition(null, [], body),
              index: subInstructions.length
            });
          } else {
            defaultIndex = subInstructions.length;
          }

          $.merge(subInstructions, subStatements(switchCase.consequent));
        });
        instructions.push(new Switch(value, subInstructions, caseMappings, defaultIndex));
        break;
      case 'WhileStatement':
        loopInstructions = [];
        $.merge(loopInstructions, makeLoopTest(statement.test));
        $.merge(loopInstructions, subStatements(statement.body));
        instructions.push(new Loop(loopInstructions));
        break;
      case 'DoWhileStatement':
        loopInstructions = [];
        $.merge(loopInstructions, subStatements(statement.body));
        $.merge(loopInstructions, makeLoopTest(statement.test));
        instructions.push(new Loop(loopInstructions));
        break;
      case 'ForStatement':
        // Initializers
        if (statement.init !== null) {
          if (statement.init.type === 'VariableDeclaration') {
            $.merge(instructions, subStatements(statement.init));
          } else {
            instructions.push(subExpression(statement.init));
          }
        }

        loopInstructions = [];
        // Test
        if (statement.test !== null) {
          $.merge(loopInstructions, makeLoopTest(statement.test));
        }
        // Loop body
        $.merge(loopInstructions, subStatements(statement.body));
        // Update
        if (statement.update !== null) {
          var updateBytecode = compileExpression(statement.update);
          $.merge(loopInstructions, updateBytecode.preInstructions);
          loopInstructions.push(updateBytecode.expression);
        }

        instructions.push(new Loop(loopInstructions));
        break;
      }
    });
    return {
      instructions: instructions,
      declaredVariables: Object.keys(declaredVariables),
      declaredFunctions: declaredFunctions
    };
  }

  function compileExpression(expression, currentTemp) {
    var compiled, elements, tempVar;
    var extraInstructions = [];

    function subExpression(toCompile) {
      var compiled = compileExpression(toCompile, currentTemp);
      $.merge(extraInstructions, compiled.preInstructions);
      return compiled.expression;
    }

    // Need this to be a reference so that
    // incrementing it will affect parent calls
    currentTemp = currentTemp || {val: 0};
    function getTempVariable() {
      return '$__temp__[' + (currentTemp.val++) + ']';
    }

    switch (expression.type) {
    case 'ThisExpression':
      compiled = 'this';
      break;
    case 'Literal':
      compiled = expression.raw;
      break;
    case 'Identifier':
      compiled = expression.name;
      break;
    case 'ArrayExpression':
      elements = $.map(expression.elements, subExpression);
      compiled = '[' + elements.join(', ') + ']';
      break;
    case 'ObjectExpression':
      compiled = '({';
      var properties = [];
      $.each(expression.properties, function (i, property) {
        properties.push(subExpression(property.key) + ': ' + subExpression(property.value));
      });
      compiled += properties.join(', ') + '})';
      break;
    case 'FunctionExpression':
      var name = expression.id !== null ? subExpression(expression.id) : null;
      var params = $.map(expression.params, subExpression);
      var body = compileStatements([expression.body]);
      tempVar = getTempVariable();
      extraInstructions.push(new FunctionDefinition(name, params, body, tempVar));
      compiled = tempVar;
      break;
    case 'SequenceExpression':
      elements = $.map(expression.expressions, subExpression);
      compiled = elements.join(', ');
      break;
    case 'UnaryExpression':
      var operator = expression.operator;
      var separator = operator.length > 1 ? ' ' : '';
      compiled = operator + separator + subExpression(expression.argument);
      break;
    case 'BinaryExpression':
      /* falls through */
    case 'AssignmentExpression':
      /* falls through */
    case 'LogicalExpression':
      var left = subExpression(expression.left);
      var right = subExpression(expression.right);
      compiled = '(' + left + ' ' + expression.operator + ' ' + right + ')';
      break;
    case 'UpdateExpression':
      compiled = subExpression(expression.argument);
      if (expression.prefix) {
        compiled = expression.operator + compiled;
      } else {
        compiled += expression.operator;
      }
      break;
    case 'ConditionalExpression':
      var test = subExpression(expression.test);
      var consequent = subExpression(expression.consequent);
      var alternate = subExpression(expression.alternate);
      compiled = test + ' ? ' + consequent + ' : ' + alternate;
      break;
    case 'MemberExpression':
      var property = subExpression(expression.property);
      var object = subExpression(expression.object);
      if (expression.computed) {
        compiled = object + '[' + property + ']';
      } else {
        // Access with dot notation, assuming property is an identifier
        compiled = object + '.' + property;
      }
      break;
    case 'CallExpression':
      /* falls through */
    case 'NewExpression':
      var Cls = expression.type === 'CallExpression' ? FunctionCall : NewObject;
      var callee = subExpression(expression.callee);
      var args = $.map(expression.arguments, subExpression);
      tempVar = getTempVariable();
      extraInstructions.push(new Cls(callee, args, tempVar));
      compiled = tempVar;
      break;
    }
    return {
      preInstructions: extraInstructions,
      expression: compiled
    };
  }

  function ControlObject() {}
  ControlObject.prototype.updateState = function () {};
  ControlObject.prototype.canBreak = function () {return false;};
  ControlObject.prototype.canContinue = function () {return false;};
  ControlObject.prototype.canReturn = function () {return false;};

  function setParent(Clazz, SuperClazz) {
    Clazz.prototype = new SuperClazz();
    Clazz.prototype.constructor = function () {
      SuperClazz.apply(this, arguments);
      Clazz.apply(this, arguments);
    };
  }

  function Closure(func, env) {
    this.function = func;
    this.environment = env;
  }
  Closure.prototype.getInstructions = function () {
    // Todo: Make defined functions work
    return this.function.body.instructions;
  };
  Closure.prototype.getEnvironment = function (args) {
    var newEnvironmentFrame = {};
    $.each(this.function.body.declaredVariables, function (i, variable) {
      newEnvironmentFrame[variable] = undefined;
    });
    $.each(this.function.params, function (i, param) {
      newEnvironmentFrame[param] = args[i];
    });
    return this.environment.concat([newEnvironmentFrame]);
  };

  function FunctionDefinition(name, params, body, tempVar) {
    this.name = name;
    this.params = params;
    this.body = body;
    this.tempVar = tempVar || null;
  }
  FunctionDefinition.prototype.updateState = function (context) {
    context.setValue(this.tempVar, new Closure(this, context.getEnvironment()));
  };

  function FunctionCall(callee, args, tempVar) {
    this.callee = callee;
    this.args = args;
    this.tempVar = tempVar;
  }
  FunctionCall.prototype.updateState = function (context) {
    var func = context.eval(this.callee);
    if (func instanceof Closure) {
      var argValues = $.map(this.args, $.proxy(context.eval, context));
      context.pushState(func.getInstructions(), this, func.getEnvironment(argValues));
    } else {
      context.eval(this.tempVar + ' = ' + this.callee + '(' + this.args.join(', ') + ')');
    }
  };
  FunctionCall.prototype.canReturn = function () {return true;};
  FunctionCall.prototype.handleReturn = function (context, value) {
    context.setValue(this.tempVar, value);
  };

  function NewObject(callee, args, tempVar) {
    this.callee = callee;
    this.args = args;
    this.tempVar = tempVar;
  }

  function Return(value) {
    this.value = value;
  }
  Return.prototype.updateState = function (context) {
    var returnValue = context.eval(this.value);
    // Find the next scope which can return
    while(!context.getControlObject().canReturn()) {
      context.popState();
    }
    // Store of the object which can return and exit this scope
    var returner = context.getControlObject();
    context.popState();
    // Let the object which is returning handle the value
    returner.handleReturn(context, returnValue);
  };

  function If(condition, thenClause, elseClause) {
    this.condition = condition;
    this.then = thenClause;
    this.else = elseClause;
  }
  setParent(If, ControlObject);
  If.prototype.updateState = function (context) {
    var instructions = context.eval(this.condition) ? this.then : this.else;
    context.pushState(instructions, this);
  };

  function With(object, body) {
    this.object = object;
    this.body = body;
  }

  function Switch(value, instructions, cases, defaultIndex) {
    this.value = value;
    this.instructions = instructions;
    this.cases = cases;
    this.default = defaultIndex || null;
  }

  function Break() {}
  Break.prototype.updateState = function (context) {
    // Pop off non-loop scopes (eg nested if's)
    while (!context.getControlObject().canBreak()) {
      context.popState();
    }
    // Pop off the loop scope
    context.popState();
  };
  function Continue() {}
  Continue.prototype.updateState = function (context) {
    // Pop off non-loop scopes (eg nested if's)
    while (!context.getControlObject().canContinue()) {
      context.popState();
    }
    // Move to beginning of loop
    context.setPC(0);
  };

  function Loop(instructions) {
    this.instructions = instructions;
    this.instructions.push(new Continue());
  }
  setParent(Loop, ControlObject);
  Loop.prototype.updateState = function (context) {
    context.pushState(this.instructions, this);
  };
  Loop.prototype.canContinue = function () {return true;};
  Loop.prototype.canBreak = function () {return true;};


  /**
   * Context class, contains the execution context and wrappers to interact
   * with the various elements of the execution context. An execution context
   * is comprised of a series of states, accessed in a first-in-first-out order,
   * or in other words, a stack of states. Each state consists of a list of
   * instructions, a program counter (pc) which is the index of the next
   * instruction, a control object, which is the control structure that is
   * responsible for the current block of instructions, and an environment,
   * which is an array of dictionaries that represent the lookup chain for
   * variables in the state.
   *
   * The 'scope' argument in the constructor should be an instance of a window
   * object, which has an eval function that can be used to evaluate code in a
   * sandboxed fashion.
   * */
  function Context(scope) {
    this.scope = scope;
    this.state = [];
  }

  Context.prototype.eval = function (command) {
    // Wrap command in 'with' blocks for scoping
    var environment = this.getEnvironment();
    this.scope['$__env__'] = environment;
    var preWrap = '';
    var postWrap = '';
    $.each(environment, function (i) {
      preWrap += 'with($__env__[' + i + ']){';
      postWrap += '}';
    });
    command = preWrap + command + postWrap;
    return this.scope.eval(command);
  };

  Context.prototype.pushState = function (instructions, controlObject, environment) {
    this.state.push({
      instructions: instructions,
      pc: 0,
      controlObject: controlObject || null,
      environment: environment || []
    });
  };

  Context.prototype.popState = function () {
    this.state.pop();
  };

  Context.prototype.getCurrentState = function () {
    if (this.state.length > 0) {
      return this.state[this.state.length - 1];
    } else {
      return {instructions: [], pc: 0, controlObject: null, environment: []};
    }
  };

  Context.prototype.hasMoreInstructions = function () {
    var currentState = this.getCurrentState();
    return currentState.instructions.length > currentState.pc;
  };

  Context.prototype.getNextInstruction = function () {
    var currentState = this.getCurrentState();
    var instruction = currentState.instructions[currentState.pc];
    currentState.pc++;
    return instruction;
  };

  Context.prototype.finishedExecution = function () {
    return this.state.length === 0;
  };

  Context.prototype.getControlObject = function () {
    return this.getCurrentState().controlObject;
  };

  Context.prototype.setPC = function (newPC) {
    this.getCurrentState().pc = newPC;
  };

  Context.prototype.getEnvironment = function () {
    return this.getCurrentState().environment;
  };

  Context.prototype.setValue = function (name, value) {
    this.scope['$__result__'] = value;
    this.eval(name + ' = $__result__');
  };

  function EvaluatorClass() {
    var iframe = $('<iframe width="0" height="0"></iframe>')
      .css({visibility : 'hidden'})
      .appendTo('body')
      .get(0);
    this.context = new Context(iframe.contentWindow);
    iframe.contentWindow['$__temp__'] = [];
  }

  EvaluatorClass.prototype.eval = function (string) {
    this.isRunning = true;
    var ast = esprima.parse(string).body;
    var bytecode = compileStatements(ast);
    this.context.pushState(bytecode.instructions);

    return this.execute();
  };

  EvaluatorClass.prototype.execute = function () {
    var instruction, lastResult;
    while (!this.context.finishedExecution()) {
      while (this.context.hasMoreInstructions()) {
        if (!this.isRunning) {
          return undefined;
        }
        instruction = this.context.getNextInstruction();
        if (typeof instruction === 'string') {
          lastResult = this.context.eval(instruction);
        } else {
          instruction.updateState(this.context);
        }
      }
      this.context.popState();
    }
    return lastResult;
  };

  EvaluatorClass.prototype.pause = function () {
    this.isRunning = false;
  };

  EvaluatorClass.prototype.resume = function () {
    this.isRunning = true;
    console.log('In Resume');
    this.execute();
  };

  EvaluatorClass.prototype.compileStatements = compileStatements;
  EvaluatorClass.prototype.compileExpression = compileExpression;
  EvaluatorClass.prototype.Function = FunctionDefinition;
  EvaluatorClass.prototype.FunctionCall = FunctionCall;
  EvaluatorClass.prototype.NewObject = NewObject;
  EvaluatorClass.prototype.Return = Return;
  EvaluatorClass.prototype.Continue = Continue;
  EvaluatorClass.prototype.Break = Break;
  EvaluatorClass.prototype.If = If;
  EvaluatorClass.prototype.With = With;
  EvaluatorClass.prototype.Switch = Switch;
  EvaluatorClass.prototype.Loop = Loop;

  return EvaluatorClass;
}());