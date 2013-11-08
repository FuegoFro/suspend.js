/*
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
*/


/*
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
*/


(function() {
  var Break, Closure, Context, Continue, ControlBlock, Evaluator, FunctionCall, FunctionDefinition, If, Loop, NativeCall, NewObject, Return, Switch, Throw, Try, With, compileExpression, compileStatements, merge,
    __hasProp = {}.hasOwnProperty,
    __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

  compileStatements = function(statements) {
    var body, bodyInstructions, caseMappings, catchBlock, catchVariable, condition, currentTemp, declarator, declaredFunctions, declaredVariables, defaultIndex, elseClause, finalBlock, instructions, loopInstructions, makeLoopTest, makeSwitchCaseIndexChooser, name, object, param, params, startPCTemp, statement, subExpression, subInstructions, subStatements, switchCase, testInstructions, thenClause, tryBlock, updateBytecode, value, _i, _j, _k, _len, _len1, _len2, _ref, _ref1;
    subExpression = function(toCompile, currentTemp) {
      var compiled;
      if (toCompile === null) {
        return null;
      }
      compiled = compileExpression(toCompile, currentTemp);
      merge(instructions, compiled.preInstructions);
      return compiled.expression;
    };
    subStatements = function(toCompile) {
      var bytecode, name, _i, _len, _ref;
      if (toCompile === null) {
        return [];
      }
      if (!Array.isArray(toCompile)) {
        toCompile = [toCompile];
      }
      bytecode = compileStatements(toCompile);
      _ref = bytecode.declaredVariables;
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        name = _ref[_i];
        declaredVariables[name] = 1;
      }
      merge(declaredFunctions, bytecode.declaredFunctions);
      return bytecode.instructions;
    };
    makeLoopTest = function(test) {
      var instructions, loopTest, negatedCondition, testBytecode;
      testBytecode = compileExpression(test);
      instructions = testBytecode.preInstructions;
      negatedCondition = "!(" + testBytecode.expression + ")";
      loopTest = new If(negatedCondition, [new Break()], []);
      return merge(instructions, [loopTest]);
    };
    instructions = [];
    declaredVariables = {};
    declaredFunctions = [];
    for (_i = 0, _len = statements.length; _i < _len; _i++) {
      statement = statements[_i];
      switch (statement.type) {
        case "ExpressionStatement":
          instructions.push(subExpression(statement.expression));
          break;
        case "VariableDeclaration":
          _ref = statement.declarations;
          for (_j = 0, _len1 = _ref.length; _j < _len1; _j++) {
            declarator = _ref[_j];
            name = subExpression(declarator.id);
            declaredVariables[name] = 1;
            if (declarator.init !== null) {
              instructions.push("void (" + name + " = " + (subExpression(declarator.init)) + ")");
            }
          }
          break;
        case "FunctionDeclaration":
          name = subExpression(statement.id);
          params = (function() {
            var _k, _len2, _ref1, _results;
            _ref1 = statement.params;
            _results = [];
            for (_k = 0, _len2 = _ref1.length; _k < _len2; _k++) {
              param = _ref1[_k];
              _results.push(subExpression(param));
            }
            return _results;
          })();
          body = compileStatements([statement.body]);
          declaredFunctions.push(new FunctionDefinition(name, params, body));
          break;
        case "ReturnStatement":
          value = subExpression(statement.argument);
          instructions.push(new Return(value));
          break;
        case "BlockStatement":
          merge(instructions, subStatements(statement.body));
          break;
        case "IfStatement":
          condition = subExpression(statement.test);
          thenClause = subStatements(statement.consequent);
          elseClause = subStatements(statement.alternate);
          instructions.push(new If(condition, thenClause, elseClause));
          break;
        case "LabeledStatement":
          merge(instructions, subStatements(statement.body));
          break;
        case "BreakStatement":
          if (statement.label !== null) {
            throw new Error("Does not support labelled breaks.");
          }
          instructions.push(new Break());
          break;
        case "ContinueStatement":
          if (statement.label !== null) {
            throw new Error("Does not support labelled continues.");
          }
          instructions.push(new Continue());
          break;
        case "ThrowStatement":
          instructions.push(new Throw(subExpression(statement.argument)));
          break;
        case "WithStatement":
          object = subExpression(statement.object);
          body = subStatements(statement.body);
          instructions.push(new With(object, body));
          break;
        case "SwitchStatement":
          startPCTemp = "$__temp__[0]";
          currentTemp = {
            val: 1
          };
          value = subExpression(statement.discriminant, currentTemp);
          subInstructions = [];
          caseMappings = [];
          defaultIndex = null;
          _ref1 = statement.cases;
          for (_k = 0, _len2 = _ref1.length; _k < _len2; _k++) {
            switchCase = _ref1[_k];
            if (switchCase.test !== null) {
              caseMappings.push({
                test: switchCase.test,
                index: subInstructions.length
              });
            } else {
              defaultIndex = subInstructions.length;
            }
            merge(subInstructions, subStatements(switchCase.consequent));
          }
          makeSwitchCaseIndexChooser = function(reverseMappings) {
            var caseIndexChooseInstructions, elseCase, predicate, testBytecode, thenCase;
            if (reverseMappings.length === 0) {
              return ["" + startPCTemp + " = " + defaultIndex];
            }
            switchCase = reverseMappings.pop();
            caseIndexChooseInstructions = [];
            testBytecode = compileExpression(switchCase.test, currentTemp);
            merge(caseIndexChooseInstructions, testBytecode.preInstructions);
            predicate = "" + value + " == " + testBytecode.expression;
            thenCase = ["" + startPCTemp + " = " + switchCase.index];
            elseCase = makeSwitchCaseIndexChooser(reverseMappings);
            caseIndexChooseInstructions.push(new If(predicate, thenCase, elseCase));
            return caseIndexChooseInstructions;
          };
          merge(instructions, makeSwitchCaseIndexChooser(caseMappings.reverse()));
          instructions.push(new Switch(startPCTemp, subInstructions));
          break;
        case "TryStatement":
          tryBlock = subStatements(statement.block);
          catchBlock = catchVariable = finalBlock = null;
          if (statement.handlers.length !== 0) {
            catchBlock = subStatements(statement.handlers[0].body);
            catchVariable = subExpression(statement.handlers[0].param);
          }
          finalBlock = subStatements(statement.finalizer);
          instructions.push(new Try(tryBlock, catchBlock, catchVariable, finalBlock));
          break;
        case "WhileStatement":
          loopInstructions = [];
          merge(loopInstructions, makeLoopTest(statement.test));
          merge(loopInstructions, subStatements(statement.body));
          instructions.push(new Loop(loopInstructions));
          break;
        case "DoWhileStatement":
          bodyInstructions = subStatements(statement.body);
          testInstructions = makeLoopTest(statement.test);
          loopInstructions = testInstructions.concat(bodyInstructions);
          instructions.push(new Loop(loopInstructions, testInstructions.length));
          break;
        case "ForStatement":
          if (statement.init !== null) {
            if (statement.init.type === "VariableDeclaration") {
              merge(instructions, subStatements(statement.init));
            } else {
              instructions.push(subExpression(statement.init));
            }
          }
          loopInstructions = [];
          if (statement.test !== null) {
            merge(loopInstructions, makeLoopTest(statement.test));
          }
          merge(loopInstructions, subStatements(statement.body));
          if (statement.update !== null) {
            updateBytecode = compileExpression(statement.update);
            merge(loopInstructions, updateBytecode.preInstructions);
            loopInstructions.push(updateBytecode.expression);
          }
          instructions.push(new Loop(loopInstructions));
      }
    }
    return {
      instructions: instructions,
      declaredVariables: Object.keys(declaredVariables),
      declaredFunctions: declaredFunctions
    };
  };

  /*
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
  */


  compileExpression = function(expression, currentTemp) {
    var Cls, arg, args, body, callee, compiled, element, elements, elseInstructions, extraInstructions, functionName, getTempVariable, ifInstructions, ifPredicate, left, name, object, operator, param, params, properties, property, right, separator, statementsFromExpression, subExpression, tempVar, test, thenInstructions, thisValue, _i, _len, _ref;
    if (currentTemp == null) {
      currentTemp = {
        val: 0
      };
    }
    subExpression = function(toCompile) {
      var compiled;
      compiled = compileExpression(toCompile, currentTemp);
      merge(extraInstructions, compiled.preInstructions);
      return compiled.expression;
    };
    statementsFromExpression = function(toCompile, destination) {
      var bytecode;
      bytecode = compileExpression(toCompile);
      return bytecode.preInstructions.concat(["" + destination + " = " + bytecode.expression]);
    };
    getTempVariable = function() {
      return "$__temp__[" + (currentTemp.val++) + "]";
    };
    extraInstructions = [];
    switch (expression.type) {
      case "ThisExpression":
        compiled = "this";
        break;
      case "Literal":
        compiled = expression.raw;
        break;
      case "Identifier":
        compiled = expression.name;
        break;
      case "ArrayExpression":
        elements = (function() {
          var _i, _len, _ref, _results;
          _ref = expression.elements;
          _results = [];
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            element = _ref[_i];
            _results.push(subExpression(element));
          }
          return _results;
        })();
        compiled = "[" + (elements.join(", ")) + "]";
        break;
      case "ObjectExpression":
        properties = [];
        _ref = expression.properties;
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          property = _ref[_i];
          properties.push(subExpression(property.key) + ": " + subExpression(property.value));
        }
        compiled = "({" + properties.join(", ") + "})";
        break;
      case "FunctionExpression":
        name = (expression.id !== null ? subExpression(expression.id) : null);
        params = (function() {
          var _j, _len1, _ref1, _results;
          _ref1 = expression.params;
          _results = [];
          for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
            param = _ref1[_j];
            _results.push(subExpression(param));
          }
          return _results;
        })();
        body = compileStatements([expression.body]);
        tempVar = getTempVariable();
        extraInstructions.push(new FunctionDefinition(name, params, body, tempVar));
        compiled = tempVar;
        break;
      case "SequenceExpression":
        elements = (function() {
          var _j, _len1, _ref1, _results;
          _ref1 = expression.expressions;
          _results = [];
          for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
            element = _ref1[_j];
            _results.push(subExpression(element));
          }
          return _results;
        })();
        compiled = elements.join(", ");
        break;
      case "UnaryExpression":
        operator = expression.operator;
        separator = (operator.length > 1 ? " " : "");
        compiled = operator + separator + subExpression(expression.argument);
        break;
      case "BinaryExpression":
      case "AssignmentExpression":
        left = subExpression(expression.left);
        right = subExpression(expression.right);
        compiled = "(" + left + " " + expression.operator + " " + right + ")";
        break;
      case "LogicalExpression":
        tempVar = getTempVariable();
        extraInstructions.push("" + tempVar + " = " + (subExpression(expression.left)));
        ifPredicate = tempVar;
        if (expression.operator === "||") {
          ifPredicate = "!" + ifPredicate;
        }
        ifInstructions = statementsFromExpression(expression.right, tempVar);
        extraInstructions.push(new If(ifPredicate, ifInstructions, []));
        compiled = tempVar;
        break;
      case "UpdateExpression":
        compiled = subExpression(expression.argument);
        if (expression.prefix) {
          compiled = expression.operator + compiled;
        } else {
          compiled += expression.operator;
        }
        break;
      case "ConditionalExpression":
        tempVar = getTempVariable();
        test = subExpression(expression.test);
        thenInstructions = statementsFromExpression(expression.consequent, tempVar);
        elseInstructions = statementsFromExpression(expression.alternate, tempVar);
        extraInstructions.push(new If(test, thenInstructions, elseInstructions));
        compiled = tempVar;
        break;
      case "MemberExpression":
        property = subExpression(expression.property);
        object = subExpression(expression.object);
        if (expression.computed) {
          compiled = object + ("[" + property + "]");
        } else {
          compiled = "" + object + "." + property;
        }
        break;
      case "CallExpression":
      case "NewExpression":
        if (expression.type === "CallExpression") {
          Cls = FunctionCall;
          if (expression.callee.type === "MemberExpression") {
            thisValue = subExpression(expression.callee.object);
            functionName = subExpression(expression.callee.property);
            if (expression.callee.property.type === "Identifier" && !expression.callee.computed) {
              functionName = "'" + functionName + "'";
            }
          } else {
            thisValue = null;
            functionName = subExpression(expression.callee);
          }
          callee = [thisValue, functionName];
        } else {
          Cls = NewObject;
          callee = subExpression(expression.callee);
        }
        args = (function() {
          var _j, _len1, _ref1, _results;
          _ref1 = expression["arguments"];
          _results = [];
          for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
            arg = _ref1[_j];
            _results.push(subExpression(arg));
          }
          return _results;
        })();
        tempVar = getTempVariable();
        extraInstructions.push(new Cls(callee, args, tempVar));
        compiled = tempVar;
    }
    return {
      preInstructions: extraInstructions,
      expression: compiled
    };
  };

  /*
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
  */


  ControlBlock = (function() {
    function ControlBlock() {}

    ControlBlock.prototype.updateState = function() {};

    ControlBlock.prototype.canBreak = function() {
      return false;
    };

    ControlBlock.prototype.canContinue = function() {
      return false;
    };

    ControlBlock.prototype.canReturn = function() {
      return false;
    };

    ControlBlock.prototype.canCatch = function() {
      return false;
    };

    return ControlBlock;

  })();

  /*
  This ia a control block that is used only in the native wrapper around Closures
  that allows user defiend functions to be called by native Javascript functions.
  */


  NativeCall = (function() {
    function NativeCall() {}

    NativeCall.prototype.canReturn = function() {
      return true;
    };

    NativeCall.prototype.handleReturn = function(context, value) {
      return context.done(value);
    };

    NativeCall.prototype.canCatch = function() {
      return true;
    };

    NativeCall.prototype.handleError = function(context, error) {
      return context.done(error, true);
    };

    return NativeCall;

  })();

  Closure = (function() {
    function Closure(_function, environment, context) {
      var closure, evaluator;
      this["function"] = _function;
      this.environment = environment;
      closure = this;
      evaluator = context.evaluator;
      this.wrapper = function() {
        var newContext, oldContext, onComplete, returnValue, wasError;
        oldContext = evaluator._getContext();
        returnValue = void 0;
        wasError = false;
        onComplete = function(value, isError) {
          returnValue = value;
          return wasError = isError;
        };
        newContext = new Context(evaluator, context.scope, onComplete);
        newContext.pushState(closure.getInstructions(), new NativeCall(), closure.getEnvironment(arguments), this);
        evaluator._setContext(newContext);
        evaluator._execute();
        evaluator._setContext(oldContext);
        if (wasError) {
          throw returnValue;
        } else {
          return returnValue;
        }
      };
    }

    Closure.prototype.getInstructions = function() {
      return this["function"].body.declaredFunctions.concat(this["function"].body.instructions);
    };

    Closure.prototype.getEnvironment = function(args) {
      var func, i, newEnvironmentFrame, param, variable, _i, _j, _k, _len, _len1, _len2, _ref, _ref1, _ref2;
      newEnvironmentFrame = {
        $__temp__: []
      };
      _ref = this["function"].body.declaredVariables;
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        variable = _ref[_i];
        newEnvironmentFrame[variable] = void 0;
      }
      _ref1 = this["function"].body.declaredFunctions;
      for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
        func = _ref1[_j];
        newEnvironmentFrame[func.name] = void 0;
      }
      _ref2 = this["function"].params;
      for (i = _k = 0, _len2 = _ref2.length; _k < _len2; i = ++_k) {
        param = _ref2[i];
        newEnvironmentFrame[param] = args[i];
      }
      Object.defineProperty(args, 'callee', {
        value: this.wrapper,
        enumerable: false
      });
      newEnvironmentFrame["arguments"] = args;
      if (this["function"].tempVar !== null && this["function"].name !== null) {
        newEnvironmentFrame[this["function"].name] = this.wrapper;
      }
      return this.environment.concat([newEnvironmentFrame]);
    };

    Closure.closureFieldName = "$__closure__";

    Closure.isWrapper = function(func) {
      return typeof func === "function" && this.toClosure(func) instanceof this;
    };

    Closure.toClosure = function(func) {
      return func[this.closureFieldName];
    };

    Closure.toWrapper = function(closure) {
      closure.wrapper[this.closureFieldName] = closure;
      return closure.wrapper;
    };

    return Closure;

  })();

  FunctionDefinition = (function() {
    function FunctionDefinition(name, params, body, tempVar) {
      this.name = name;
      this.params = params;
      this.body = body;
      this.tempVar = tempVar != null ? tempVar : null;
    }

    FunctionDefinition.prototype.updateState = function(context) {
      var closure, targetLocation, wrapper;
      targetLocation = this.tempVar || this.name;
      closure = new Closure(this, context.getEnvironment(), context);
      wrapper = Closure.toWrapper(closure);
      wrapper.prototype = new context.scope.Object();
      wrapper.prototype.constructor = wrapper;
      return context.setValue(targetLocation, wrapper);
    };

    return FunctionDefinition;

  })();

  FunctionCall = (function() {
    function FunctionCall(callee, args, tempVar) {
      this.callee = callee;
      this.args = args;
      this.tempVar = tempVar;
    }

    FunctionCall.prototype.updateState = function(context) {
      var arg, argValues, closure, func, functionLocation, thisObject;
      if (this.callee[0] === null) {
        functionLocation = this.callee[1];
      } else {
        functionLocation = "" + this.callee[0] + "[" + this.callee[1] + "]";
      }
      func = context["eval"](functionLocation);
      if (Closure.isWrapper(func)) {
        closure = Closure.toClosure(func);
        thisObject = this.callee[0] === null ? null : context["eval"](this.callee[0]);
        argValues = (function() {
          var _i, _len, _ref, _results;
          _ref = this.args;
          _results = [];
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            arg = _ref[_i];
            _results.push(context["eval"](arg));
          }
          return _results;
        }).call(this);
        return context.pushState(closure.getInstructions(), this, closure.getEnvironment(argValues), thisObject);
      } else {
        return context["eval"]("" + this.tempVar + " = " + functionLocation + "(" + (this.args.join(", ")) + ")");
      }
    };

    FunctionCall.prototype.canReturn = function() {
      return true;
    };

    FunctionCall.prototype.canCatch = function() {
      return false;
    };

    FunctionCall.prototype.handleReturn = function(context, value) {
      return context.setValue(this.tempVar, value);
    };

    return FunctionCall;

  })();

  NewObject = (function() {
    function NewObject(callee, args, tempVar) {
      this.callee = callee;
      this.args = args;
      this.tempVar = tempVar;
    }

    NewObject.prototype.updateState = function(context) {
      var arg, argValues, closure, func, instance;
      func = context["eval"](this.callee);
      if (Closure.isWrapper(func)) {
        closure = Closure.toClosure(func);
        instance = Object.create(func.prototype);
        context.setValue(this.tempVar, instance);
        argValues = (function() {
          var _i, _len, _ref, _results;
          _ref = this.args;
          _results = [];
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            arg = _ref[_i];
            _results.push(context["eval"](arg));
          }
          return _results;
        }).call(this);
        return context.pushState(closure.getInstructions(), this, closure.getEnvironment(argValues), instance);
      } else {
        return context["eval"]("" + this.tempVar + " = new " + this.callee + "(" + (this.args.join(", ")) + ")");
      }
    };

    NewObject.prototype.canCatch = function() {
      return false;
    };

    NewObject.prototype.canReturn = function() {
      return true;
    };

    NewObject.prototype.handleReturn = function() {};

    return NewObject;

  })();

  Return = (function() {
    function Return(value) {
      this.value = value;
    }

    Return.prototype.updateState = function(context) {
      var returnValue, returner;
      returnValue = context["eval"](this.value);
      while (!context.getControlObject().canReturn()) {
        context.popState();
      }
      returner = context.getControlObject();
      context.popState();
      return returner.handleReturn(context, returnValue);
    };

    return Return;

  })();

  If = (function(_super) {
    __extends(If, _super);

    function If(condition, thenCase, elseCase) {
      this.condition = condition;
      this.thenCase = thenCase;
      this.elseCase = elseCase;
    }

    If.prototype.updateState = function(context) {
      var instructions;
      instructions = (context["eval"](this.condition) ? this.thenCase : this.elseCase);
      return context.pushState(instructions, this);
    };

    return If;

  })(ControlBlock);

  With = (function(_super) {
    __extends(With, _super);

    function With(object, body) {
      this.object = object;
      this.body = body;
    }

    With.prototype.updateState = function(context) {
      var environmentFrame, newEnvironment;
      environmentFrame = context["eval"](this.object);
      newEnvironment = context.getEnvironment().concat([environmentFrame]);
      return context.pushState(this.body, this, newEnvironment);
    };

    return With;

  })(ControlBlock);

  Switch = (function(_super) {
    __extends(Switch, _super);

    function Switch(startPCVar, instructions) {
      this.startPCVar = startPCVar;
      this.instructions = instructions;
    }

    Switch.prototype.updateState = function(context) {
      var startPC;
      startPC = context["eval"](this.startPCVar);
      if (startPC !== null) {
        context.pushState(this.instructions, this);
        return context.setPC(startPC);
      }
    };

    Switch.prototype.canBreak = function() {
      return true;
    };

    return Switch;

  })(ControlBlock);

  Break = (function() {
    function Break() {}

    Break.prototype.updateState = function(context) {
      while (!context.getControlObject().canBreak()) {
        context.popState();
      }
      return context.popState();
    };

    return Break;

  })();

  Continue = (function() {
    function Continue() {}

    Continue.prototype.updateState = function(context) {
      while (!context.getControlObject().canContinue()) {
        context.popState();
      }
      return context.setPC(0);
    };

    return Continue;

  })();

  Throw = (function() {
    function Throw(error, evaluated) {
      this.error = error;
      this.evaluated = evaluated != null ? evaluated : false;
    }

    Throw.prototype.updateState = function(context) {
      var controlObject, errorObject;
      if (this.evaluated) {
        errorObject = this.error;
      } else {
        errorObject = context["eval"](this.error);
      }
      while (context.hasMoreStates() && !context.getControlObject().canCatch()) {
        context.popState();
      }
      if (context.hasMoreStates()) {
        controlObject = context.getControlObject();
        context.popState();
        return controlObject.handleError(context, errorObject);
      } else {
        return context.done(errorObject, true);
      }
    };

    return Throw;

  })();

  Try = (function(_super) {
    __extends(Try, _super);

    function Try(tryBlock, catchBlock, catchVariable, finallyBlock) {
      this.tryBlock = tryBlock;
      this.catchBlock = catchBlock;
      this.catchVariable = catchVariable;
      this.finallyBlock = finallyBlock;
      this._isInTry = false;
      this._isInCatch = false;
    }

    Try.prototype.updateState = function(context) {
      var instructions;
      this._isInTry = true;
      this._endOfBlock = {};
      instructions = this.tryBlock.concat([new Throw(this._endOfBlock, true)]);
      return context.pushState(instructions, this);
    };

    Try.prototype.canCatch = function() {
      return this._isInTry || this._isInCatch;
    };

    Try.prototype.handleError = function(context, error) {
      var instructions, newEnvironment, newEnvironmentFrame;
      if (error === this._endOfBlock) {
        this._isInTry = this._isInCatch = false;
        if (this.finallyBlock.length > 0) {
          return context.pushState(this.finallyBlock, this);
        }
      } else if (this._isInTry && this.catchBlock !== null) {
        this._isInTry = false;
        this._isInCatch = true;
        instructions = this.catchBlock.concat([new Throw(this._endOfBlock, true)]);
        newEnvironmentFrame = {};
        newEnvironmentFrame[this.catchVariable] = error;
        newEnvironment = context.getEnvironment().concat([newEnvironmentFrame]);
        return context.pushState(instructions, this, newEnvironment);
      } else {
        this._isInTry = this._isInCatch = false;
        instructions = this.finallyBlock.concat([new Throw(error, true)]);
        return context.pushState(instructions, this);
      }
    };

    return Try;

  })(ControlBlock);

  Loop = (function(_super) {
    __extends(Loop, _super);

    function Loop(instructions, initialPC) {
      this.instructions = instructions;
      this.initialPC = initialPC != null ? initialPC : 0;
      this.instructions.push(new Continue());
    }

    Loop.prototype.updateState = function(context) {
      context.pushState(this.instructions, this);
      return context.setPC(this.initialPC);
    };

    Loop.prototype.canContinue = function() {
      return true;
    };

    Loop.prototype.canBreak = function() {
      return true;
    };

    return Loop;

  })(ControlBlock);

  /*
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
  */


  Context = (function() {
    function Context(evaluator, scope, onComplete) {
      this.evaluator = evaluator;
      this.scope = scope;
      this.onComplete = onComplete;
      this.stateStack = [];
      this.isDone = false;
    }

    Context.prototype["eval"] = function(command) {
      var e, environment, i, postWrap, preWrap, thisObject, _i, _ref;
      preWrap = "";
      postWrap = "";
      environment = this.getEnvironment();
      this.scope["$__env__"] = environment;
      for (i = _i = 0, _ref = environment.length; 0 <= _ref ? _i < _ref : _i > _ref; i = 0 <= _ref ? ++_i : --_i) {
        preWrap += "with($__env__[" + i + "]){";
        postWrap = "}" + postWrap;
      }
      thisObject = this.getThisObject();
      if (thisObject !== null) {
        this.scope["$__this__"] = thisObject;
        preWrap = "(function () {" + preWrap + "return ";
        postWrap += "}).call($__this__)";
      }
      command = preWrap + command + postWrap;
      try {
        if ((this.scope["eval"] == null) && (this.scope.execScript != null)) {
          this.scope.execScript("null");
        }
        return this.scope["eval"](command);
      } catch (_error) {
        e = _error;
        if (e instanceof this.scope.Error || e instanceof Error) {
          e = Object.create(e, {
            stack: {
              value: null
            }
          });
        }
        return this.pushState([new Throw(e, true)]);
      }
    };

    Context.prototype.done = function(value, isError) {
      if (!this.isDone) {
        this.isDone = true;
        return typeof this.onComplete === "function" ? this.onComplete(value, isError) : void 0;
      }
    };

    Context.prototype.pushState = function(instructions, controlObject, environment, thisObject) {
      if (controlObject == null) {
        controlObject = new ControlBlock();
      }
      if (environment == null) {
        environment = this.getEnvironment();
      }
      if (thisObject == null) {
        thisObject = this.getThisObject();
      }
      return this.stateStack.push({
        instructions: instructions,
        pc: 0,
        controlObject: controlObject,
        environment: environment,
        thisObject: thisObject
      });
    };

    Context.prototype.popState = function() {
      return this.stateStack.pop();
    };

    Context.prototype.getCurrentState = function() {
      if (this.stateStack.length > 0) {
        return this.stateStack[this.stateStack.length - 1];
      } else {
        return {
          instructions: [],
          pc: 0,
          controlObject: null,
          environment: [
            {
              $__temp__: []
            }
          ],
          thisObject: null
        };
      }
    };

    Context.prototype.stateHasMoreInstructions = function() {
      var currentState;
      currentState = this.getCurrentState();
      return currentState.instructions.length > currentState.pc;
    };

    Context.prototype.getNextInstruction = function() {
      var currentState, instruction;
      currentState = this.getCurrentState();
      instruction = currentState.instructions[currentState.pc];
      currentState.pc++;
      return instruction;
    };

    Context.prototype.getPreviousInstruction = function() {
      var currentState;
      currentState = this.getCurrentState();
      return currentState.instructions[currentState.pc - 1];
    };

    Context.prototype.hasMoreStates = function() {
      return this.stateStack.length !== 0;
    };

    Context.prototype.getControlObject = function() {
      return this.getCurrentState().controlObject;
    };

    Context.prototype.setPC = function(newPC) {
      return this.getCurrentState().pc = newPC;
    };

    Context.prototype.getEnvironment = function() {
      return this.getCurrentState().environment;
    };

    Context.prototype.setValue = function(name, value) {
      this.scope["$__result__"] = value;
      return this["eval"](name + " = $__result__");
    };

    Context.prototype.getThisObject = function() {
      return this.getCurrentState().thisObject;
    };

    return Context;

  })();

  Evaluator = (function() {
    function Evaluator() {
      var iframe;
      iframe = document.createElement("iframe");
      iframe.height = iframe.width = 0;
      iframe.style["visibility"] = "hidden";
      document.body.appendChild(iframe);
      this.scope = iframe.contentWindow;
      this.context = null;
    }

    Evaluator.prototype.setGlobal = function(name, value) {
      return this.scope[name] = value;
    };

    Evaluator.prototype.getGlobal = function(name) {
      return this.scope[name];
    };

    /*
    Takes in the string of the code to be evaluated and a callback that has two
    parameters. If the evaluation does not produce an error, the first argument
    to the callback will be the result of the evaluation as expected normally
    (an arbitrary Javascript value), and the second value will be the boolean
    `false`. If an uncaught error occurs while evaluating the code, the first
    argument will be the error object (again an arbitrary Javascript value) and
    the second will be the boolean `true`.
    */


    Evaluator.prototype["eval"] = function(string, onComplete) {
      var ast, bytecode, instructions;
      ast = esprima.parse(string).body;
      bytecode = compileStatements(ast);
      instructions = bytecode.declaredFunctions.concat(bytecode.instructions);
      this.context = new Context(this, this.scope, onComplete);
      this.context.pushState(instructions);
      return this._execute();
    };

    Evaluator.prototype._execute = function() {
      var instruction, lastResult;
      this.isRunning = true;
      lastResult = void 0;
      while (this.context.hasMoreStates()) {
        while (this.context.stateHasMoreInstructions()) {
          instruction = this.context.getNextInstruction();
          if (typeof instruction === "string") {
            lastResult = this.context["eval"](instruction);
          } else {
            instruction.updateState(this.context);
          }
          if (!this.isRunning) {
            return;
          }
        }
        this.context.popState();
      }
      return this.context.done(lastResult, false);
    };

    Evaluator.prototype.pause = function() {
      var context, _ref;
      this.isRunning = false;
      _ref = [this.context, null], context = _ref[0], this.context = _ref[1];
      return context;
    };

    Evaluator.prototype.resume = function(context, value, isError) {
      var lastInstruction;
      if (isError == null) {
        isError = false;
      }
      if (context === null) {
        return;
      }
      if (!context) {
        throw new Error("Resuming evaluation requires a context as returned by pause.");
      } else if (!(context instanceof Context)) {
        throw new Error("Invalid context given to resume.");
      }
      if (arguments.length > 1) {
        lastInstruction = context.getPreviousInstruction();
        if (isError) {
          context.pushState([new Throw(value, true)]);
        } else {
          lastInstruction.handleReturn(context, value);
        }
      }
      this.context = context;
      return this._execute();
    };

    Evaluator.prototype._getContext = function() {
      return this.context;
    };

    Evaluator.prototype._setContext = function(context) {
      this.context = context;
    };

    return Evaluator;

  })();

  Evaluator.compileStatements = compileStatements;

  Evaluator.compileExpression = compileExpression;

  Evaluator.Function = FunctionDefinition;

  Evaluator.FunctionCall = FunctionCall;

  Evaluator.NewObject = NewObject;

  Evaluator.Return = Return;

  Evaluator.Continue = Continue;

  Evaluator.Break = Break;

  Evaluator.Throw = Throw;

  Evaluator.If = If;

  Evaluator.With = With;

  Evaluator.Switch = Switch;

  Evaluator.Try = Try;

  Evaluator.Loop = Loop;

  window.Evaluator = Evaluator;

  if (!Array.isArray) {
    Array.isArray = function(vArg) {
      return Object.prototype.toString.call(vArg) === "[object Array]";
    };
  }

  merge = function(first, second) {
    var i, j, l, _i;
    i = first.length;
    l = second.length;
    j = 0;
    if (typeof l === "number") {
      for (j = _i = 0; 0 <= l ? _i < l : _i > l; j = 0 <= l ? ++_i : --_i) {
        first[i++] = second[j];
      }
    } else {
      while (second[j] !== void 0) {
        first[i++] = second[j++];
      }
    }
    first.length = i;
    return first;
  };

}).call(this);
