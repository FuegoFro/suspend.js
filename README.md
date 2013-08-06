# suspend.js

The purpose of this project is to be able to pause arbitrary Javascript interpretation during any function call and replace the result of that call with an arbitrary value. It is written with the use case of a browser based console in mind, but was designed in a generalized fasion to evaluate (as in, using ```eval``` on a string) arbitrary Javascript code in a sandboxed, pause-able fashion. The problem we are solving is simulating a blocking paradigm without locking up the browser. For instance suppose a function ```getData``` needs to fetch JSON from a remote server and you want the interaction to look like:

```javascript
data = getData();
```

Normally the only way to do this is to have the ajax request performed in ```getData``` to be synchronous so that the ```getData``` call doesn't return until the data has been fetched. However, a synchronous request will lock up the browser, preventing the user from interacting with the page at all and creating a poor experience. One solution is to have an interpreter that can pause execution at any time and resume execution from that same point later. That way ```getData``` could pause interpretation, make an asynchronous request, and allow the function to return, freeing up the Javascript thread. When the data gets back from the server, execution can be resumed and the recieved data can be dropped in as the result of the most recent function call. In this way, we allow the execution of 'blocking' code without blocking the browser.

## Using

You can either use the latest built version of Suspend found in the ```dist``` folder or you can build it from source. When the library is included in a page it exports a single variable to the global namespace, which is the class ```Evaluator```. For example, the following would create an instance of the ```Evaluator``` class and evaluate basic Javascript code:

```html
<script type="test/javascript" src="esprima.js"></script>
<script type="test/javascript" src="dist/suspend.js"></script>
<script type="test/javascript">
  var evaluator = new Evaluator();
  evaluator.eval('console.log("hello")');
</script>
```

Let's take the ```getData``` example given above. With this initialization, a solution to this problem might look like this:

```javascript
var getData = function () {
  var context = evaluator.pause();
  // Assume some function to make a network request
  // that takes in a url, some data, and a callback.
  ajaxRequest(url, data, function (response) {
    evaluator.resume(context, response);
  });
};
evaluator.setGlobal('getData', getData);

evaluator.eval('data = getData()');
```

### Dependencies

The only library that Suspend relies on to run is [Esprima][]. Make sure it is loaded into the page before trying to eval any code using Suspend. Suspend also assumes that you are in a browser-like environment, particularly that a ```document``` exists, can create an ```iframe``` and append it to ```document.body```. This is used for sandboxing evaluated Javascript.

To build, you will additionally need [Node.js](nodejs.org).


### Building

The following commands will initialize a freshly cloned repository and compile the library:

```bash
git submodule init && git submodule update
npm install
```

Then run ```grunt``` to compile the code and run the tests. You can find ```grunt``` either in the ```node_modules``` subdirectory created by Node.js or you can run

```bash
npm install -g grunt-cli
```

to globally install the grunt command line interface and alias, at which point you should be able to run

```bash
grunt
```

to compile the code and run the tests. Compiled code is stored in the ```dist``` directory.

### Browser Compatibility

This has been tested on the latest versions of Chrome and Firefox and works on IE 8 and up.

## API

The Suspend library creates a global class called ```Evaluator```. The constructor for this class takes no arguments and creates an invisible ```iframe```, appending it to the document body. The resulting instance has a number of public APIs to used to evaluate code, interface with the sandbox, and pause and resume execution.

- ```eval(jsCode, onComplete)```: Takes in a string and a function. The ```jsCode``` string should be the text of the Javascript to be evaluated. The ```onComplete``` function should take two arguments, an arbitrary Javascript value and a boolean. If the boolean is false, then the evaluation finished normally (there were no uncaught errors) and the first argument to onComplete is the value of the last statement evaluated. If the boolean is true, then there was an uncaught error and the value of the first argument is that error.
- ```setGlobal(name, value)```: Takes a string ```name``` and an arbitrary Javascript value ```value```. It sets a global variable with name ```name``` to be value ```value``` in the sandbox that the code is evaluated in.
- ```getGlobal(name)```: Takes a string ```name``` and returns the value of the global variable with name ```name``` in the sandbox that the code is evaluated in.
- ```pause()```: Pauses execution of the program, preventing any further statements from being evaluated. This function returns an execution context which is used to resume the evaluation of that particular code. Due to the single threaded nature of Javascript, it is only possible for this be called during a function call in the current execution or when nothing is being evaluated.
- ```resume(context, value [optional], isError [optional, defaults to false])```: Resumes execution of the code. The ```context``` argument must be a context object returned by ```pause```. Since the program must have been paused during a function call, the instruction that was run immediately previously must be a function call. When ```isError``` is false, the ```value``` argument is treated as the return value of that function, overriding any value that the function actually did return. When ```isError``` is true, the ```value``` argument is treated as an error that was thrown by that funciton, and the error is rethrown in the calling execution context. The ```isError``` argument defaults to false. If only the ```context``` is provided, the function's actual return value will be used.


## Architecture

In order to lessen the work that needs to be done by the interpreter, we rely on the browser's builtin ```eval``` as much as possible. To do this we try to keep most of the instructions as strings. However, in order to be able to pause execution at any point, such as in the middle of a loop or function call, we need to keep track of the execution state/context manually. This means that instructions which affect control flow (what the next instruction will be) have to be handled separately. Since this includes function calls, we have to perform function calls by hand. This means that we also need to compile all user defined functions in a similar manner. Additionally, since function calls affect the environment and what variables are in scope, we need to handle the environment chains as well. Thus, anything that affects the control flow or the environment cannot be run with ```eval```, but needs to be handled by the evaluator. With these requirements in mind, a high level outline of the evaluation is given in two main steps:

1. Walk the AST created by [Esprima][] and convert the statements into 'bytecode'.
    - Mutate the source code for easier evaluation.
      - Hoist declared function and variable, using function scoping.
        - All ```var```'s are pulled to the top of the scope, whether that is the top level of the code being executed or a function defined within the code.
        - Internally, this is done by setting all declared names to undefined in the appropriate environment frame upon function invocation.
      - Unpack inlined or chained method calls as well as function definitions onto their own line, introducing temporary variables as necessary.
        - By requiring that all function calls be their own statements, it becomes very easy to pause after a function call without evaluating more code or relying on the result of that call. This is because we will be pausing between statements rather than having to try to pause in the middle of an expression.
    - Output 'bytecode' that can be easily interpreted.
      - All statements that affect control flow or the environment are converted in objects.
        - This includes function definitions, function calls, if statements, loops, with blocks, try/catch/finally blocks, returns, breaks, continues, and throws.
      - All other statements are converted back to strings to be run through ```eval```.
2. Interpret the 'bytecode' produced in the first step. As mentioned, this has three main requirements:
    - Keep track of the execution context.
      - This includes the call stack and program counter (current instruction) for each entry in the stack.
    - Keep track of environment.
      - When evaluating statements, we wrap the text being passed to ```eval``` in ```with``` blocks to simulate the lookup chain represented by our environment frames.
      - On function definition, we save a copy of the current environment with the function, creating a closure.
      - On function invocation, use the closure's environment.
      - On function return, use the previous environment moving up our call stack.
    - Be able to pause and resume execution.
      - On pause, save off and return all relevant state (call stack, program counter), exiting the evaluation loop.
      - On resume, pick up execution where it left off. From within the program, there should be no way to tell whether or not it has been paused.
      - Can set the return value of the function that was being called when execution was paused.

## Issues/Differences from Javascript

While many parts of interpreter are complete, it is still under development and behavior of the evaluated code may differ from that of a full blown interpreter such as V8. Below is a list of the currently known differences from the Javascript specification.

- The ```caller``` property on functions is not set properly during function calls.
- ```var``` statements should not affect the output of ```eval```, so ```"1; var a = 0"``` should return ```1``` when evaluated. However, right now it returns ```undefined```.
- Labels, as well as labelled breaks and continues, are not supported.

## Testing

This project was developed using Behavior Driven Development and has a complete test suite. This helps to ensure correctness (at least within the domain of what is tested) since this project is decently complex and has many edge cases.

[esprima]: esprima.org "Esprima"