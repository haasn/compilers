using System;
using System.Collections.Generic;

// One level of indirection is maintained for function closures,
// because it allows later updating.

class Fun {
  public FunPtr f;
  public Fun () {}
  public Fun (FunPtr p) { f = p; }}

// Type safe delegate for computing continuations

delegate Fun FunPtr ();

// Exceptions used for various errors: no match in case, WHNF, improper
// termination.

class CaseException : Exception {
  int t; public CaseException (int i) { t = i; }
  public override string ToString () { return
    "Incomplete pattern match for tag = " + t; }}

class TerminationException : Exception {
  public override string ToString () { return
    "Improper termination in IO action"; }}

// The main execution machine lives in its own class

static partial class STG {
  // Integer register, used to hold tags as well as machine integers
  static int ireg = 0;
  // Double register, currently unused but theoretically for machine floats
  static double dreg = 0;
  // Execution stack, used to hold closures and continuations
  static Stack<Fun> stack = new Stack<Fun> ();
  // Constructor field array, used to hold arguments to computed ctors
  static Fun[] vars = null;

  // Main entry point
  static void Main () {
    // Since C# doesn't optimize tail calls, we trampoline by having each
    // closure return the next continuation instead.
    var next = _unsafeRealMain;

    while (true)
      // Compute a single step and update the pointer
      next = next.f (); }

  // Push an update continuation
  static void update (Fun f) {
    stack.Push (new Fun (delegate {
      // Remember the state after evaluation
      var myireg = ireg;
      var mydreg = dreg;
      var myvars = vars;
      // Update the referenced closure with a quasi-constructor
      f.f = delegate {
        ireg = myireg;
        dreg = mydreg;
        vars = myvars;
        return stack.Pop (); };
      return stack.Pop (); })); }

  // Primitive function to force evaluation and updating
  static Fun _seq = new Fun (delegate {
    var f = stack.Pop ();
    update (f);
    return f; });

  // Literal constructor for some i which simply assigns ireg
  static Fun lit (int i) {
    return new Fun (delegate {
      ireg = i;
      return stack.Pop (); }); }

  // Unsafe IO primitives
  static Fun _unsafeTermination = new Fun (delegate {
    throw new TerminationException (); });

  static Fun _unsafePutChar = new Fun (delegate {
    var c = stack.Pop ();
    stack.Push (new Fun (delegate {
      Console.Write ((char) ireg);
      return _unit; }));
    return c; });

  static Fun _unsafeGetChar = new Fun (delegate {
    stack.Pop (); // Unit, ignored
    ireg = (int) Console.ReadKey (true).KeyChar;
    return stack.Pop (); });

  static Fun _unsafeExit = new Fun (delegate {
    var code = stack.Pop ();
    stack.Push (new Fun (delegate {
      Environment.Exit (ireg);
      return null; }));
    return code; });

  // Boring integer arithmetic primitives
  static Fun Add (Fun a, Fun b) {
    return new Fun (delegate {
      stack.Push (new Fun (delegate {
        int it = ireg;
        stack.Push (new Fun (delegate {
          ireg += it;
          return stack.Pop (); }));
        return b; }));
      return a; });}

  static Fun Mul (Fun a, Fun b) {
    return new Fun (delegate {
      stack.Push (new Fun (delegate {
        int it = ireg;
        stack.Push (new Fun (delegate {
          ireg *= it;
          return stack.Pop (); }));
        return b; }));
      return a; });}

  static Fun Sub (Fun a, Fun b) {
    return new Fun (delegate {
      stack.Push (new Fun (delegate {
        int it = ireg;
        stack.Push (new Fun (delegate {
          ireg -= it;
          return stack.Pop (); }));
        return b; }));
      return a; });}

  static Fun Div (Fun a, Fun b) {
    return new Fun (delegate {
      stack.Push (new Fun (delegate {
        int it = ireg;
        stack.Push (new Fun (delegate {
          ireg /= it;
          return stack.Pop (); }));
        return b; }));
      return a; });}
}
