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

// The main execution machine lives in its own class

static partial class STG {
  // Integer register, used to hold machine integers
  static int ireg = 0;

  // Execution stack, used to hold call parameters
  static Stack<Fun> stack = new Stack<Fun> ();

  // Main entry point
  static void Main () {
    // Since C# doesn't optimize tail calls, we trampoline by having each
    // closure return the next continuation instead.
    var next = _unsafeRealMain;

    while (true)
      // Compute a single step and update the pointer
      next = next.f (); }

  // Push an update continuation
  static Fun update (Fun f) {
    return new Fun (delegate {
      stack.Push (new Fun (delegate {
        // Remember the state after evaluation
        var myireg = ireg;
        // Update the referenced closure with a quasi-constructor
        f.f = delegate {
          ireg = myireg;
          return stack.Pop (); };
        return stack.Pop (); }));

      return f; });}

  // Primitive function to force evaluation and updating
  static Fun _seq = new Fun (delegate {
    return update (stack.Pop ()); });

  // Literal constructor for some i which simply assigns ireg
  static Fun lit (int i) {
    return new Fun (delegate {
      ireg = i;
      return stack.Pop (); }); }

  // Unsafe IO primitives
  static Fun _unsafeTermination = new Fun(delegate {
    throw new Exception ("Improper termination of IO action"); });

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
