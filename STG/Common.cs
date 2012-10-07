using System;
using System.Collections.Generic;

// One level of indirection is maintained for function closures,
// because it allows later updating and recursive mentioning.

class Fun {
  public FunPtr f;
  public Fun () {}
  public Fun (FunPtr p) { f = p; }}

// Type safe delegate for computing continuations

delegate Fun FunPtr ();

// The main execution machine lives in its own class

static partial class STG {
  // Dynamic register, used for all FFI operations
  static dynamic reg = null;

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

  // Primitive function to force FFI evaluation and updating
  static Fun _ffiSeq = new Fun (delegate {
    var c = stack.Pop ();
    var n = stack.Pop ();

    stack.Push (new Fun (delegate {
      var myreg = reg;
      c.f = delegate {
        reg = myreg;
        return stack.Pop (); };
      return n; }));

    return c; });

  // Literal constructor for some p which simply assigns reg
  static Fun lit (dynamic p) {
    return new Fun (delegate {
      reg = p;
      return stack.Pop (); }); }

  // FFI wrapper, lifts a native function to a Kleisli arrow in IO
  static Fun ffi (Func<dynamic, dynamic> f) {
    return new Fun (delegate {
      stack.Push (_returnIO);
      stack.Push (new Fun (delegate {
        var p = stack.Pop ();
        stack.Push (new Fun (delegate {
          reg = f (reg);
          return stack.Pop (); }));
        return p; }));
      return _runIO; }); }

  // Unsafe termination primitive
  static Fun _unsafeTermination = new Fun (delegate {
    throw new Exception ("Improper termination of IO action"); });
}
