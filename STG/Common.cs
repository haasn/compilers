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
  static Fun ffi (int arity, Func<dynamic[], dynamic> f) {
    return new Fun (delegate {
      // The arrow takes ‘arity’ parameters..
      Fun[] vars = new Fun[arity];
      for (int i = 0; i < arity; i++) {
        vars[i] = stack.Pop(); }

      // ..and creates an n-tuple which simply applies them all to their cont
      stack.Push(new Fun(delegate {
        var cont = stack.Pop();
        foreach (Fun a in vars)
          stack.Push (a);
        return cont; }));

      stack.Push (_returnIOF);

      // The impure ffi action needs to be lifted to work with n-tuples
      stack.Push (new Fun (delegate {
        var i = stack.Pop ();
        stack.Push (liftn (arity, new dynamic[arity], f));
        return i; }));

      return _runIO; }); }

  // Impure function lifter: accepts ‘arity’ arguments, evaluates them all
  // and stores them in a temporary arguments array, which is finally passed
  // to the impure function ‘f’
  static Fun liftn (int arity, dynamic[] args, Func<dynamic[], dynamic> f) {
    return new Fun (delegate {
      if (arity < args.Length)
        args[args.Length - 1 - arity] = reg;

      if (arity == 0) {
        reg = f (args);
        return stack.Pop (); }

      else {
        var p = stack.Pop ();
        stack.Push (liftn (arity - 1, args, f));
        return p; }}); }

  // Unsafe termination primitive
  static Fun _unsafeTermination = new Fun (delegate {
    throw new Exception ("Improper termination of IO action"); });
}
