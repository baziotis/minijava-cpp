// -unused -no-style
class Main {
  public static void main(String[] a) {}
}

// ---------------------------
// Shadow field of parent class
// ---------------------------

class B {
  int a;
}

class A extends B {
  // Shadow field `a` of parent class `B`.
  int a;

  public int test() { return a; }
}

// ---------------------------
// Shadow field of current class inside method
// ---------------------------

class D {
  int a;
}

class C extends D {
  // Shadow field `a` of parent class `D`.
  int[] a;

  public int[] test() { return a; }

  public C test2() {
    // Shadow field `a` in method.
    C a;
    a = new C(); // To avoid uninitialized error
    return a;
  }
}

// ---------------------------
// Same id of field of current
// and method of parent.
// ---------------------------

class F {
  public int a() { return 1; }
}

class E extends F {
  // A field of the current class and
  // a method of the parent class can
  // have the same id.
  int a;
}

/// Make sure that there's no problem having
/// same name in field and method.

class J {
  int x;
  public int x() {
    return x;
  }
}
