// -unused -no-style
class Main {
  public static void main(String[] a) {}
}

class B {
  public int[] overriden(int a) { return new int[10]; }
}

// Invalid overloading
// The return types don't match.
class A extends B {
  public int overriden(int a) { return 1; }
}

// The parameter types don't match.
class I {
  public int foo(int a, int[] b) { return 3; }
}

class H extends I {
  public int foo(int a) { return 2; }
}

class G extends H {
  public int foo() { return 1; }
}
