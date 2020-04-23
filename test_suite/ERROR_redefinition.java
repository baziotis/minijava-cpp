// -unused -no-style
class Main {
  public static void main(String[] a) {}
}

// Redefinition of field
class A {
  int test;
  boolean a;
  // Error already defined.
  int[] test;
}

// Conflict of method with field in the
// same class.
class B {
  int test;

  // Error: Already defined as a field.
  public int test() { return 1; }
}

// Redefinition of method in the same class.
class C {
  public int test() { return 1; }

  public int[] dummy() { return new int[10]; }

  // Error: Redefinition; We have overriding but
  // not overloading
  public int test(int a) { return a; }
}
