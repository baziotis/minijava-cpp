// -unused -no-style
class Main {
  public static void main(String[] a) {}
}

class A {
  public int test() { return new A().foo(); }

  public int foo() { return 1; }
}
