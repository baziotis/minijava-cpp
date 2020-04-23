// -unused -no-style
class Main {
  public static void main(String[] a) {}
}

class A {
  public int test() {
    C c;
    c = new C();
    return c.foo(new int[10]);
  }
}

class B {
  public int foo(boolean b) { return 1; }
}

class C extends B {
  public int foo(int i) { return 2; }
}
