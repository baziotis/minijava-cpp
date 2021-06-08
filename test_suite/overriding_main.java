// -unused -no-style -offsets
class Main {
  public static void main(String[] a) {}
}

class A extends Main {
  int x;
  public int foo() {
    return 1;
  }
}

class B extends A {
  public int bar() {
    return x;
  }
}
