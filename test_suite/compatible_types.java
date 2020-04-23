// -unused -no-style
class Main {
  public static void main(String[] a) {}
}

class A {
  public int test() {
    B b;
    C c;
    c = new C(); // to avoid uninitialized error
    b = c;
    return 1;
  }

  public int test2(B b) { return 2; }

  public int test3() {
    C c;
    D d;
    c = new C(); // to avoid uninitialized error
    d = new D(); // to avoid uninitialized error
    return (this.test2(c)) + (this.test2(d));
  }
}

class B {}

class C extends B {}

class D extends C {}
