// -unused
class Test1 {
  public static void main(String[] x) {
    boolean rv;
    A a;
    B b;
    b = new B();
  }
}

class A {
  public A test() { return new B(); }
}

class B extends A {
  A type;
  int k;
  public boolean bla() {
    int x;
    return true;
  }
}
