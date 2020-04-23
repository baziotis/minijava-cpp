// -unused -no-style -offsets
class Main {
  public static void main(String[] a) {}
}

class A {
  int i;
  boolean flag;
  int j;
  public int foo() { return 1; }
  public boolean fa() { return false; }
}

class B extends A {
  A type;
  int k;
  public int foo() { return 2; }
  public boolean bla() { return true; }
}
