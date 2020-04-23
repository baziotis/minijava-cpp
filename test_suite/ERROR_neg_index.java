// -unused -no-style
class Main {
  public static void main(String[] a) {}
}

class A {
  public int test(int[] a) {
    // There's nothing like -1 (i.e. there's no unary `-`)
    // in MiniJava and so we have to construct -1 as a binary
    // expression.
    return a[(0 - 1)];
  }
}
