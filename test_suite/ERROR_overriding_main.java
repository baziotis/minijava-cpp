// -unused -no-style -offsets
class Main {
  public static void main(String[] a) {}
}

class A extends Main {
  public int foo() {
    return 1;
  }
}

class B extends A {
  public int main() {
    return 2;
  }
}
