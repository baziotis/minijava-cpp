// -unused -no-style -codegen
class Main {
    public static void main(String[] args) {
      A a;
      boolean b;
      a = new A();
      b = a.test(2);
      if (b) { // false
        System.out.println(1);
      } else {
        System.out.println(0);
      }
    }
}

class A {
    public boolean test(int a) {
        return !(3 < (a + 2)) && !false;
    }
}
