// -unused -no-style -codegen
class Main {
    public static void main(String[] args) {
      boolean b;
      A a;
      a = new A();
      b = a.test(true, true, true);
      if (b) {
        System.out.println(1);
      } else {
        System.out.println(0);
      }

      b = a.test(true, false, true);
      if (b) {
        System.out.println(1);
      } else {
        System.out.println(0);
      }
    }
}

class A {
    public boolean test(boolean a, boolean b, boolean c) {
        return a && (b && c);
    }
}
