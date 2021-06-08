// -unused -no-style -codegen
class Main {
    public static void main(String[] args) {
      A a;
      int dummy;
      int v;
      boolean b;
      int[] arr;

      a = new A();
      dummy = a.set();
      v = a.test_int();
      System.out.println(v);  // Should be 1
      b = a.test_boolean();
      if (b) { // Should be `false`
        System.out.println(1);
      } else {
        System.out.println(0);
      }

      arr = a.test_arr();
      System.out.println(arr[2]); // Should be 0
      System.out.println(arr[3]); // Should be 2

      a = a.test_obj();
      v = a.test_int();
      System.out.println(v); // Should be 3
    }
}

class A {
    int a;
    boolean b;
    int[] c;
    B bb;
    public int set() {
      int dummy;
      a = 1;
      b = false;
      c = new int[7];
      c[3] = 2;
      bb = new B();
      dummy = bb.set_a();
      return 0;
    }
    public int test_int() {
        return a;
    }
    public boolean test_boolean() {
        return b;
    }
    public int[] test_arr() {
        return c;
    }
    public A test_obj() {
        int dummy;
        dummy = bb.set_a();
        return bb;
    }
}


class B extends A {
  public int set_a() {
    a = 3;
    return 0;
  }
}
