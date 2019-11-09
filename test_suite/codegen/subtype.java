// -unused -no-style -codegen
class Main {
    public static void main(String[] a) { }
}

class A extends B {
    int c;
    public B test_int() {
        B b;
        b = new A();
        return b;
    }

    public int foo(int a) {
        return 2;
    }
}

class B { }
