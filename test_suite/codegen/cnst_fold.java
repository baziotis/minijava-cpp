// -unused -no-style -codegen
class Main {
    public static void main(String[] a) { }
}

class A {
    public int test(int a) {
        return a + (((1 + 2) + 3));
    }

    public int foo() {
        // Constant propagating `a` and
        // then constant folding 3 + 2;
        int a;
        int b;
        a = 2;
        b = 3 + a;
        return b;
    }
}
