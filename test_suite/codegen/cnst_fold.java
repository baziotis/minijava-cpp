// -unused -no-style -codegen
class Main {
    public static void main(String[] args) {
        int b;
        A aa;

        aa = new A();
        b = aa.test(2);  // 8
        b = 3 + b; // 11
        System.out.println(b);
    }
}

class A {
    public int test(int a) {
        // Constant-fold the RHS part.
        return a + (((1 + 2) + 3));
    }
}
