// -no-style -undefined -codegen
class Main {
    public static void main(String[] a) { }
}

class A {
    public A foo() {
        A a;
        int[] b;
        int c;
        a = new A();
        b = new int[10 + 4];
        return a;
    }
}
