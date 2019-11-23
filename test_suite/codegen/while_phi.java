// -unused -no-style -codegen
class Main {
    public static void main(String[] args) { }
}

class A {
    public int foo() {
        int a;
        a = 3;
        while (a < 4) {
            a = a + 1;
        }
        return 1;
    }
    
    public int bar(int a, boolean cond) {
        int b;
        while (cond) {
            b = a;
            if (cond) {
                a = 2;
            } else { }
        }
        return 1;
    }
}
