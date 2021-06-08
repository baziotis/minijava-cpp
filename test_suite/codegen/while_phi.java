// -unused -no-style -codegen
class Main {
    public static void main(String[] args) {
        int a;
        A aa;

        a = 3;
        while (a < 4) {
            a = a + 1;
            System.out.println(a);
        }

        aa = new A();
        a = aa.bar(a, true);
    }
}

class A {
    public int bar(int a, boolean cond) {
        int b;
        b = 0;
        while (cond) {
            System.out.println(a);
            System.out.println(b);
            b = a;
            if (cond) {
                a = 2;
                cond = false;
            } else { }
        }
        return 1;
    }
}
