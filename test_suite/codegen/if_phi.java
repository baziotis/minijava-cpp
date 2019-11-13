// -unused -no-style -codegen
class Main {
    public static void main(String[] a) { }
}

class A {
    public int foo(boolean cond) {
        int a;
        a = 7;
        if (cond) {
            if (cond) {
                a = 2;
            } else {}
        } else {
        }
        return a;
    }

    public int bar(boolean cond) {
        int a;
        a = 7;
        if (cond) {
            if (cond) {
            } else {
                a = 2;
            }
        } else {
        }
        return a;
    }

    public int fa(boolean cond) {
        int a;
        a = 7;
        if (cond) {
            a = 3;
        } else {
            a = 4;
        }
        return a;
    }

    public int test1(boolean cond) {
        A a;
        a = new A();
        if (cond) {
            a = new A();
        } else {
            if (cond) {
            } else {
                a = new A();
            }
        }
        return 5;
    }
}
