// -unused -no-style -codegen
class Main {
    public static void main(String[] a) { }
}

class A {
    int i;
    boolean flag;
    int j;
    public int foo() {
        return 1;
    }
    public boolean fa() {
        return false;
    }
}

class B extends A {
    C type;
    int k;
    public int foo() {
        return 2;
    }
    public boolean bla() {
        return true;
    }
}

class C {
}
