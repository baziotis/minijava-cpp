//  -unused
class Main {
    public static void main(String[] a) {
    }
}

class A {
    public int test() {
        B b;
        C c;
        b = c;
        return 1;
    }

    public int test2(B b) {
        return 2;
    }

    public int test3() {
        C c;
        return this.test2(c);
    }
}

class B {
    
}

class C extends B {
    
}
