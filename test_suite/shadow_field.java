class Main {
    public static void main(String[] a) {
    }
}

class A {
    int a;

    public int test() {
        return a;
    }
}

class B {
    int a;
}

// Shadow field in method

class C extends D {
    int[] a;

    public int[] test() {
        return a;
    }

    public C test2() {
        C a;
        return a;
    }
}

class D {
    int a;
}
