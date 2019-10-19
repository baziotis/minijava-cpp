class Main {
    public static void main(String[] a) {
    }
}

// ---------------------------
// Shadow field of parent class
// ---------------------------

class A extends B {
    // Shadow field `a` of parent class `B`.
    int a;

    public int test() {
        return a;
    }
}

class B {
    int a;
}

// ---------------------------
// Shadow field of current class inside method
// ---------------------------

class C extends D {
    // Shadow field `a` of parent class `D`.
    int[] a;

    public int[] test() {
        return a;
    }

    public C test2() {
        // Shadow field `a` in method.
        C a;
        return a;
    }
}

class D {
    int a;
}

// ---------------------------
// Same id of field of current
// and method of parent.
// ---------------------------

class E extends F {
    // A field of the current class and
    // a method of the parent class can
    // have the same id.
    int a;
}

class F {
    public int a() {
        return 1;
    }
}

// ---------------------------
// Method Overriding
// ---------------------------

class G extends H {
    public int foo() {
        return 1;
    }
}

class H extends I {
    public int foo(int a) {
        return 2;
    }
}

class I {
    public int foo(int a, int[] b) {
        return 3;
    }
}
