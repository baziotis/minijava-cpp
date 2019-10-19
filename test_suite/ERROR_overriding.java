class Main {
    public static void main(String[] a) {
    }
}

// Invalid overloading
// The return types don't match so the
// parameters (and their types) should
// not match as well so they can be
// disambiguated.
class A extends B {
    public int overriden(int a) {
        return 1;
    }
}

class B {
    public int[] overriden(int a) {
        return new int[10];
    }
}
