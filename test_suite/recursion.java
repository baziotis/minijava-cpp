class Main {
    public static void main(String[] a) {
    }
}

class A {
    int a;  // Unused

    public int test(int[] b, int c) {
        return this.test(b, b[c]);
    }
}
