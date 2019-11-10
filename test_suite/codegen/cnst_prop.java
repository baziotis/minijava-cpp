// -unused -no-style -codegen
class Main {
    public static void main(String[] a) { }
}

class A {
    int[] a;
    // Constant propagate ndx
    public int constant_propagate_integer() {
        int ndx;
        int[] arr;
        ndx = 8;
        arr = new int[10];
        arr[ndx] = 3;
        return 1;
    }
}
