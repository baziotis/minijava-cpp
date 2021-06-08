// -unused -no-style -codegen
class Main {
    // Constant propagate ndx
    public static void main(String[] a) {
        int ndx;
        int[] arr;
        ndx = 8;
        arr = new int[10];
        System.out.println(ndx); // 8
        System.out.println(arr[1]); // 0
        arr[ndx] = 3;
        System.out.println(arr[8]); // 3
    }
}
