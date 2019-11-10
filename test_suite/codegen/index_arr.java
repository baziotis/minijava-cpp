// -unused -no-style -codegen
class Main {
    public static void main(String[] a) { }
}

class A {
    int[] a;
    public int field_const_index() {
        a[1] = 10;
        return 1;
    }

    public int field_var_index(int ndx) {
        a[ndx] = 10;
        return 2;
    }

    public int local_const_index() {
        int[] b;
        b = new int[20];
        b[3] = 7;
        return 3;
    }

    public int local_var_index(int ndx) {
        int[] b;
        b = new int[20];
        b[ndx] = 7;
        return 4;
    }
}
