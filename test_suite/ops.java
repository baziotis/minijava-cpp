// -unused
class Main {
    public static void main(String[] a) {
    }
}

class A {
    public boolean t() {
        return (!(1 < 2)) && (true && false);
    }

    public int t2() {
        return ((1 + 2) + 3) + 4;
    }

    public int lispy() {
        return ((((1) + 2) + 3));
    }

    public boolean t3() {
        int a;
        int b;
        a = 2;  // avoid uninitialized error
        b = 2;  // avoid uninitialized error
        return (349 + 908) < ((23 * a) - (b - 2));
    }

    public boolean t4(int a, int[] b) {
        int[] arr;
        arr = new int[10];  // avoid uninitialized error
        return ( ((29347 + (this.t2()) ) < 12 ) && ( ( (a < (arr[0]) ) && (this.t3()) ) && (this.t4(this.t2(), arr)) ) );
    }
}
