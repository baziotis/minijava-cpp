// -unused -no-style
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

    public int lispy(int[] a) {
        return ((((1) + 2) + (a[3])));
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

    public int t5(int[] a) {
        int b;
        b = new int [(new int[(this.t2()) + (this.lispy(new int [a[0]]))][0]) + 10][2];
        return a[b];
    }

    // Like t4() but more crazy (involving overriding methods)
    public boolean t6(boolean dummy, int[] arr) {
        int a;
        C c;
        a = 2;  // avoid uninitialized error
        c = new C();  // avoid uninitialized error
        return ( ((29347 + (this.t2()) ) < 12 ) && ( ( (a < (arr[0]) ) && (this.t3()) ) && (this.t6(this.t4( (new B().test(c.test(true)))[0], arr ), new int [arr[0]])) ) );
    }
}

class B extends C {
    public int[] test(int i) {
        return new int[i];
    }
}

class C {
    public int test(boolean a) {
        return 1;
    }
}
