class stefanos {
    public static void main(String[] a) {
        int a;
        if (a) {
        } else { }
        System.out.println(a);
    }
}

class A extends B {
    boolean a;
    int b;
    int[] b;

    public int test(int c, int[] d, boolean) {
        int c;
        a = 3;
        return foo;  // <- this should be C, check class B
    }

    public int test(int c, int[] d, boolean) {
        int c;
        a = 3;
        return 1;
    }
}

class C {
    boolean b;
    D d2;
    
    public int test2(D d, int e) {
        int c;
        return new int [10];
    }

    public int test4(D d, int d) {
        int c;
        return ;
    }

    public int test_length(int[] e) {
        return e.length;  // e must be array (here it is)
    }

    public boolean test_not(int e) {
        return !e;  // Error
    }

    public boolean test_and(int e) {
        return true && e;  // Error, right operand is not boolean
    }

    public boolean test_cmp_correct(int e, int f) {
        return e < f;  // Correct
    }

    public boolean test_cmp_wrong(boolean e, int f) {
        return e < f;  // Wrong
    }

    public int test_plus_correct(int a, int b) {
        return a + b;
    }

    public int test_plus_wrong(int a, int[] b) {
        return a + b;
    }

    public int test_minus_correct(int a, int b) {
        return a - 1;
    }

    public int test_minus_wrong(int a, B b) {
        return a - b;
    }

    public int test_times_correct(int a) {
        return a * 2;
    }

    public int test_times_wrong(boolean a, int b) {
        return a * b;
    }

    public int test_arr_look_correct(int[] a, int b) {
        return a[b];
    }

    public int test_arr_look_wrong(int[] a, int[] b) {
        return a[b];
    }

    public int test_arr_look_wrong2(int a, int b) {
        return a[b];
    }
}

class D {
    boolean a;
}

class A {
    boolean;
    
    public int test3(int, int d) {
        int c;
        return 2;
    }
}

class B {
    C foo;
}
