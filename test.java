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

    public int[] test5(int[] e) {
        return e.length;
    }

    public int[] test6(int e) {
        return !e;  // Error
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
