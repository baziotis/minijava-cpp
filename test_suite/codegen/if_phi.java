// -unused -no-style -codegen
class Main {
    public static void main(String[] args) {
        int a;
        boolean cond;
        A aa;
        
        cond = true;
        a = 7;
        if (cond) {
            if (cond) {
                a = 2;
            } else {}
        } else {
        }
        // Should be reduced to just 2.
        System.out.println(a);

        aa = new A();
        a = aa.fa();
        System.out.println(a); // 4
    }
}

class A {

    public int fa() {
        boolean cond;
        int a;

        cond = false;
        a = 7;
        if (cond) {
            a = 3;
        } else {
            a = 4;
        }
        // Should be reduced to just 4.
        return a;
    }
}
