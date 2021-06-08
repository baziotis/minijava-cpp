// -no-style -undefined -codegen
class Main {
    public static void main(String[] args) {
        A a;
        int[] b;
        int c;
        int dummy;
        a = new A();
        dummy = a.set_x(10);
        b = new int[10 + 4];
        b[13] = 7;
        System.out.println(a.get_x());  // Must be 10
        System.out.println(b[7]);  // Must be 0
        System.out.println(b[13]); // Must be 7
    }
}

class A {
    int x;

    public int set_x(int _x) {
        x = _x;
        return 0;
    }

    public int get_x() {
        return x;
    }
}
