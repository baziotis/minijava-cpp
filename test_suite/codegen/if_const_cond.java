// -unused -no-style -codegen
class Main {
    public static void main(String[] a) { }
}

// Test that either the "then" body or the "else" body
// is elided if an `if` condition is constant (i.e.
// no branch should be generated because we know
// what body will be executed at compile-time).
// Essentialy, this is a type of dead code elimination
// on essentially on the AST level.
class A {
    public int foo() {
        int a;
        if (1 < 2) {
            a = 2;
        } else {  // this should be completely elided
            a = 3;
        }
        // Notice that because of constant propagation
        // in `a`, we should just see a `ret i32 2` in this
        // function.
        return a;
    }

    public int bar(int[] arr) {
        int a;
        a = 2;
        if (a < 2) {  // this should be completely elided
            arr[2] = 1;
        } else {
            arr[2] = 2;
        }
        return 2;
    }
}
