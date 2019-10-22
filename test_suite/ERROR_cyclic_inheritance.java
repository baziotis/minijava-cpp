// -unused -no-style
class Main {
    public static void main(String[] a) {
    }
}

// A -> B -> A
// Also: B -> A -> B

class A extends B {
}

class B extends A {
}

// -------- More complex --------
// C -> E -> D -> C
// D -> C -> E -> D
// E -> D -> C -> E
class C extends E {
}

class D extends C {
}

class E extends D {
}
