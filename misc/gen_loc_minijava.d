import core.stdc.stdio : printf;
import std.string : toStringz;
import std.conv : to;
import std.algorithm : count;

void main(string[] args) {

  if (args.length != 2) {
    printf("Give one cmd argument that is the number of lines " ~
           "to be generated (approximately).\n");
    return;
  }

printf(
"
class Main {
  public static void main(String[] a) {
    System.out.println(1);
  }
}
");

  string str =
"
class A%d {
  public boolean t() { return (!(1 < 2)) && (true && false); }
  public int t2() { return ((1 + 2) + 3) + 4; }
  public int lispy(int[] a) { return ((((1) + 2) + (a[3]))); }
  public boolean t3() {
    int a;
    int b;
    a = 2;
    b = 2;
    return (349 + 908) < ((23 * a) - (b - 2));
  }
  public boolean t4(int a, int[] b) {
    int[] arr;
    arr = new int[10];
    return (((29347 + (this.t2())) < 12) &&
            (((a < (arr[0])) && (this.t3())) && (this.t4(this.t2(), arr))));
  }
  public int t5(int[] a) {
    int b;
    b = new int[(new int[(this.t2()) + (this.lispy(new int[a[0]]))][0]) + 10]
               [2];
    return a[b];
  }
  public boolean t6(boolean dummy, int[] arr) {
    int a;
    C%d c;
    a = 2;
    c = new C%d();
    return (((29347 + (this.t2())) < 12) &&
            (((a < (arr[0])) && (this.t3())) &&
             (this.t6(this.t4((new B%d().test(c.test(true)))[0], arr),
                      new int[arr[0]]))));
  }
}
class C%d {
  public int test(boolean a) { return 1; }
}
class B%d extends C%d {
  public int[] test(int i) { return new int[i]; }
}
";
  
  auto str_loc = str.count('\n'.to!ubyte);
  auto requested_loc = to!int(args[1]);
  auto iterations = requested_loc / str_loc;

  const(char*) strz = toStringz(str);
  for (int i = 0; i < iterations; ++i) {
    printf(strz, i, i, i, i, i, i, i, i);
  }
}
