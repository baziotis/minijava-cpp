CLANG=~/Documents/llvm-project/bin/clang

cd ../../
./compile.sh
cd test_suite/codegen

for i in `find . -name "*.java" -type f`; do
    echo $i
    javac $i || break
    java Main > java_out
    cat java_out
    ../../main $i -codegen -no-style > curr_out.ll
    $CLANG curr_out.ll -o test -Wno-override-module || { echo "Compilation of .ll failed!\n"; break; }
    ./test > curr_out
    diff curr_out java_out > curr_diff

    if [ -s "curr_diff" ] 
    then
    	echo "MISMATCH2!!\n"
      break
    fi

    rm *.class
done

rm curr_out curr_out.ll curr_diff java_out test
