#include <assert.h>
#include <stdio.h>

#include <fstream>
#include <string>
#include <sstream>

#include "ast.h"
#include "parse.h"
#include "typecheck.h"

const char *filename;

// GENERAL TODO:
// 1) Give option to Buf for an allocator (basically, an arena
//    from alloc.h.

Goal parse_and_return_goal() {
    std::ifstream input_file(filename);
    if (!input_file.good()) {
        assert(0);
    }
    std::stringstream sstr;
    sstr << input_file.rdbuf();
    std::string file_contents_str = sstr.str();
    input_file.close();

    const char *file_contents = file_contents_str.c_str();

    parse_init(file_contents, filename);
    Goal goal = parse_goal();
    return goal;
    // Here `file_contents_str` memory gets deallocated because
    // of destructor.
}

int main() {
    filename = "test.java";

    Goal goal = parse_and_return_goal();

    // TODO: Check for fatal parsing errors here and don't continue
    // if they exist.
    
    typecheck_init();   
    install_type_declarations(goal);
}

/* For a method
- If we don't have overloading, check that the name does not exist (otherwise
  check that if it exists, they have the same return type).
- When parsing the parameters, verify that a parameter with the same name does
  not exist. _However_, I think that a local variable of the class can exist
  and the parameter overrides it. The same thing is for local variables of
  the method.
*/
