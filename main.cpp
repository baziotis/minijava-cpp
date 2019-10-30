#include <stdio.h>

#include <fstream>
#include <string>
#include <cstring>
#include <sstream>

#include "ast.h"
#include "common.h"
#include "parse.h"
#include "typecheck.h"

config_t config;

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

int main(int argc, char **argv) {
    filename = argv[1];

    config.log = false;
    config.ansi_style = true;
    config.unused = true;
    config.offsets = false;
    config.codegen = false;

    for (int i = 1; i != argc; ++i) {
        if (!strcmp(argv[i], "-v")) {
            config.log = true;
        }
        if (!strcmp(argv[i], "-unused")) {
            config.unused = false;
        }
        if (!strcmp(argv[i], "-no-style")) {
            config.ansi_style = false;
        }
        if (!strcmp(argv[i], "-offsets")) {
            config.offsets = true;
        }
        if (!strcmp(argv[i], "-codegen")) {
            config.codegen = true;
        }
    }

    Goal goal = parse_and_return_goal();

    // TODO: Check for fatal parsing errors here and don't continue
    // if they exist.
    
    bool typecheck_correct = typecheck(&goal);
}
