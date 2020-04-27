#include <assert.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int streq(const char *a, const char *b) {
    return !strcmp(a, b);
}

int ends_with(const char *str, const char *needle, int *len) {
    assert(str);
    assert(needle);
    int nlen = strlen(needle);
    int slen = strlen(str);
    *len = slen;
    if (!nlen || !slen) return 0;
    if (slen < nlen) return 0;
    str = str + slen - nlen;
    while (*str) {
        if (*str++ != *needle++) return 0;
    }
    return 1;
}

int main()
{
    DIR *src;
    struct dirent *entry;

    src = opendir("./");
    assert(src);
    while ((entry = readdir(src)))
    {
        int namelen;
        if (ends_with(entry->d_name, ".java", &namelen))
        {
            struct stat st;
            char buf[512];
            printf("-- Codegen -- %s\n", entry->d_name);
            sprintf(buf, "./%.*s.ll", namelen - 5, entry->d_name);
            stat(buf, &st);
            if (access(buf, F_OK) == -1) {
              printf("\t\033[1;31m No .ll \033[0m\n");
              continue;
            }
            sprintf(buf, "../../main %s -codegen > curr_out", entry->d_name);
            system(buf);
            sprintf(buf, "diff curr_out ./%.*s.ll > curr_diff", namelen - 5, entry->d_name);
            system(buf);
            stat("curr_diff", &st);
            if (st.st_size != 0) {
                printf("MISMATCH2 in %s\n", entry->d_name);
                break;
            } else {
                printf("\t\033[1;32m SUCCESS \033[0m\n");
                system("rm curr_diff");
                system("rm curr_out");
            }

            /*
            // Test the output
            // Copy the file to Main.java 'cause all classes are Main
            sprintf(buf, "~/llvm-project/bin/clang ../inputs/codegen/%.*s.ll -o test -Wno-override-module", namelen - 5, entry->d_name);
            system(buf);
            sprintf(buf, "./test > curr_out");
            system(buf);
            sprintf(buf, "../inputs/codegen/%.*s.out", namelen - 5, entry->d_name);
            if (access(buf, F_OK) == -1) {
                printf("\t\033[1;31m No .out \033[0m\n");
                continue;
            }
            sprintf(buf, "diff ../inputs/codegen/%.*s.out curr_out > curr_diff", namelen - 5, entry->d_name);
            system(buf);
            stat("curr_diff", &st);
            if (st.st_size != 0) {
                printf("MISMATCH2 in %s\n", entry->d_name);
                return 1;
            } else {
                printf("\t\033[1;32m SUCCESS \033[0m\n");
                system("rm curr_diff");
                system("rm ./curr_out");
            }
            */
        }
    }
    closedir(src);

    return(0);
}
