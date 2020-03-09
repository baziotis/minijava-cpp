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

int check_mismatch(const char *name) {
  struct stat st;
  stat("curr_diff", &st);
  if (st.st_size != 0) {
      printf("MISMATCH in %s\n", name);
      return 0;
  }
  return 1;
}

void test_dir(const char *dir) {
    DIR *src;
    struct dirent *entry;

    src = opendir(dir);
    while ((entry = readdir(src)))
    {
        int namelen;
        if (ends_with(entry->d_name, ".java", &namelen))
        {
            FILE *fp = fopen(entry->d_name, "r");
            char params[64];
            // Read until next line; the compilation flags
            fscanf(fp, "%[^\n]", params);
            assert(params[0] = '/' && params[1] == '/' && params[2] == ' ');
            char *par_ptr = params;
            par_ptr += 3;
            fclose(fp);
            char buf[512];
            sprintf(buf, "../main %s %s > curr_out", entry->d_name, par_ptr);
            system(buf);
            sprintf(buf, "diff curr_out %.*s.out > curr_diff", namelen - 5, entry->d_name);
            system(buf);
            if (!check_mismatch(entry->d_name))
              break;
            sprintf(buf, "opt -S curr_out > opt_out");
            system(buf);
            system("rm curr_diff");
            system("rm curr_out");
            system("rm opt_out");
        }
    }
    closedir(src);
}

int main()
{
    test_dir("./");
    return(0);
}
