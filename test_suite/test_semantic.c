#include <assert.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int streq(const char *a, const char *b) { return !strcmp(a, b); }

int ends_with(const char *str, const char *needle, int *len) {
  assert(str);
  assert(needle);
  int nlen = strlen(needle);
  int slen = strlen(str);
  *len = slen;
  if (!nlen || !slen)
    return 0;
  if (slen < nlen)
    return 0;
  str = str + slen - nlen;
  while (*str) {
    if (*str++ != *needle++)
      return 0;
  }
  return 1;
}

int main() {
  DIR *src;
  struct dirent *entry;

  src = opendir("./");
  while ((entry = readdir(src))) {
    int namelen;
    if (ends_with(entry->d_name, ".java", &namelen)) {
      char buf[512];
      struct stat st;
      printf("-- Semantic -- %s\n", entry->d_name);
      sprintf(buf, "./%.*s.out", namelen - 5, entry->d_name);
      stat(buf, &st);
      if (access(buf, F_OK) == -1) {
        printf("\t\033[1;31m No .out \033[0m\n");
        continue;
      }
      sprintf(buf, "../main %s -offsets -no-style > curr_out", entry->d_name);
      system(buf);
      sprintf(buf, "diff curr_out ./%.*s.out > curr_diff", namelen - 5, entry->d_name);
      system(buf);
      system("rm curr_out");
      stat("curr_diff", &st);
      if (st.st_size != 0) {
        printf("MISMATCH in %s\n", entry->d_name);
        break;
      } else {
        printf("\t\033[1;32m SUCCESS \033[0m\n");
        system("rm curr_diff");
      }
    }
  }
  closedir(src);

  return (0);
}
