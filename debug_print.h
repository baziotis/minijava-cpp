struct LogScope {
  LogScope();
  ~LogScope();
};

#define LOG_SCOPE LogScope _logscope;

void print_indentation();
void debug_print(const char *fmt, ...);
void debug_line(const char *fmt, ...);
void set_indent_char(int c);
