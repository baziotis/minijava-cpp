#ifndef COMMON_H
#define COMMON_H

// Represent a file location. This for the time being
// is only an integer denoting the line.
// TODO: Consider what Go does with Pos.
struct location_t {
    int ln;
};

struct config_t {
    bool log;
    bool ansi_style;
    bool unused;  // Issue message for unused types
    bool offsets;
};

#endif
