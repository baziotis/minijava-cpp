#include <vector>

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "alloc.h"
#include "str_intern.h"

/*
********************************** STRINGS ****************************************
    
    String interning utilities.

***********************************************************************************
    
    String interning is when all representations of a string point to the same
    memory. For example, if I see "foo" for the first time, I save it in a
    hash table. If I see it again, by searching the ht I've found that I've
    seen it again. So, I return this saved strings _pointer_. The impact
    of that is that now all "foo"s have the same memory address and
    so strings can be compared just by comparing the pointers.
*/

struct InternedStr {
    size_t len;
    char *str;
    InternedStr *link;
};

InternedStr *buckets[1024];

// Map characters to random values
int scatter[256] = {
	2078917053, 143302914, 1027100827, 1953210302, 755253631,
	2002600785, 1405390230, 45248011, 1099951567, 433832350,
	2018585307, 438263339, 813528929, 1703199216, 618906479,
	573714703, 766270699, 275680090, 1510320440, 1583583926,
	1723401032, 1965443329, 1098183682, 1636505764, 980071615,
	1011597961, 643279273, 1315461275, 157584038, 1069844923,
	471560540, 89017443, 1213147837, 1498661368, 2042227746,
	1968401469, 1353778505, 1300134328, 2013649480, 306246424,
	1733966678, 1884751139, 744509763, 400011959, 1440466707,
	1363416242, 973726663, 59253759, 1639096332, 336563455,
	1642837685, 1215013716, 154523136, 593537720, 704035832,
	1134594751, 1605135681, 1347315106, 302572379, 1762719719,
	269676381, 774132919, 1851737163, 1482824219, 125310639,
	1746481261, 1303742040, 1479089144, 899131941, 1169907872,
	1785335569, 485614972, 907175364, 382361684, 885626931,
	200158423, 1745777927, 1859353594, 259412182, 1237390611,
	48433401, 1902249868, 304920680, 202956538, 348303940,
	1008956512, 1337551289, 1953439621, 208787970, 1640123668,
	1568675693, 478464352, 266772940, 1272929208, 1961288571,
	392083579, 871926821, 1117546963, 1871172724, 1771058762,
	139971187, 1509024645, 109190086, 1047146551, 1891386329,
	994817018, 1247304975, 1489680608, 706686964, 1506717157,
	579587572, 755120366, 1261483377, 884508252, 958076904,
	1609787317, 1893464764, 148144545, 1415743291, 2102252735,
	1788268214, 836935336, 433233439, 2055041154, 2109864544,
	247038362, 299641085, 834307717, 1364585325, 23330161,
	457882831, 1504556512, 1532354806, 567072918, 404219416,
	1276257488, 1561889936, 1651524391, 618454448, 121093252,
	1010757900, 1198042020, 876213618, 124757630, 2082550272,
	1834290522, 1734544947, 1828531389, 1982435068, 1002804590,
	1783300476, 1623219634, 1839739926, 69050267, 1530777140,
	1802120822, 316088629, 1830418225, 488944891, 1680673954,
	1853748387, 946827723, 1037746818, 1238619545, 1513900641,
	1441966234, 367393385, 928306929, 946006977, 985847834,
	1049400181, 1956764878, 36406206, 1925613800, 2081522508,
	2118956479, 1612420674, 1668583807, 1800004220, 1447372094,
	523904750, 1435821048, 923108080, 216161028, 1504871315,
	306401572, 2018281851, 1820959944, 2136819798, 359743094,
	1354150250, 1843084537, 1306570817, 244413420, 934220434,
	672987810, 1686379655, 1301613820, 1601294739, 484902984,
	139978006, 503211273, 294184214, 176384212, 281341425,
	228223074, 147857043, 1893762099, 1896806882, 1947861263,
	1193650546, 273227984, 1236198663, 2116758626, 489389012,
	593586330, 275676551, 360187215, 267062626, 265012701,
	719930310, 1621212876, 2108097238, 2026501127, 1865626297,
	894834024, 552005290, 1404522304, 48964196, 5816381,
	1889425288, 188942202, 509027654, 36125855, 365326415,
	790369079, 264348929, 513183458, 536647531, 13672163,
	313561074, 1730298077, 286900147, 1549759737, 1699573055,
	776289160, 2143346068, 1975249606, 1136476375, 262925046,
	92778659, 1856406685, 1884137923, 53392249, 1735424165,
	1602280572
};

/// Intern a string starting from `str` with length `len`.
const char *str_intern_len(const char *str, size_t len) {
    assert(str);
    size_t i, h;
    const char *end;
    InternedStr *p;

    h = 0;
    i = len;
    // Should point one-past last char after the loop.
    end = str;
    for (; i; --i) {
        h = (h<<1) + scatter[*(uint8_t*)end++];
    }
    constexpr size_t buckets_len = 1024;
    h &= buckets_len - 1;
    for (p = buckets[h]; p; p = p->link) {
        if (p->len == len && !memcmp(str, p->str, len)) {
            return p->str;
        }
    }
    // Pointer to the next free byte the current block.
    static char *next;
    // Pointer to one past the last free byte in the current chunk.
    static char *strlimit;
    // The `=` is because `strlimit` is _one-past_.
    if (len + 1 >= strlimit - next) {
        size_t n = len + 4 * 1024;
        next = (char*)allocate(n, MEM::STR_INTERN);
        strlimit = next + n;
    }
    // We use the current block (the one guarded by `next` and `strlimit`)
    // only to the string data per se. So, for the structs, we have to allocate
    // new memory
    p = (InternedStr*) allocate(sizeof(*p), MEM::STR_INTERN);
    // Point to allocated memory.
    p->str = next;
    // Copy
    memcpy(p->str, str, len);
    // Set `len`
    p->len = len;
    // Set NUL terminator.
    *(p->str + len) = 0;
    // Increment next;
    next += len+1;
    // Put in the start of the chain.
    p->link = buckets[h];
    buckets[h] = p;
    return p->str;
}

/// Intern a NUL-terminated string.
const char *str_intern(const char *str) {
    return str_intern_len(str, strlen(str));
}

/// Intern a string range
const char *str_intern_range(const char *start, const char* end) {
    return str_intern_len(start, end - start);
}
