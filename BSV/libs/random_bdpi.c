#include <stdint.h>
#include <stdlib.h>
#include <time.h>

uint32_t get_time() {
    return (uint32_t)time(NULL);
}

void seed_rng(uint32_t seed) {
    srand(seed);
}

uint32_t urandom() {
    return rand();
}

