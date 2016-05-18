#ifndef WITH_CACHE_H
#define WITH_CACHE_H
#define SIMULATE_MEM
#undef SIMULATE_MEM

#define CACHE_SET_BIT 4
#define CACHE_BLOCK_BIT 4
#define CACHE_TAG_BIT (32 - CACHE_SET_BIT - CACHE_BLOCK_BIT)
#define WAY 4
#define CSET (1 << CACHE_SET_BIT)
/* number of word-sized block */
#define CACHE_BLOCK_COUNT ((1 << CACHE_BLOCK_BIT) / sizeof(word_t))

#define GET_TAG(ADDR) (((unsigned) ADDR) >> (CACHE_SET_BIT + CACHE_BLOCK_BIT))
#define GET_SET_NDX(ADDR) ((ADDR >> CACHE_BLOCK_BIT) & ((1 << CACHE_SET_BIT) - 1))
#define GET_OFFSET(ADDR) (ADDR & ((1 << CACHE_BLOCK_BIT) - 1))

#define MADDR(TAG, SET, OFFSET) ((TAG << (CACHE_SET_BIT + CACHE_BLOCK_BIT)) || (SET << CACHE_BLOCK_BIT) || OFFSET)

enum cache_option {
	L, S
};

typedef struct cache_line {
#ifdef SIMULATE_MEM
	unsigned int data[CACHE_BLOCK_COUNT];
#endif
	unsigned int tag:CACHE_TAG_BIT;
	unsigned int dirty:1;
	unsigned int valid:1;
	unsigned int ref_count:(46-CACHE_TAG_BIT);
	struct cache_line *next;
} cache_line_t;

typedef struct {
	cache_line_t *first;
} cache_set_t;
#endif
