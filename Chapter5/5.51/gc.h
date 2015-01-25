#ifndef GC_H
#define GC_H
#include "basic.h"
#define MAX_MEMORY 256

item memory1[MAX_MEMORY];
item memory2[MAX_MEMORY];
item memory3[MAX_MEMORY];
item memory4[MAX_MEMORY];

void begin_garbage_collection();
void relocate_old_result_in_new();

#endif