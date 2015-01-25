#ifndef PARSING_H
#define PARSING_H
#include "basic.h"

char * read();
item tokenized(char *str);
item parse(item *alist);

#endif