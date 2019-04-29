
/* numbers_to_bin_c.h */

#ifndef NUMBERS_TO_BIN_C_
#define NUMBERS_TO_BIN_C_

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*  prototypes */

void byte_swap_4(int lengh_byte, char *array);
void byte_swap_8(int lengh_byte, char *array);

void uchar_2binarray(unsigned char hex, int bin[8]);
void uchar_2bin(unsigned char hex, char bin[8]);
void dec2bin(int dec, char *bin);
int charhex2bin(char *hex, char *bin);

double round_2_3digit(double value);
#endif
