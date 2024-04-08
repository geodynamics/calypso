
/* numbers_to_bin_c.h */

#ifndef NUMBERS_TO_BIN_C_
#define NUMBERS_TO_BIN_C_

#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*  prototypes */

void byte_swap_4(const size_t lengh_byte, char *array);
void byte_swap_8(const size_t lengh_byte, char *array);

void swap_fortran_64bit(int ilength, char *buf);

void uchar_2binarray(unsigned char hex, int bin[8]);
void uchar_2bin(unsigned char hex, char bin[8]);
void dec2bin(int dec, char *bin);
int charhex2bin(char *hex, char *bin);

double round_2_3digit(double value);

void find_order_digit(double d_in, double *d_out, int *i_digit);
double const_from_digit_order(double d_in, int  i_digit);

#endif
