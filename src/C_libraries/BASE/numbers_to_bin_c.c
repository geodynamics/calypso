
/* numbers_to_bin_c.c */

#include "numbers_to_bin_c.h"

void byte_swap(int lengh_byte, char *array){
    int i;
    unsigned char tmp0, tmp1;

    for(i = 0; i < lengh_byte/4; i++){
        tmp0 = array[4*i  ]; 
        tmp1 = array[4*i+1]; 
        array[4*i  ] = array[4*i+3]; 
        array[4*i+1] = array[4*i+2]; 
        array[4*i+2] = tmp1; 
        array[4*i+3] = tmp0; 
    };

    return;
}

void uchar_2bin(unsigned char hex, char bin[9]){
	int itmp;
	char rev[9];
	int i, j;
	
	itmp = (int)hex;
	for (j = 0; j < 8; j++) {rev[j] = '0';};
	rev[8] = '\0';
	
	i = 0;
	while(itmp){
		rev[i++] = itmp%2 + '0';
		itmp/=2;
	};
	
	for (j = 0; j < strlen(rev); j++) {
		i = (int) strlen(rev)-(j+1);
		bin[j] = rev[i];
	};
	i = (int) strlen(rev);
	bin[i] = '\0';
	
	return;
}

void uchar_2binarray(unsigned char hex, int ibin[8]){
	int itmp;
	int  irev[8];
	int i, j;
	
	itmp = (int)hex;
	for (j = 0; j < 8; j++) {irev[j] = 0;};
	
	i = 0;
	while(itmp){
		irev[i++] = itmp%2;
		itmp/=2;
	};
	
	for (j = 0; j < 8; j++) {
		i = 7-j;
		ibin[j] = irev[i];
	};
	
	return;
}

void dec2bin(int dec, char *bin){
	int itmp;
	char rev[2096];
	int i, j;
	
	itmp = dec;
	i = 0;
	while(itmp){
		rev[i++] = itmp%2 + '0';
		itmp/=2;
	};
	rev[i] = '\0';
	for (j = 0; j < strlen(rev); j++) {
		i = (int) strlen(rev)-(j+1);
		bin[j] = rev[i];
	};
	i = (int) strlen(rev);
	bin[i] = '\0';
	
	return;
}

int charhex2bin(char *hex, char *bin){
	int dec, itmp;
	char rev[2096];
	int i, j;
	
	dec = (int) strtol(hex,NULL,16);
	itmp = dec;
	i = 0;
	while(itmp){
		rev[i++] = itmp%2 + '0';
		itmp/=2;
	};
	rev[i] = '\0';
	for (j = 0; j < strlen(rev); j++) {
		i = (int) strlen(rev)-(j+1);
		bin[j] = rev[i];
	};
	i = (int) strlen(rev);
	bin[i] = '\0';
	
	return dec;
}

double round_2_3digit(double value){
    double rounded_3;
    int i_log10, i_round3;
    
    i_log10 = (int) log10(fabs(value)) - 3;
    i_round3 = (int) (value * pow(10.0,(-i_log10)) + 0.5);
    rounded_3 =  ((double) i_round3) * pow(10.0,(double) i_log10);
    return rounded_3;
}
