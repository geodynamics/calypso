
/* numbers_to_bin_c.c */

#include "numbers_to_bin_c.h"

void byte_swap_4(const size_t lengh_byte, char *array){
    long i;
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

void byte_swap_8(const size_t lengh_byte, char *array){
    long i;
    unsigned char tmp0, tmp1, tmp2, tmp3;
    
    for(i = 0; i < lengh_byte/8; i++){
        tmp0 = array[8*i  ]; 
        tmp1 = array[8*i+1]; 
        tmp2 = array[8*i+2]; 
        tmp3 = array[8*i+3]; 
        
        array[8*i  ] = array[8*i+7]; 
        array[8*i+1] = array[8*i+6]; 
        array[8*i+2] = array[8*i+5]; 
        array[8*i+3] = array[8*i+4]; 

        array[8*i+4] = tmp3; 
        array[8*i+5] = tmp2; 
        array[8*i+6] = tmp1; 
        array[8*i+7] = tmp0; 
    };
    return;
}

void swap_4byte(char *buf8){
    int j;
    char tmpbuf[4];
    
    for(j=0;j<4;j++){tmpbuf[j] = buf8[j];};
    for(j=0;j<4;j++){buf8[j] =   buf8[j+4];};
    for(j=0;j<4;j++){buf8[j+4] = tmpbuf[j];};
    /*
     long tako = 16;
     char *ctako;
     ctako = (char *) &tako;
     printf("ctako ");
     for(i=0;i<8;i++){printf("%d ", ctako[i]);};
     printf("\n");
     */
    
    return;
}

void swap_fortran_64bit(int ilength, char *buf){
    int i;
    for(i=0;i<ilength/8;i++){swap_4byte((char *) &buf[8*i]);}
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

void find_order_digit(double d_in, double *d_out, int *i_digit){
	*d_out = d_in;
	*i_digit = 0;
	
	if(d_in == 0.0){
		return;
	} else if (fabs(*d_out) < 1.0){
		*d_out = d_in;
		while (fabs(*d_out) < 1.0){
			*i_digit = *i_digit - 1;
			*d_out = *d_out * 10.0;
		};
	} else {
		while (fabs(*d_out) >= 10.0){
			*i_digit = *i_digit + 1;
			*d_out = *d_out * 0.1;
		};
	}
	return;
}

double const_from_digit_order(double d_in, int  i_digit){
	int i;
	double d_out = d_in;
	if(i_digit < 0){
		for(i=0;i< (-i_digit);i++) d_out = d_out * 0.1;
	}
	else {
		for(i=0;i<i_digit;i++) d_out = d_out * 10.0;
	}
	return d_out;
}
