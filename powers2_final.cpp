/*
Project     : Programming Languages 1 - Assignment 1 - Exercise 1 
Author      : Eleni-Elpida Kapsali (eleni_kaps@hotmail.com)
Date        : April 29, 2020
Description : Powers2 (C++ code)
------------
School of ECE, National Technical University of Athens
*/

//Source: https://www.geeksforgeeks.org/represent-n-as-the-sum-of-exactly-k-powers-of-two-set-2/

#include<stdio.h>
#include<stdlib.h>

using namespace std;

//This function calculates log2(x)
int log2 (int x){
	if(x > 1) return 1 + log2(x / 2);
	else return 0;
}

int main(int argc, char* argv[]){
FILE *fptr;
fptr = fopen(argv[1], "r");
if(fptr == NULL) printf("Error in opening file");
else{
int T, K, N;
fscanf(fptr, "%d", &T);
for(int i = 0; i < T; i++){
    fscanf(fptr, "%d", &N);
    fscanf(fptr, "%d", &K);
    int powers[K], sumpow = K;
    for(int j = 0; j < K; j++) powers[j] = 1;
    for(int j = (K-1); j >= 0; --j){
        while(sumpow + powers[j] <= N){
            sumpow += powers[j];
            powers[j] *= 2;
        }
    }
    if(sumpow != N) printf("[]\n"); 
    else{
    	int size = log2(powers[K-1]) + 1;
    	int powers2[size];
    	for(int i = 0; i < size; i++) powers2[i] = 0;
        for(int i = 0; i < K; i++) powers2[log2(powers[i])]++;
		printf("[");
		for(int i = 0; i < (size - 1); i++) printf("%d,", powers2[i]);
		printf("%d]\n", powers2[size-1]);
	}
}
}
return 0;
}
       
