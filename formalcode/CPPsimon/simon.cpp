#include<cstdio>
#include<iostream>
#include <float.h>
#include <math.h>

using namespace std;



double* myfun(int m){
     double* x = new double[m];
     x[0] = 1;
     return x;
}
int main(){

    int m = 10;
    double* a=myfun(m);
    
    
    cout << a[0] << endl;

    return 0;
}
