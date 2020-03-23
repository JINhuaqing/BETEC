// To implment the minmax Simon's design with early stopping for efficacy

#include<cstdio>
#include<iostream>
#include <float.h>
#include <math.h>
#include <boost/math/distributions/binomial.hpp>

using namespace std;
using boost::math::binomial;
//using namespace boost;

double cdfbinomial(double theta, int n, int k);
double PET(double theta, int r1, int l1, int n1);
double ESS(double theta, int r1, int l1, int n1, int n);
double RJdrug(double theta, int r1, int l1, int n1, int r, int n);
int* Simonph2Eff(double p0, double p1, double alpha, double beta);

double cdfbinomial(double theta, int n, int k){
    if (k <= n)
        return cdf(binomial(n, theta), k);
    else
        return 1;
}

double PET(double theta, int r1, int l1, int n1){
    double itm1 = cdfbinomial(theta, n1, r1-1);
    double itm2 = 1 - cdfbinomial(theta, n1, l1);
    return itm1 + itm2;
}

double ESS(double theta, int r1, int l1, int n1, int n){
    return (1-PET(theta, r1, l1, n1))*(n-n1) + n1;
}

double RJdrug(double theta, int r1, int l1, int n1, int r, int n){
    double itm1 = cdf(binomial(n1, theta), r1-1);
    int upb = ((l1+1)>r)?(r-1):l1;
    double itm2=0;
    for (int i=r1; i<= upb; i++){
        double v=0;
        v = pdf(binomial(n1, theta),i) * cdfbinomial(theta, n-n1, r-i-1);
        itm2 += v;
    }
    return itm1 + itm2; 
}

int* Simonph2Eff(double p0, double p1, double alpha, double beta){
    int* res = new int[5];
    int flag = 0;
    double cESS0 = 3000;
    for (int n=1; n<= 200; n++){
        for (int n1=1; n1<=n-1; n1++){
            for (int r1=1; r1<n1; r1++){
                for (int l1=n1;l1>r1; l1--){
                    for (int r=n; r>l1; r--){
                      //  cout << "r1 " << r1 << " l1 " << l1 << " n1 " << n1 << " r " << r << " n " << n << endl;
                        double typ1 = 1-RJdrug(p0, r1, l1, n1, r, n);
                        double typ2 = RJdrug(p1, r1, l1, n1, r, n);
                        if (typ1 < alpha && typ2 < beta){
                            double tESS0 = ESS(p0, r1, l1, n1, n);
                            if (tESS0 <= cESS0){
                                cout << "The expected sample size under p0 is " << tESS0 << endl;
                                res[0] = r1;
                                res[1] = l1;
                                res[2] = n1;
                                res[3] = r;
                                res[4] = n;
                                cESS0 = tESS0;
                            }
                            
                            flag = 1;
                            break;
                        }
                    }
                }
            }
        }    
        if (flag == 1) break;
    }
    return res;
}


int main(){

    int n = 10;
    double p = 0.9;
    int k = 10;
    int ns[3]={1, 2, 3};
    double p0 =0.2;
    double p1 =0.4;
    double alpha =0.1;
    double beta =0.1;
    int* res = Simonph2Eff(p0, p1, alpha, beta);
    
    cout << "r1: " << res[0] 
         << ". l1: " <<  res[1]
         << ". n1: " <<  res[2]
         << ". r: " <<  res[3]
         << ". n: " <<  res[4]
        << endl;

    cout << "The expected sample size under p0 is " << ESS(p0, res[0], res[1], res[2], res[4]) << endl;
    return 0;
}
