#include <stdio.h>
int main(int argc, char const *argv[]) {
   int a=1,b=0,c=0,d=0,e=0,f=0,g=0,h=0;
   l01: b = 57;
   l02: c = b;
   l03: if (a) goto l05;
   l04: goto l09;
   l05: b *= 100;
   l06: b -= -100000;
   l07: c = b;
   l08: c -= -17000;
   l09: f = 1;
   l10: d = 2;
   l11: e = 2;
   l12: g = d;
   l13: g *= e;
   l14: g -= b;
   l15: if (g) goto l17;
   l16: f = 0;
   l17: e -= -1;
   l18: g = e;
   l19: g -= b;
   l20: if (g) goto l12;
   l21: d -= -1;
   l22: g = d;
   l23: g -= b;
   l24: if (g) goto l11;
   l25: if (f) goto l27;
   l26: h -= -1;
   l27: g = b;
   l28: g -= c;
   l29: if (g) goto l31;
   l30: goto end;
   l31: b -= -17;
   l32: goto l09;
   end: printf("h: %d\n", h);
}
