#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int charToInt(char a)
{
   switch (a) {
      case '1': return 1;
      case '2': return 2;
      case '3': return 3;
      case '4': return 4;
      case '5': return 5;
      case '6': return 6;
      case '7': return 7;
      case '8': return 8;
      case '9': return 9;
      default: fprintf(stderr, "%s\n", "something went terribly wrong!!!");
   }
   return -1;
}

int main(int argc, char const *argv[]) {
   char* captcha    = calloc(sizeof(char), 4096);
   scanf("%s\n", captcha);
   int  length      = strlen(captcha);
   int  half_length = length/2;
   int  score       = 0;
   for (int i = 0; i < length; i++)
   {
      if (captcha[i] == captcha[(i+half_length)%length])
      {
         score += charToInt(captcha[i]);
      }
   }
   printf("score: %d\n", score);
   free(captcha);
   return 0;
}
