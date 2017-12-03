#include <stdio.h>

#define ARRAY_LENGHT 11
int array[ARRAY_LENGHT][ARRAY_LENGHT];

int sum_surrounding(int x, int y)
{
   int sum = 0;
   for (int i = -1; i < 2; i++)
   {
      for (int j = -1; j < 2; j++)
      {
         if ((x+j) < ARRAY_LENGHT && (x+j) >= 0)
         {
            if ((y+i) < ARRAY_LENGHT && (y+i) >= 0)
            {
               sum += array[x+j][y+i];
            }
         }
      }
   }
   if(sum > 289326)
   {
      printf("[solution] %d\n", sum);
      exit(0);
   }
   return sum;
}

int main(int argc, char const *argv[])
{
   for (int i = 0; i < ARRAY_LENGHT; i++)
   {
      for (int j = 0; j < ARRAY_LENGHT; j++) array[i][j] = 0;
   }
   array[ARRAY_LENGHT/2][ARRAY_LENGHT/2] = 1;
   int x = ARRAY_LENGHT/2+1;
   int y = ARRAY_LENGHT/2;
   int h = 2;
   int v = 1;
   int s = 1;
   for(int c = 0; c < ARRAY_LENGHT; c++) {
      for (int i = 0; i < v; i++)
      {
         array[x][y] = sum_surrounding(x, y);
         y += 1 * s;
      }
      v++;
      s *= -1;
      for (int i = 0; i < h; i++)
      {
         array[x][y] = sum_surrounding(x, y);
         x += 1 * s;
      }
      h++;
   }
   for (int i = ARRAY_LENGHT; i > 0; i--)
   {
      for (int j = 0; j < ARRAY_LENGHT; j++)
      {
         printf("%d\t", array[j][i]);
      }
      printf("\n");
   }
   return 0;
}
