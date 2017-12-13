#include <stdio.h>

const int knot_length = 256;

void reverse(int* list, int start, int end)
{
   int h;
   while (end > start)
   {
      h = list[start%knot_length];
      list[(start++)%knot_length] = list[end%knot_length];
      list[(end--)%knot_length] = h;
   }
}

void reverse_length(int* list, int start, int len)
{
   reverse(list, start, start+len-1);
}

void partOne(int* list)
{
   printf("%d\n", list[0]*list[1]);
}

int main(int argc, char const *argv[])
{
   int list[knot_length];
   for (int i = 0; i < knot_length; i++) list[i] = i;
   int current_position = 0;
   int skip_size = 0;
   int input[] = {70,66,255,2,48,0,54,48,80,141,244,254,160,108,1,41};
   int length;
   for (int i = 0; i < 16; i++)
   {
      length = input[i];
      reverse_length(list, current_position, length);
      current_position += length + skip_size;
      skip_size++;
   }
   partOne(list);
   return 0;
}
