//
//  PlayWithDNA.c
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 13.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int min(int x, int y) {
  return x < y ? x : y;
}

int edit_distance(const char* str_x, size_t len_x, const char* str_y, size_t len_y) {
  int prev_mem[len_x+1], curr_mem[len_x+1];
  int* prev = prev_mem;
  int* curr = curr_mem;

  for( size_t i = 0; i <= len_x; ++i )
    curr[i] = (int) i;

  for( size_t j = 1; j <= len_y; ++j ) {
    int* temp = prev;
    prev = curr;
    curr = temp;

    curr[0] = (int) j;
    for( size_t i = 1; i <= len_x; ++i ) {
      if( str_x[i-1] == str_y[j-1] )
        curr[i] = prev[i-1];
      else
        curr[i] = 1 + min(curr[i-1], min(prev[i], prev[i-1]));
    }
  }

  return curr[len_x];
}

typedef struct match_t {
  int dist;
  const char* segment;
} match_t;

size_t cmp_match_len;

int cmp_match(const void* x, const void* y) {
  const match_t* mx = (const match_t*) x;
  const match_t* my = (const match_t*) y;
  if( mx->dist < my->dist )
    return -1;
  if( mx->dist > my->dist )
    return 1;
  return strncmp(mx->segment, my->segment, cmp_match_len);
}

void matches(const char* pattern, int max_dist, const char* sequence) {
  size_t len_pattern = strlen(pattern), len_sequence = strlen(sequence);
  size_t max_start = len_sequence - len_pattern;
  match_t matches[max_start+1];
  size_t num_matches = 0;

  for( size_t start = 0; start <= max_start; ++start ) {
    const char* segment = sequence + start;
    int dist = edit_distance(segment, len_pattern, pattern, len_pattern);
    if( dist <= max_dist ) {
      matches[num_matches].dist = dist;
      matches[num_matches].segment = segment;
      ++num_matches;
    }
  }

  cmp_match_len = len_pattern;
  qsort(matches, num_matches, sizeof(match_t), &cmp_match);

  int first = 1;
  for( size_t i = 0; i < num_matches; ++i ) {
    if( first )
      first = 0;
    else
      putchar(' ');
    fwrite(matches[i].segment, sizeof(char), len_pattern, stdout);
  }

  if( first )
    fputs("No match", stdout);
  putc('\n', stdout);
}

int main(int argc, char* argv[]) {
  FILE* file = fopen(argv[1], "r");
  while( !feof(file) ) {
    char pattern[51], sequence[301];
    int max_dist;
    fscanf(file, "%s %d %s", pattern, &max_dist, sequence);
    fgetc(file); // read newline
    matches(pattern, max_dist, sequence);
  }
  fclose(file);
}
