//
//  NumberOfOnes.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 08.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

long count_bits(long number) {
  long count = 0;
  while( number != 0 ) {
    number = number & (number-1);
    ++count;
  }
  return count;
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    long number;
    file >> number;
    if( file.good() )
      cout << count_bits(number) << endl;
  }
  file.close();
}
