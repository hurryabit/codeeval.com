//
//  ClimbingStairs.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 09.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <cstdint>
#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

struct integer {
  vector<uint8_t> digits;

  integer() {
  }

  integer(long unsigned x) {
    while( x > 0 ) {
      digits.push_back(x % 10);
      x /= 10;
    }
  }

  friend integer operator+(const integer& x, const integer& y);

  friend ostream& operator<<(ostream& out, const integer& x);
};

integer operator+(const integer& x, const integer& y) {
  integer z;
  z.digits.reserve(max(x.digits.size(), y.digits.size())+1);
  bool carry = false;
  auto i = x.digits.cbegin(), j = y.digits.cbegin();
  auto max_i = x.digits.cend(), max_j = y.digits.cend();
  while( i != max_i || j != max_j || carry ) {
    uint8_t digit = 0;
    if( i != max_i ) {
      digit += *i;
      ++i;
    }
    if( j != max_j ) {
      digit += *j;
      ++j;
    }
    if( carry )
      digit += 1;
    if(digit < 10 ) {
      z.digits.push_back(digit);
      carry = false;
    }
    else { // 10 <= digit < 20
      z.digits.push_back(digit - 10);
      carry = true;
    }
  }
  return z;
}

ostream& operator<<(ostream& out, const integer& x) {
  if( x.digits.empty() )
    out << "0";
  else {
    for( auto i = x.digits.crbegin(); i != x.digits.crend(); ++i )
      out << (int) *i;
  }
  return out;
}


integer num_stair_climbs(long num_stairs) {
  integer a = 1, b = 1;
  for( int i = 2; i <= num_stairs; ++i ) {
    integer c = a+b;
    b = a;
    a = c;
  }
  return a;
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() )
      cout << num_stair_climbs(stol(line)) << endl;
  }
  file.close();
}
