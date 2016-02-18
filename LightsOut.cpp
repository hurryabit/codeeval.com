//
//  LightsOut.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 18.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <bitset>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

struct Bits128 {
  uint64_t high, low;

  Bits128(uint64_t low_ = 0) : high(0), low(low_) {}
  Bits128(uint64_t high_, uint64_t low_) : high(high_), low(low_) {}
  Bits128(const Bits128& x) : high(x.high), low(x.low) {}

  Bits128& operator|=(const Bits128& x) {
    high |= x.high;
    low  |= x.low;
    return *this;
  }

  size_t count() {
    bitset<128> bits(high);
    bits <<= 64;
    bits |= low;
    return bits.count();
  }
};

Bits128 operator^(const Bits128& x, const Bits128& y) {
  return Bits128(x.high ^ y.high, x.low ^ y.low);
}

Bits128 operator|(const Bits128& x, const Bits128& y) {
  return Bits128(x.high | y.high, x.low | y.low);
}

Bits128 operator<<(const Bits128& x, size_t k) {
  if( k == 0 )
    return x;
  else if( k < 64 )
    return Bits128((x.high << k) | (x.low >> (64-k)), x.low << k);
  else
    return Bits128(x.low << (k-64), 0);
}

bool operator==(const Bits128& x, const Bits128& y) {
  return x.high == y.high && x.low == y.low;
}

bool operator<(const Bits128& x, const Bits128& y) {
  return x.high < y.high || (x.high == y.high && x.low < y.low);
}

ostream& operator<<(ostream& out, const Bits128& x) {
  out << uppercase << hex << "0x";
  if( x.high == 0 )
    out << x.low;
  else {
    out << x.high;
    out.fill('0');
    out.width(16);
    out << x.low;
  }
  out << dec;
  return out;
}

using EquationSystem = vector<pair<Bits128, bool>>;

bool upper_triangle(EquationSystem& eqs) {
  for( size_t i = 0; i < eqs.size(); ++i ) {
    // move the biggest row to the top
    for( size_t j = i+1; j < eqs.size(); ++j ) {
      if( eqs[i].first < eqs[j].first )
        swap(eqs[i], eqs[j]);
    }

    for( size_t j = i+1; j < eqs.size(); ++j ) {
      Bits128 coeffs = eqs[i].first ^ eqs[j].first;
      if( coeffs < eqs[j].first ) {
        bool abs = eqs[i].second ^ eqs[j].second;
        if( coeffs == 0 && abs == 1 )
          return false;
        eqs[j].first = coeffs;
        eqs[j].second = abs;
      }
    }
  }
  return true;
}

void echelon_form(EquationSystem& eqs) {
  for( size_t i = eqs.size() - 1; i > 0; --i ) {
    for( size_t j = 0; j < i; ++j ) {
      Bits128 coeffs = eqs[i].first ^ eqs[j].first;
      if( coeffs < eqs[j].first ) {
        bool abs = eqs[i].second ^ eqs[j].second;
        eqs[j].first = coeffs;
        eqs[j].second = abs;
      }
    }
  }
}

EquationSystem parse(const string& line) {
  size_t height, width;
  string matrix;
  istringstream(line) >> height >> width >> matrix;

  auto var = [&](size_t i, size_t j) { return i*width + j; };

  EquationSystem eqs(height*width);
  for( size_t i = 0; i < height; ++i ) {
    for( size_t j = 0; j < width; ++j ) {
      Bits128 coeffs = Bits128(1) << var(i, j);
      if( i > 0 )
        coeffs |= Bits128(1) << var(i-1, j);
      if( i+1 < height )
        coeffs |= Bits128(1) << var(i+1, j);
      if( j > 0 )
        coeffs |= Bits128(1) << var(i, j-1);
      if( j+1 < width )
        coeffs |= Bits128(1) << var(i, j+1);

      eqs[var(i,j)].first = coeffs;
      eqs[var(i,j)].second = matrix[i*(width+1)+j] == 'O';
    }
  }

  return eqs;
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;
  while( getline(file, line) ) {
    EquationSystem eqs = parse(line);
    if( upper_triangle(eqs) ) {
      echelon_form(eqs);
      size_t count = 0;
      for( auto eq: eqs ) {
        if( eq.second )
          ++count;
      }
      cout << count << endl;
    }
    else
      cout << "-1" << endl;
  }
  file.close();
}
