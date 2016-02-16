//
//  APileOfBricks.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 08.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <list>
#include <sstream>
#include <vector>

using namespace std;

template<class Container> ostream& printContainer(ostream& stream, Container container) {
  bool first = true;
  for( auto elem: container ) {
    if( first )
      first = false;
    else
      stream << ',';
    stream << elem;
  }
  return stream;
}

struct hole_t {
  int x1, y1, x2, y2;
};

struct brick_t {
  int id;
  int x1, y1, z1, x2, y2, z2;

  bool fits_through(const hole_t& hole) {
    array<int, 3> brick_dims = {abs(x1-x2), abs(y1-y2), abs(z1-z2)};
    array<int, 2> hole_dims = {abs(hole.x1-hole.x2), abs(hole.y1-hole.y2)};
    sort(brick_dims.begin(), brick_dims.end());
    sort(hole_dims.begin(), hole_dims.end());
    return brick_dims[0] <= hole_dims[0] && brick_dims[1] <= hole_dims[1];
  }
};

struct pile_t {
  hole_t hole;
  vector<brick_t> bricks;

  pile_t(const string& input) {
    istringstream stream(input);
    vector<int> numbers;
    char c;
    while( !stream.eof() ) {
      stream.get(c);
      if( ('0' <= c && c <= '9') || c == '-' ) {
        stream.unget();
        int number;
        stream >> number;
        numbers.push_back(number);
      }
    }

    int* ptr = (int*) &hole;
    for( long i = 0; i < 4; ++i )
      *(ptr+i) = numbers[i];

    for( long i = 4; i < numbers.size(); i += 7 ) {
      brick_t brick;
      ptr = (int*) &brick;
      for( long j = 0; j < 7; ++j )
        *(ptr+j) = numbers[i+j];
      bricks.push_back(brick);
    }
  }

  list<int> fitting_ids() {
    list<int> result;
    for( auto brick: bricks ) {
      if( brick.fits_through(hole) )
        result.push_back(brick.id);
    }
    result.sort();
    return result;
  }
};

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      pile_t pile(line);
      list<int> ids = pile.fitting_ids();
      if( ids.empty() )
        cout << "-";
      else
        printContainer(cout, ids);
      cout << endl;
    }
  }
  file.close();
}
