//
//  StraightLines.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 17.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <algorithm>
#include <fstream>
#include <iostream>
#include <list>
#include <sstream>
#include <vector>

using namespace std;

struct Point {
  int x, y;
};

bool colinear(const Point& a, const Point& b, const Point& c) {
  return (b.x-a.x) * (c.y-a.y) == (c.x-a.x) * (b.y-a.y);
}

vector<Point> parsePoints(const string& line) {
  vector<Point> points;
  istringstream in(line);
  while( !in.eof() ) {
    Point a;
    in >> a.x >> a.y;
    in.ignore(3);
    points.push_back(a);
  }
  return points;
}

int countLines(const vector<Point>& points) {
  int count = 0;
  for( auto a = points.cbegin(); a != points.cend(); ++a ) {
    for( auto b = a+1; b != points.cend(); ++b ) {
      int online = 0;
      for( auto c = b+1; c != points.cend(); ++c ) {
        if( colinear(*a, *b, *c) )
          ++online;
      }
      switch( online ) {
        case 1:
          ++count;
          break;
        case 2:
          --count;
          break;
        default: ;
      }
    }
  }
  return count;
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;
  while( getline(file, line) ) {
    vector<Point> points = parsePoints(line);
    cout << countLines(points) << endl;
  }
  file.close();
}
