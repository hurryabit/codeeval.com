//
//  PrisonerOrCitizen.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 15.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

using namespace std;

struct Point {
  int x, y;
};

istream& operator>>(istream& in, Point& p) {
  in >> p.x >> p.y;
  return in;
}

Point operator-(const Point& p, const Point& q) {
  return Point{p.x - q.x, p.y - q.y};
}

int operator*(const Point& p, const Point& q) {
  return p.x * q.y - q.x * p.y;
}

int side(const Point& p1, const Point& p2, const Point& q) {
  return (p2 - p1) * (q - p1);
}

bool inside(int x, int y1, int y2) {
  return min(y1, y2) <= x && x <= max(y1, y2);
}

bool inside(const Point& q, const vector<Point>& ps) {
  if( ps.size() < 3 )
    return false;

  int numWindings = 0;

  for( auto p = ps.cbegin(); p != ps.cend(); ++p ) {
    Point p1, p2;
    if( p+1 != ps.cend() ) {
      p1 = *p;
      p2 = *(p+1);
    }
    else {
      p1 = ps.back();
      p2 = ps.front();
    }

    int s = side(p1, p2, q);
    if( s == 0 && min(p1.x, p2.x) <= inside(q.x, p1.x, p2.x) && inside(q.y, p1.y, p2.y) )
      return true;

    if( p1.y <= q.y && q.y < p2.y && s > 0 )
        ++numWindings;
    if( p2.y <= q.y && q.y < p1.y && s < 0 )
        --numWindings;
  }

  return numWindings != 0;
}

struct Instance {
  Point person;
  vector<Point> jail;
};

istream& operator>>(istream& in, Instance& inst) {
  inst.jail.clear();
  char comma = ',';
  while( comma == ',' ) {
    Point p;
    in >> p >> comma;
    inst.jail.push_back(p);
  }
  in.ignore(1, '|');
  in >> inst.person;
  return in;
}

string solve(const Instance& inst) {
  return inside(inst.person, inst.jail) ? "Prisoner" : "Citizen";
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;
  while( file ) {
    Instance inst;
    file >> inst;
    file.ignore(1, '\n');
    cout << solve(inst) << endl;
  }
  file.close();
}
