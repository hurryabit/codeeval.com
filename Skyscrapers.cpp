//
//  Skyscrapers.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 16.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <sstream>

using namespace std;

struct Skyscraper {
  int left, right, height;
};

istream& operator>>(istream& in, Skyscraper& s) {
  in.ignore(2, '(') >> s.left;
  in.ignore(1, ',') >> s.height;
  in.ignore(1, ',') >> s.right;
  in.ignore(1, ')');
  return in;
}

struct Skyline {
  struct Event {
    list<int> inserts, deletes;

    Event() : inserts(), deletes() {
    }
  };

  map<int, Event> events;

  Skyline() : events() {
  }

  void add(const Skyscraper& s) {
    events[s.left ].inserts.push_back(s.height);
    events[s.right].deletes.push_back(s.height);
  }

  void process(ostream& out) {
    multiset<int> current_heights;
    int last_height = 0;
    bool first = true;
    current_heights.insert(0);
    for( auto pos_event: events ) {
      int pos = pos_event.first;
      Event event = pos_event.second;
      for( int height: event.deletes )
        current_heights.erase(current_heights.find(height));
      for( int height: event.inserts )
        current_heights.insert(height);
      if( last_height != *current_heights.crbegin() ) {
        last_height = *current_heights.crbegin();
        if( first )
          first = false;
        else
          out << " ";
        out << pos << " " << last_height;
      }
    }
    out << endl;
  }
};

istream& operator>>(istream& in, Skyline& sl) {
  Skyscraper s;
  do {
    in >> s;
    sl.add(s);
  } while( in.get() == ';' );
  return in;
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;
  while( getline(file, line) ) {
    istringstream stream(line);
    Skyline sl;
    stream >> sl;
    sl.process(cout);
  }
  file.close();
}
