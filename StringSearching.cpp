//
//  StringSearching.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 13.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>

using namespace std;

class PatternAutomaton {
private:
  struct Transition {
    char label;
    bool pre_loop;
  };

  vector<Transition> transitions;

public:
  PatternAutomaton(const string& pattern) {
    bool star = true;
    for( auto it = pattern.cbegin(); it != pattern.cend(); ++it ) {
      switch( *it ) {
        case '*':
          star = true;
          break;
        case '\\':
          ++it;
          if( *it != '*' )
            throw invalid_argument(string("pattern contains invalid escape sequence (\\") + *it + ")");
        default:
          transitions.push_back(Transition{*it, star});
      }
    }
  }

  bool match(const string& input) {
    size_t final_state = transitions.size();
    vector<bool> state_set(final_state+1, false);
    state_set[0] = true;

    auto letter = input.cbegin();
    while( letter != input.cend() && !state_set[final_state] ) {
      vector<bool> old_state_set = state_set;
      state_set.assign(final_state+1, false);

      for( size_t state = 0; state < final_state; ++state ) {
        if( old_state_set[state] ) {
          if( transitions[state].label == *letter )
            state_set[state+1] = true;
          if( transitions[state].pre_loop )
            state_set[state] = true;
        }
      }

      ++letter;
    }

    return state_set[final_state];
  }
};

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      size_t comma = line.find(',');
      string text = line.substr(0, comma), pattern = line.substr(comma+1);
      PatternAutomaton automaton(pattern);
      cout << boolalpha << automaton.match(text) << endl;
    }
  }
  file.close();
}
