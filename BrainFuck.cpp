//
//  BrainFuck.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 10.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stack>
#include <vector>

using namespace std;

class interpreter_t {
private:
  string program;
  size_t counter = 0;
  vector<size_t> jumps;
  stack<uint8_t> prev_cells;
  uint8_t curr_cell = 0;
  stack<uint8_t> next_cells;
  ostringstream outstream;

public:
  interpreter_t(const string& program_) : program(program_) {
  }

  void calculate_jumps() {
    jumps.clear();
    jumps.resize(program.size(), 0);
    jumps.shrink_to_fit();
    stack<size_t> last_brackets;
    for( size_t i = 0; i < program.length(); ++i ) {
      switch( program[i] ) {
        case '[':
          last_brackets.push(i);
          break;
        case ']':
          size_t j = last_brackets.top();
          last_brackets.pop();
          jumps[i] = j;
          jumps[j] = i;
          break;
      }
    }
  }

  void move_next(stack<uint8_t>& prev_cells, stack<uint8_t>& next_cells) {
    prev_cells.push(curr_cell);
    if( next_cells.empty() )
      next_cells.push(0);
    curr_cell = next_cells.top();
    next_cells.pop();
  }

  void move_next() {
    move_next(prev_cells, next_cells);
  }

  void move_prev() {
    move_next(next_cells, prev_cells);
  }

  void increment() {
    ++curr_cell;
  }

  void decrement() {
    --curr_cell;
  }

  void output() {
    outstream.put(curr_cell);
  }

  void jump() {
    counter = jumps[counter];
  }

  void execute() {
    size_t steps = 0;
    while( counter < program.length() ) {
      ++steps;
      switch( program[counter] ) {
        case '>':
          move_next();
          break;
        case '<':
          move_prev();
          break;
        case '+':
          increment();
          break;
        case '-':
          decrement();
          break;
        case '.':
          output();
          break;
        case '[':
          if( curr_cell == 0 )
            jump();
          break;
        case ']':
          if( curr_cell != 0 )
            jump();
          break;
      }
      ++counter;
    }
  }

  string get_output() {
    return outstream.str();
  }
};


int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      interpreter_t interpreter(line);
      interpreter.calculate_jumps();
      interpreter.execute();
      cout << interpreter.get_output() << endl;
    }
  }
  file.close();
}
