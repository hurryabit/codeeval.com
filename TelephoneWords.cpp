//
//  TelephoneWords.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 13.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

class Generator {
private:
  static const vector<string> table;
  const string input;
  ostream& out;
  string buffer;
  bool first;


public:
  Generator(ostream& out_, const string& input_) :
    input(input_), out(out_), buffer(input.size(), ' ') {
  }

  void run() {
    first = true;
    run(input.cbegin(), buffer.begin());
    out << endl;
  }

private:
  void run(string::const_iterator inp_pos, string::iterator buf_pos) {
    if( inp_pos == input.cend() ) {
      if( first )
        first = false;
      else
        out << ",";
      cout << buffer;
    }
    else {
      for( char c: table[*inp_pos - '0'] ) {
        *buf_pos = c;
        run(inp_pos+1, buf_pos+1);
      }
    }
  }
};

const vector<string> Generator::table = {"0", "1", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"};

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      Generator gen(cout, line);
      gen.run();
    }
  }
  file.close();
}
