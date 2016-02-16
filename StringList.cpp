//
//  StringList.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 15.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <set>
#include <vector>

using namespace std;

using Alphabet = set<char>;
using Letter = Alphabet::const_iterator;
using Possibility = vector<Letter>;

bool increment(Possibility& letters, const Letter& begin, const Letter& end) {
  for( auto it = letters.rbegin(); it != letters.rend(); ++it ) {
    ++*it;
    if( *it == end )
      *it = begin;
    else
      return true;
  }
  return false;
}

ostream& operator<<(ostream& out, const Possibility& letters) {
  for( auto letter: letters )
    out << *letter;
  return out;
}

void printPossibilities(size_t length, const string& text) {
  Alphabet alphabet(text.cbegin(), text.cend());
  Possibility letters(length, alphabet.cbegin());

  cout << letters;
  while( increment(letters, alphabet.cbegin(), alphabet.cend()) ) {
    cout << "," << letters;
  }
  cout << endl;
}


int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      size_t comma = line.find(',');
      printPossibilities(stoul(line.substr(0, comma)), line.substr(comma+1));
    }
  }
  file.close();
}
