//
//  DecodeNumbers.cpp
//  CodeEval
//
//  Created by Martin Huschenbett on 08.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

long num_decodings(const string& message) {
  long size = message.size();
  vector<long> table(size + 1);
  table[size] = 1;
  table[size-1] = 1;
  for( long i = size-2; i >= 0; --i ) {
    char a = message[i], b = message[i+1];
    table[i] = 0;
    if( a > '0' ) {
      table[i] = table[i+1];

      if( a == '1' || (a == '2' && b <= '6') )
        table[i] += table[i+2];
    }
  }
  return table[0];
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string message;
    getline(file, message);
    if( !message.empty() )
      cout << num_decodings(message) << endl;
  }
  file.close();
}
