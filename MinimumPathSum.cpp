//
//  MinimumPathSum.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 09.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>

using namespace std;

using matrix_t = vector<vector<long>>;

long min_path_sum(matrix_t A) {
  long m = A.size();
  long n = A.front().size();

  for( long i = 1; i < m; ++i )
    A[i][0] += A[i-1][0];
  for( long j = 1; j < n; ++j )
    A[0][j] += A[0][j-1];

  for( long i = 1; i < m; ++i ) {
    for( long j = 1; j < n; ++j )
      A[i][j] += min(A[i-1][j], A[i][j-1]);
  }

  return A[m-1][n-1];
}

vector<string> tokenize(const string& input, char sep) {
  vector<string> result;
  size_t pos = 0;
  while( pos != string::npos ) {
    size_t sep_pos = input.find(sep, pos);
    string token;
    if( sep_pos == string::npos ) {
      token = input.substr(pos);
      pos = string::npos;
    }
    else {
      token = input.substr(pos, sep_pos-pos);
      pos = sep_pos+1;
    }
    result.push_back(token);
  }
  return result;
}

matrix_t parse_matrix(istream& input) {
  string line;
  getline(input, line);
  long n = stol(line);
  matrix_t A(n, vector<long>(n));
  for( long i = 0; i < n; ++ i) {
    getline(input, line);
    vector<string> tokens = tokenize(line, ',');
    for( long j = 0; j < n; ++j )
      A[i][j] = stol(tokens[j]);
  }
  return A;
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    try {
      matrix_t A = parse_matrix(file);
      cout << min_path_sum(A) << endl;
    }
    catch(invalid_argument ex) {}
  }
  file.close();
}
