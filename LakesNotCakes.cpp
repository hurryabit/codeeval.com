//
//  LakesNotCakes.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 16.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <algorithm>
#include <fstream>
#include <iostream>
#include <list>
#include <vector>

using namespace std;

struct Map {
  vector<vector<bool>> isWater; // width x height
  long width, height;

  Map(const string& input) {
    long pipe1 = input.find('|');
    width = pipe1 / 2;
    height = (input.size()+3) / (pipe1+2);
    isWater.resize(width, vector<bool>(height, false));
    for( long i = 0; i < width; ++i ) {
      for( long j = 0; j < height; ++j )
        isWater[i][j] = input[2 * ((width+1) * j + i)] == 'o';
    }
  }

  long countLakes() const {
    long count = 0;
    vector<vector<bool>> visited(width, vector<bool>(height, false));

    for( long i0 = 0; i0 < width; ++i0 ) {
      for( long j0 = 0; j0 < height; ++j0 ) {
        if( isWater[i0][j0] && !visited[i0][j0] ) {
          ++count;

          list<pair<long,long>> queue;
          queue.push_back(make_pair(i0, j0));
          visited[i0][j0] = true;

          while( !queue.empty() ) {
            auto node = queue.front();
            long i1 = node.first, j1 = node.second;
            queue.pop_front();
//            cout << "dicovered (" << i1 << ", " << j1 << ")" << endl;

            for( long i2 = max(i1-1, (long) 0); i2 <= min(i1+1, width-1); ++i2 ) {
              for( long j2 = max(j1-1, (long) 0); j2 <= min(j1+1, height-1); ++j2 ) {
                if( isWater[i2][j2] && !visited[i2][j2] ) {
                  queue.push_back(make_pair(i2,j2));
                  visited[i2][j2] = true;
                }
              }
            }
          }
        }
      }
    }

    return count;
  }
};

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;
  while( getline(file, line) ) {
    Map map(line);
    cout << map.countLakes() << endl;
  }
  file.close();
}
