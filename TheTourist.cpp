//
//  TheTourist.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 12.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <cstdint>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <vector>

using namespace std;

using Node = int;

using Distance = int;

class Graph : public vector<map<Node,Distance>> {
public:
  vector<bool> existent;

  Graph(const string& input) {
    istringstream stream(input);
    while( !stream.eof() ) {
      Node u, v;
      Distance d;
      stream >> u >> v >> d;

      if( u >= size())
        resize(u+1);
      operator[](u).insert(make_pair(v, d));

      if( max(u,v) >= existent.size() )
        existent.resize(max(u,v)+1);
      existent[u] = true;
      existent[v] = true;

      stream.ignore(3, '|');
    }
    for( Node u = 1; u < size(); ++u ) {
      if( existent[u] )
        operator[](0).insert(make_pair(u, 0));
    }
  }

  size_t nodeCount() const {
    size_t count = 0;
    for( auto ex: existent ) {
      if( ex )
        ++count;
    }
    return count;
  }

  size_t edgeCount() const {
    size_t count = 0;
    for( auto adj: *this )
      count += adj.size();
    return count;
  }

  bool satisfiesTI() const {
    for( Node u = 1; u < size(); ++u ) {
      for( auto vd: operator[](u) ) {
        Node v = vd.first;
        Distance d_uv = vd.second;
        for( auto wd: operator[](u) ) {
          Node w = wd.first;
          Distance d_uw = wd.second;
          auto it = operator[](w).find(v);
          if( it != operator[](w).cend() ) {
            Distance d_wv = it->second;
            if( d_uv > d_uw + d_wv ) {
              cout << "u = " << u << ", v = " << v << ", w = " << w << endl;
              cout << "d(u,v) = " << d_uv << ", d(u,w) = " << d_uw << ", d(w,v) = " << d_wv << endl;
              return false;
            }
          }
        }
      }
    }
    return true;
  }

  mutable Distance min_dist;

  int tsp() const {
    vector<bool> visited(size(), false);
    visited[0] = true;
    min_dist = INT_MAX;
    tsp(0, 0, nodeCount(), visited);
    return min_dist;
  }


  void tsp(Node u, Distance dist_u, size_t missing, vector<bool>& visited) const {
    if( missing == 0 ) {
      min_dist = dist_u;
      cout << "found new shortest route: " << min_dist << endl;
    }
    else {
      for( auto adj: operator[](u) ) {
        Node v = adj.first;
        Distance dist_v = dist_u + adj.second;

        if( !visited[v] && dist_v < min_dist ) {
          visited[v] = true;
          tsp(v, dist_v, missing-1, visited);
          visited[v] = false;
        }
      }
    }
  }
};

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      Graph graph(line);
      size_t nodes = graph.nodeCount();
      cout << "nodes: " << nodes << ", edges: " << graph.edgeCount() << endl;
      cout << graph.satisfiesTI() << endl;
      map<size_t, size_t> histogram;
      for( Node u = 0; u < graph.size(); ++u ) {
        if( graph.existent[u] )
          ++histogram[graph[u].size()];
      }
      for( auto kv: histogram )
        cout << kv.first << ": " << kv.second << endl;
      cout << graph.tsp() << endl;
    }
  }
  file.close();
}
