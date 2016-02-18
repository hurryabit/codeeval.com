//
//  DistinctTriangles.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 18.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

class Graph {
private:
  using AdjacencyMatrix = vector<vector<bool>>;

  AdjacencyMatrix adjacency_matrix;
  size_t num_triangles = 0;

public:
  bool areAdjacent(size_t u, size_t v) {
    return u != v && adjacency_matrix[max(u, v)][min(u, v)];
  }

  void addEdge(size_t u, size_t v) {
    for( size_t w = 0; w < adjacency_matrix.size(); ++w ) {
      if( areAdjacent(u, w)  && areAdjacent(v, w) )
        ++num_triangles;
    }
    adjacency_matrix[max(u, v)][min(u, v)] = true;
  }

  Graph(const string& line) {
    size_t num_nodes, num_edges;
    istringstream stream(line);
    stream >> num_nodes >> num_edges;

    adjacency_matrix.resize(num_nodes);
    for( size_t i = 0; i < num_nodes; ++i )
      adjacency_matrix[i].resize(i, false);

    for( size_t i = 0; i < num_edges; ++i ) {
      size_t u, v;
      stream.ignore(1) >> u >> v;
      addEdge(u, v);
    }
  }

  size_t getNumTriangles() const {
    return num_triangles;
  }
};

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;
  while( getline(file, line) ) {
    Graph graph(line);
    cout << graph.getNumTriangles() << endl;
  }
  file.close();
}
