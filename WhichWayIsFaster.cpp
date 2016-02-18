//
//  WhichWayIsFaster.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 17.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <set>
#include <tuple>
#include <vector>

using namespace std;

using Distance = unsigned;

static const Distance INF_DISTANCE = numeric_limits<Distance>::max();


template<class T> class DistanceGraph {
public:
  using Name = T;

private:
  using Id = unsigned;

  struct Node {
    const Name name;
    list<pair<Id, Distance>> neighbors;

    Node(const Name& u) : name(u), neighbors() {}
  };

  vector<Node> nodes;
  map<Name, Id> names;

  Id getOrCreateId(const Name& u) {
    auto u_it = names.find(u);
    if( u_it != names.cend() )
      return u_it->second;
    else {
      Id u_id = (unsigned) nodes.size();
      nodes.push_back(Node(u));
      names.insert(make_pair(u, u_id));
      return u_id;
    }
  }

public:
  DistanceGraph() : nodes(), names() {}

  void addEdge(const Name& u, const Name& v, const Distance& d) {
    Id u_id = getOrCreateId(u);
    Id v_id = getOrCreateId(v);
    nodes[u_id].neighbors.push_back(make_pair(v_id, d));
  }

  Distance dijkstra(const Name& s, const Name& t) {
    Id s_id = getOrCreateId(s);
    Id t_id = getOrCreateId(t);

    set<pair<Distance, Id>> queue;
    vector<Distance> min_distances(nodes.size(), INF_DISTANCE);
    queue.insert(make_pair(0, s_id));
    min_distances[s_id] = 0;

    while( !queue.empty() ) {
      auto it = queue.cbegin();
      Distance u_distance = it->first;
      Id u_id = it->second;
      queue.erase(it);

      for( auto neighbor: nodes[u_id].neighbors ) {
        Id v_id = neighbor.first;
        Distance new_distance = u_distance + neighbor.second;
        if( new_distance < min_distances[v_id] ) {
          queue.erase(make_pair(min_distances[v_id], v_id));
          queue.insert(make_pair(new_distance, v_id));
          min_distances[v_id] = new_distance;
        }
      }
    }

    return min_distances[t_id];
  }

  Distance bellmanFord(const Name& s, const Name& t) {
    Id s_id = getOrCreateId(s);
    Id t_id = getOrCreateId(t);

    vector<Distance> min_distances(nodes.size(), INF_DISTANCE);
    min_distances[s_id] = 0;
    bool changed = true;

    while( changed ) {
      changed = false;
      for( Id u_id = 0; u_id < nodes.size(); ++u_id ) {
        if( min_distances[u_id] != INF_DISTANCE ) {
          for( auto neighbor: nodes[u_id].neighbors ) {
            Id v_id = neighbor.first;
            Distance new_distance = min_distances[u_id] + neighbor.second;
            if( new_distance < min_distances[v_id] ) {
              changed = true;
              min_distances[v_id] = new_distance;
            }
          }
        }
      }
    }

    return min_distances[t_id];
  }
};

class MapGraph : DistanceGraph<tuple<int,int,bool>> {
private:
  Name source, target;

public:
  MapGraph(const string& line) {
    int dim = (int) line.find(' ');
    auto terrain = [&](int i, int j) { return line[i*(dim+3)+j]; };

    for( int i1 = 0; i1 < dim; ++i1 ) {
      for( int j1 = 0; j1 < dim; ++j1 ) {
        if( terrain(i1, j1) != '^' ) {
          // add edges to adjacent square on land & water
          int dis[] = {1, 0, -1, 0}, djs[] = {0, 1, 0, -1};
          for( int k = 0; k < 4; ++k ) {
            int i2 = i1 + dis[k], j2 = j1 + djs[k];
            if( 0 <= i2 && i2 < dim && 0 <= j2 && j2 < dim && terrain(i2, j2) != '^' ) {
              for( bool land: {true, false} )
                addEdge(make_tuple(i1, j1, land), make_tuple(i2, j2, land), land ? 2 : 1);
            }
          }
        }

        switch( terrain(i1, j1) ) {
          case 'S':
            source = make_tuple(i1, j1, true);
            break;
          case 'F':
            target = make_tuple(i1, j1, true);
            break;
          case 'P':
            // for ports, add edges between land & water
            for( bool land: {true, false} )
              addEdge(make_tuple(i1, j1, land), make_tuple(i1, j1, !land), 1);
        }
      }
    }
  }

  Distance solve() {
    return dijkstra(source, target);
  }
};

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;
  while( getline(file, line) ) {
    MapGraph graph(line);
    cout << graph.solve() << endl;
  }
  file.close();
}
