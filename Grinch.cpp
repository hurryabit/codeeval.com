//
//  Grinch.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 17.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <limits>
#include <list>
#include <set>
#include <sstream>
#include <unordered_map>
#include <vector>

using namespace std;

using Distance = unsigned long;

static const Distance INF_DISTANCE = numeric_limits<Distance>::max();


template<typename NameType> class DistanceGraph {
public:
  using Name = NameType;

private:
  using Id = size_t;

  struct Node {
    const Name name;
    list<pair<Id, Distance>> neighbors;

    Node(const Name& u) : name(u), neighbors() {}
  };

  vector<Node> nodes;
  unordered_map<Name, Id> names;

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

class GrinchGraph : DistanceGraph<long> {
private:
  Name source, target;

public:
  GrinchGraph(const string& line) {
    istringstream stream(line);

    do {
      Name u, v;
      Distance d;
      stream >> u >> v >> d;
      addEdge(u, v, d);
      addEdge(v, u, d);
    } while( stream.get() == ',' );

    stream.ignore(2, '|');
    stream >> source >> target;
  }

  Distance solve() {
    return bellmanFord(source, target);
  }
};

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;
  while( getline(file, line) ) {
    GrinchGraph graph(line);
    Distance d = graph.solve();
    if( d == INF_DISTANCE )
      cout << "False" << endl;
    else
      cout << d << endl;
  }
  file.close();
}
