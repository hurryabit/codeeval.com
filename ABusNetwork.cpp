//
//  ABusNetwork.cpp
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
#include <sstream>
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

class BusGraph : DistanceGraph<pair<int,int>> {
private:
  Name source, target;

  void addRoute(int route, const vector<int>& stations, vector<list<int>>& served_routes) {
    for( int station: stations ) {
      if( station < served_routes.size() ) {
        for( int other_route: served_routes[station] ) {
          addEdge(make_pair(station, route), make_pair(station,other_route), 12);
          addEdge(make_pair(station, other_route), make_pair(station,route), 12);
        }
      }
      else
        served_routes.resize(station+1);
      served_routes[station].push_back(route);

      if( station == source.first )
        addEdge(source, make_pair(station, route), 0);
      if( station == target.first )
        addEdge(make_pair(station, route), target, 0);
    }

    for( auto it = stations.cbegin(); it+1 != stations.cend(); ++it ) {
      addEdge(make_pair(*it, route), make_pair(*(it+1), route), 7);
      addEdge(make_pair(*(it+1), route), make_pair(*it, route), 7);
    }
  }

public:
  BusGraph(const string& line) {
    vector<list<int>> served_routes;
    served_routes.reserve(301);

    istringstream stream(line);
    stream.ignore(1) >> source.first;
    source.second = -1;
    stream.ignore(1) >> target.first;
    target.second = -1;

    while( stream.good() ) {
      int route;
      stream.ignore(10, 'R') >> route;
      stream.ignore(10, '[');

      vector<int> stations;
      stations.reserve(35);
      do {
        int station;
        stream >> station;
        stations.push_back(station);
      } while( stream.get() == ',' );

      addRoute(route, stations, served_routes);
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
    BusGraph graph(line);
    Distance d = graph.solve();
    if( d != INF_DISTANCE )
      cout << d << endl;
    else
      cout << "None" << endl;
  }
  file.close();
}
