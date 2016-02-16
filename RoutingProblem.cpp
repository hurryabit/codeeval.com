//
//  RoutingProblem.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 09.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <cctype>
#include <fstream>
#include <iostream>
#include <list>
#include <vector>

using namespace std;

struct addr_t {
  unsigned ip, nm;

  bool in_same_network(const addr_t& other) {
    return (ip & nm) == (other.ip & other.nm);
  }
};

struct host_t {
  unsigned id;
  list<addr_t> interfaces;

  bool in_same_network(const host_t& other) {
    for( auto addr: interfaces ) {
      for( auto other_addr: other.interfaces ) {
        if( addr.in_same_network(other_addr) )
          return true;
      }
    }
    return false;
  }
};

struct network_t {
  vector<host_t> hosts;

  network_t(const list<host_t>& host_list) : hosts(host_list.size()) {
    for( auto host: host_list )
      hosts[host.id] = host;
  }
};

struct parser_t {
  istream& input;

  parser_t(istream& input) : input(input) {}

  template<typename T> T read() {
    T result;
    input >> result;
    return result;
  }

  void skip_spaces() {
    while( isspace(input.peek()) )
      input.get();
  }

  void match(char c) {
    if( input.peek() == c )
      input.get();
    else
      input.setstate(ios_base::failbit);
  }

  void match(const string& str) {
    for( char c: str )
      match(c);
  }

  addr_t address() {
    match('\'');
    unsigned a = read<unsigned>();
    match('.');
    unsigned b = read<unsigned>();
    match('.');
    unsigned c = read<unsigned>();
    match('.');
    unsigned d = read<unsigned>();
    match('/');
    unsigned n = read<unsigned>();
    match('\'');
    addr_t result;
    result.ip = a << 24 | b << 16 | c << 8 | d;
    result.nm = ((1 << n) - 1) << (32-n);
    return result;
  }

  host_t host() {
    host_t result;
    result.id = read<unsigned>();
    match(": [");
    result.interfaces.push_back(address());
    while( input.peek() == ',' ) {
      match(", ");
      result.interfaces.push_back(address());
    }
    match(']');
    return result;
  }

  network_t network() {
    list<host_t> result;
    match('{');
    result.push_back(host());
    while( input.peek() == ',' ) {
      match(", ");
      result.push_back(host());
    }
    match('}');
    return network_t(result);
  }

  list<pair<unsigned,unsigned>> test_cases() {
    list<pair<unsigned,unsigned>> result;
    while( !input.eof() ) {
      match('\n');
      unsigned s = read<unsigned>();
      match(' ');
      unsigned t = read<unsigned>();
      result.push_back(make_pair(s,t));
    }
    return result;
  }
};


int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  parser_t parser(file);
  auto network = parser.network();
  auto test_cases = parser.test_cases();
  for( auto test_case: test_cases )
    cout << test_case.first << " " << test_case.second << endl;

  file.close();
}
