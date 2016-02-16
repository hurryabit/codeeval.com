//
//  LevenshteinDistance.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 08.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <queue>
#include <unordered_set>

using namespace std;

struct trie {
  struct node_t {
    bool value;
    map<char, node_t*> children;
    node_t() : value(false), children() {}
  };

  node_t* root = new node_t();

  void insert(const string& word) {
    insert(root, word.begin(), word.end());
  }

  void insert(node_t* node, string::const_iterator begin, string::const_iterator end) {
    if( begin == end )
      node->value = true;
    else {
      auto child_entry = node->children.find(*begin);
      if( child_entry != node->children.end() )
        insert(child_entry->second, begin+1, end);
      else {
        node_t* new_node = new node_t();
        node->children.insert(make_pair(*begin, new_node));
        insert(new_node, begin+1, end);
      }
    }
  }

  bool find(const string& word) {
    return find(root, 0, word);
  }

  bool find(node_t* node, size_t pos, const string& word) {
    if( pos >= word.size() )
      return node->value;
    else {
      auto child_entry = node->children.find(word[pos]);
      if( child_entry == node->children.end() )
        return false;
      else
        return find(child_entry->second, pos+1, word);
    }
  }

  void friends(const string& word, queue<string>& pending) {
    friends(root, 0, word, pending);
  }

  void friends(node_t* node, size_t pos, const string& word, queue<string>& pending) {
    if( pos < word.size() ) {
      // don't edit
      auto child_entry = node->children.find(word[pos]);
      if( child_entry != node->children.end() )
        friends(child_entry->second, pos+1, word, pending);

      // delete (if we're at a repeated character, we already missed the chance to delete it)
      if( pos == 0 || word[pos] != word[pos-1] ) {
        if( find(node, pos+1, word) )
          pending.push(word.substr(0, pos) + word.substr(pos+1));
      }

      // replace
      for( auto child_entry: node->children ) {
        if( child_entry.first != word[pos] && find(child_entry.second, pos+1, word) )
          pending.push(word.substr(0, pos) + child_entry.first + word.substr(pos+1));
      }
    }

    // insert (don't insert the last character, we could have done that earlier)
    for( auto child_entry: node->children ) {
      if( (pos == 0 || child_entry.first != word[pos-1]) && find(child_entry.second, pos, word) )
        pending.push(word.substr(0, pos) + child_entry.first + word.substr(pos));
    }
  }

  int size_of_network(const string& word) {
    unordered_set<string> visited;

    queue<string> pending;
    pending.push(word);

    while( !pending.empty() ) {
      string acquaintance = pending.front();
      pending.pop();
      if( visited.find(acquaintance) == visited.end() ) {
        visited.insert(acquaintance);
        friends(acquaintance, pending);
      }
    }

    return visited.size();
  }
};



int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  string line;

  list<string> queries;
  while( getline(file, line), line != "END OF INPUT" )
    queries.push_back(line);

  trie dict;
  while( !file.eof() ) {
    getline(file, line);
    if( !line.empty() )
      dict.insert(line);
  }

  file.close();

  for( auto query: queries )
    cout << dict.size_of_network(query) << endl;
}
