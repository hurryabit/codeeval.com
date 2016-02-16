//
//  CodeLikeHuffman.cpp
//  CodeEval
//
//  Created by Martin Huschenbett on 10.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <vector>

using namespace std;

struct node_t {
  bool inner;
  node_t* left = nullptr;
  node_t* right = nullptr;
  char label = 0;
  unsigned priority;

  node_t(node_t* left_, node_t* right_) : inner(true), left(left_), right(right_), priority(left_->priority+right_->priority) {
  }

  node_t(char label_, unsigned priority_) : inner(false), label(label_), priority(priority_) {
  }

  ~node_t() {
    if( inner ) {
      delete left;
      delete right;
    }
  }
};

int compare(const node_t* x, const node_t* y) {
  if( x->priority != y->priority)
    return x->priority < y->priority ? -1 : 1;

  if( x->inner != y->inner )
    return x->inner ? -1 : 1;
  else if( x->inner ) { // x->inner && y->inner
    int cmp = compare(x->left, y->left);
    if( cmp == 0 )
      return compare(x->right, y->right);
    else
      return cmp;
  }
  else { // !x->inner && !y->inner
    if( x->label < y->label )
      return -1;
    else if( x->label == y->label )
      return 0;
    else // x->label > y->label
      return 1;
  }
}

namespace std {

template<> class less<node_t*> {
public:
  bool operator()(const node_t* x, const node_t* y) const {
    return compare(x, y) > 0;
  }
};

};

node_t* build_code_tree(const string& message) {
  vector<int> histogram(26, 0);
  for( char c: message )
    ++histogram[c-'a'];
  
  priority_queue<node_t*> pending;
  for( char i = 0; i < 26; ++i ) {
    if( histogram[i] > 0 )
      pending.push(new node_t('a'+i, histogram[i]));
  }

  while( pending.size() >= 2 ) {
    node_t* left = pending.top();
    pending.pop();
    node_t* right = pending.top();
    pending.pop();
    pending.push(new node_t(left, right));
  }

  return pending.top();
}

using code_table_t = map<char,string>;

void extract_from_node(node_t* node, string path, code_table_t& code_table) {
  if( node->inner ) {
    extract_from_node(node->left, path + "0", code_table);
    extract_from_node(node->right, path + "1", code_table);
  }
  else // !node->inner
    code_table.insert(make_pair(node->label, path));
}

code_table_t extract_codes(node_t* code_tree) {
  code_table_t code_table;
  extract_from_node(code_tree, "", code_table);
  return code_table;
}

void format_code_tree(node_t* code_tree, ostream& out) {
  code_table_t code_table = extract_codes(code_tree);
  bool first = true;
  for( auto entry: code_table ) {
    if( first )
      first = false;
    else
      out << " ";
    out << entry.first << ": " << entry.second << ";";
  }
  out << endl;
}

int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      node_t* code_tree = build_code_tree(line);
      format_code_tree(code_tree, cout);
      delete code_tree;
    }
  }
  file.close();
}
