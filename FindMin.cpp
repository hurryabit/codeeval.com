//
//  FindMin.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 09.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

using nat_t = long unsigned;

struct nat_set_t {
  struct node_t {
    nat_t value, min, max;
    nat_t size;
    node_t* left;
    node_t* right;

    static nat_t get_size(node_t* node) {
      return node == nullptr ? 0 : node->size;
    }

    void adjust_size() {
      size = 1 + get_size(left) + get_size(right);
    }

    void adjust_min() {
      min = left == nullptr ? value : left->min;
    }

    void adjust_max() {
      max = right == nullptr ? value : right->max;
    }

    bool is_packed() {
      return max-min+1 == size;
    }
  };

  node_t* root = nullptr;

  void insert(nat_t value) {
    insert(root, value);
  }

  void insert(node_t*& node, nat_t value) {
    if( node == nullptr )
      node = new node_t{value, value, value, 1, nullptr, nullptr};
    else if( value < node->value ) {
      if( value < node->min )
        node->min = value;
      node->size++;
      insert(node->left, value);
    }
    else if( value > node->value ) {
      if( value > node->max )
        node->max = value;
      node->size++;
      insert(node->right, value);
    }
  }

  void remove(nat_t value) {
    remove(root, value);
  }

  nat_t remove_minimum(node_t*& node) {
    nat_t result;
    if( node->left == nullptr) {
      result = node->value;
      node_t* right = node->right;
      delete node;
      node = right;
    }
    else {
      result = remove_minimum(node->left);
      node->adjust_min();
      node->size--;
    }
    return result;
  }

  void remove(node_t*& node, nat_t value) {
    if( node == nullptr )
      return;

    if( value < node->value ) {
      remove(node->left, value);
      node->adjust_min();
      node->adjust_size();
    }
    else if( value > node->value ) {
      remove(node->right, value);
      node->adjust_max();
      node->adjust_size();
    }
    else { // value == node->value
      if( node->left == nullptr ) {
        node_t* right = node->right;
        delete node;
        node = right;
      }
      else if( node->right == nullptr ) {
        node_t* left = node->left;
        delete node;
        node = left;
      }
      else { // node->left != nullptr && node->right != nullptr
        node->value = remove_minimum(node->right);
        node->size--;
      }
    }
  }

  nat_t least_excluded() {
    return least_excluded_above(root, 0);
  }

  nat_t least_excluded_above(node_t* node, nat_t bnd) {
    if( node == nullptr || !(node->min <= bnd && bnd <= node->max) )
      return bnd;
    else if( node->is_packed() )
      return node->max+1;
    else {
      nat_t left_bnd = least_excluded_above(node->left, bnd);
      if( left_bnd < node->value )
        return left_bnd;
      else
        return least_excluded_above(node->right, max(left_bnd, node->value+1));
    }
  }
};

struct instance_t {
  nat_t n, k, a, b, c, r;

  instance_t(const string& input) {
    istringstream stream(input);
    nat_t* ptr = (nat_t*) this;
    for( size_t i = 0; i < 6; ++i ) {
      if( i > 0 )
        stream.get();
      stream >> *(ptr+i);
    }
  }

  nat_t solve() {
    nat_set_t set;
    vector<nat_t> ms(k);
    ms[0] = a;
    for( nat_t i = 1; i < k; ++i )
      ms[i] = (b*ms[i-1]+c) % r;
    for( auto m: ms )
      set.insert(m);

    for( nat_t i = k; i < n; ++i ) {
      nat_t j = i % k;
      nat_t m = set.least_excluded();
      set.remove(ms[j]);
      ms[j] = m;
      set.insert(m);
    }

    return ms[(n-1) % k];
  }
};


int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    if( !line.empty() ) {
      instance_t instance(line);
      cout << instance.solve() << endl;
    }
  }
  file.close();
}
