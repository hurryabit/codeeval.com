//
//  DiscountOffers.cpp
//  PrepareGoogle
//
//  Created by Martin Huschenbett on 09.02.16.
//  Copyright © 2016 Martin Huschenbett. All rights reserved.
//

#include <cctype>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <queue>
#include <vector>

using namespace std;

double max_auction_value(const vector<vector<double>>& max_biddings, double unit) {
  long num_customers = max_biddings.size();
  long num_products = max_biddings.front().size();
  vector<double> prices(num_products, 0.0);
  vector<long> owners(num_products, -1);
  queue<long> waiting;
  for( long customer = 0; customer < num_customers; ++customer )
    waiting.push(customer);
  double delta = unit / (num_customers+1);

  while( !waiting.empty() ) {
    long customer = waiting.front();
    waiting.pop();
    double best_value = 0;
    long best_product = -1;
    for( long product = 0; product < num_products; ++product ) {
      double value = max_biddings[customer][product] - prices[product];
      if( value > best_value ) {
        best_value = value;
        best_product = product;
      }
    }
    if( best_value > 0 ) {
      if( owners[best_product] >= 0 )
        waiting.push(owners[best_product]);
      owners[best_product] = customer;
      prices[best_product] += delta;
    }
  }

  double auction_value = 0;
  for( long product = 0; product < num_products; ++product ) {
    if( owners[product] >= 0 )
      auction_value += max_biddings[owners[product]][product];
  }

  return auction_value;
}

struct string_stat_t {
  int num_vowels = 0;
  int num_consonants = 0;
  int num_letters = 0;

  void parse(const string& input) {
    for( auto letter: input ) {
      letter = tolower(letter);
      if( 'a' <= letter && letter <= 'z' ) {
        ++num_letters;
        switch( letter ) {
          case 'a':
          case 'e':
          case 'i':
          case 'o':
          case 'u':
          case 'y':
            ++num_vowels;
            break;
          default:
            ++num_consonants;
        }
      }
    }
  }
};

int gcd(int a, int b) {
  while( b > 0 ) {
    int c = a % b;
    a = b;
    b = c;
  }
  return a;
}

double score(const string_stat_t& customer, const string_stat_t& product) {
  double result;
  if( product.num_letters % 2 == 0 )
    result = 1.5 * customer.num_vowels;
  else
    result = customer.num_consonants;
  if( gcd(customer.num_letters, product.num_letters) > 1 )
    result *= 1.5;
  return result;
}

vector<string> tokenize(const string& input, char sep) {
  vector<string> result;
  size_t pos = 0;
  while( pos != string::npos ) {
    size_t sep_pos = input.find(sep, pos);
    string token;
    if( sep_pos == string::npos ) {
      token = input.substr(pos);
      pos = string::npos;
    }
    else {
      token = input.substr(pos, sep_pos-pos);
      pos = sep_pos+1;
    }
    result.push_back(token);
  }
  return result;
}

vector<string_stat_t> parse(const string& input) {
  vector<string> tokens = tokenize(input, ',');
  vector<string_stat_t> stats(tokens.size());
  for( size_t i = 0; i < tokens.size(); ++i )
    stats[i].parse(tokens[i]);
  return stats;
}


int main(int argc, char* argv[]) {
  ifstream file(argv[1]);
  while( !file.eof() ) {
    string line;
    getline(file, line);
    size_t semicolon_pos = line.find(';');
    vector<string_stat_t> customer_stats = parse(line.substr(0, semicolon_pos));
    vector<string_stat_t> product_stats = parse(line.substr(semicolon_pos+1));
    size_t num_customers = customer_stats.size();
    size_t num_products = product_stats.size();

    vector<vector<double>> max_biddings(num_customers, vector<double>(num_products, 0.0));
    for( size_t customer = 0; customer < num_customers; ++customer ) {
      for( size_t product = 0; product < num_products; ++product )
        max_biddings[customer][product] = score(customer_stats[customer], product_stats[product]);
    }
    double auction_value = max_auction_value(max_biddings, 0.25);
    cout << setprecision(2) << fixed << auction_value << endl;
  }
  file.close();
}
