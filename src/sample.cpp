// #include <cpp11.hpp>
// #include "fastplyr.h"
// #include <cheapr_api.h>
// #include <algorithm>
// // #include <random>
// #include <vector>
// #include <R_ext/Random.h>

// SEXP cpp_sample_int(int n, int size){
//   std::vector<int> population(n);
//   for (int i = 0; i < n; ++i) population[i] = i + 1;
//   std::vector<int> sample;
//   auto rng = std::mt19937{std::random_device{}()};
//   std::sample(population.begin(), population.end(),
//               std::back_inserter(sample),
//               size,
//               rng);
//   return cpp11::as_sexp(sample);
// }

// #include <cpp11.hpp>
// #include <algorithm>      // for std::partial_sort
// #include <vector>
// #include <random>         // for random number generation
// #include <Rmath.h>        // for R's rexp
// #include <R_ext/Random.h>
//
// using namespace cpp11;

// Custom comparator struct
// struct Comp {
//   doubles& v;
//   Comp(doubles& v_) : v(v_) {}
//   bool operator()(int a, int b) { return v[a] < v[b]; }
// };
// integers cpp_weighted_sample_without_replacement(int n, int size, doubles prob) {
//   if (size > n){
//     stop("Sample size cant be greater than population size");
//   }
//
//   writable::doubles rnd(n);
//   GetRNGstate();
//   for (int i = 0; i < n; ++i) {
//     rnd[i] = Rf_rexp(1.0) / prob[i];
//   }
//
//   // Create index vector vx
//   std::vector<int> vx(n);
//   for (int i = 0; i < n; ++i) vx[i] = i;
//
//   // Partial sort using custom comparator
//   std::partial_sort(vx.begin(), vx.begin() + size, vx.end(), Comp(rnd));
//
//   // Prepare output: vx[0:(num-1)] + 1 (for R 1-based indexing)
//   writable::integers out(size);
//   for (int i = 0; i < size; ++i) {
//     out[i] = vx[i] + 1;
//   }
//
//   PutRNGstate();
//   return out;
// }
