#include "random"

extern "C" {
static std::mt19937 mt;

void mt19937_seed(int seed){
  mt.seed(seed);
}

int mt19937(int n)
{
return ((mt()%n)+n)%n;
}
}
