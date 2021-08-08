// expected: 2
#define plus(a, b) ((a) + (b))
#define inc(x) (plus((x), 1))
#define z x
int main() {
  int x = 1;
  return inc(z);
}
