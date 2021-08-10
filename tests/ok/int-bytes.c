// expected: 1
int main() {
  // FIXME: depends on sizes / byte order
  int x = 0x12345678;
  short *p = &x;
  return (p[0] == 0x5678) && (p[1] == 0x1234);
}
