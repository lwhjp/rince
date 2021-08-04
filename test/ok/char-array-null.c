// expected: 1
int main() {
  char a[4] = "test";
  char b[5] = "test";
  char c[] = "test";
  return b[4] == c[4];
}
