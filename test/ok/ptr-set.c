// expected: 1
int main(void)
{
    int x;
    int *p = &x;
    *p = 1;
    return x;
}
