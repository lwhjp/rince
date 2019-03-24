int f(int x, ...)
{
    return x;
}
int main(void)
{
    f(1, 2);
    return f(3);
}
