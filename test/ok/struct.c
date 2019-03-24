struct foo {
    int x;
    int y;
};
int main(void)
{
    struct foo f;
    f.x = 1;
    f.y = 2;
    return f.x;
}
