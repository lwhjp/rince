int main(void)
{
    const char *str = "test";
    const char *p = str;
    while (*p) p++;
    return p - str;
}
