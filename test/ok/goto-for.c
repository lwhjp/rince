int main()
{
	before:
	goto inside;
	for (;;) {
		inside:
		if (1)
			goto outside;
		else
			break;
	}
	goto before;
	outside:
	return 0;
}
