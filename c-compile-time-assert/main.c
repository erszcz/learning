// Found in GRUB source code.

#define COMPILE_TIME_ASSERT(cond) switch (0) { case 1: case !(cond): ; }

int main(int argc, const char *argv[])
{
    COMPILE_TIME_ASSERT (3 != 3);
    return 0;
}
