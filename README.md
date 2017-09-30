When "a" is found, it becomes #\a
But when matching options, "a" and #\a are distinct (see optarg tester)

Should we accept `-o foo` and `--o=foo` as equivalent?
Meh. Probably.

Should we scan the whole line for options and arguments (GNU)
or just the options before the first argument (POSIX)?

Partial matching?

