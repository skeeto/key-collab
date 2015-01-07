CFLAGS  = -std=c99 -Wall -O3 -g3
LDFLAGS = -pthread

key_collab : key_collab.c platform_posix.c wordlist.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

wordlist.o : wordlist
	$(LD) -r -b binary -o $@ $^

wordlist :
	curl -o $@ 'http://pastebin.com/raw.php?i=9aFn1r27'

.PHONY : test clean distclean

test : key_collab wordlist
	./$^

clean :
	$(RM) key_collab wordlist.o

distclean : clean
	$(RM) wordlist
