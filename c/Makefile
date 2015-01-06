CFLAGS  = -std=c99 -Wall -O3 -g3
LDFLAGS = -pthread
LDLIBS  = -lncurses

key-collab : key-collab.c

wordlist :
	curl -o $@ 'http://pastebin.com/raw.php?i=9aFn1r27'

.PHONY : test clean distclean

test : key-collab wordlist
	./$^ < wordlist

clean :
	$(RM) key-collab

distclean : clean
	$(RM) wordlist
