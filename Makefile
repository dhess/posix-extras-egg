build:
	chicken-install -n

install:
	chicken-install

test:
	chicken-install -test

clean:
	rm -f *.import.scm *.so *.o
