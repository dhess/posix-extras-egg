build:
	chicken-setup -n

install:
	chicken-setup

test:
	chicken-setup -t

clean:
	rm -f *.exports *.so
