
all: minipython.exe
	./minipython.exe --debug test.py
	gcc -no-pie -g test.s && ./a.out

parse: minipython.exe
	./minipython.exe --parse-only test.py

type: minipython.exe
	./minipython.exe --type-only test.py

minipython.exe:
	dune build minipython.exe

test_typing:
	@echo "Running tests..."
	cd tests-mini-python && ./test -2 ../minipython.exe
	cd ..

clean:
	dune clean

.PHONY: all clean minipython.exe



