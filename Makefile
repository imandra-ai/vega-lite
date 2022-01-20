
all:
	@dune build @all

test:
	@dune runtest --force

clean:
	@dune clean

WATCH ?= @all
watch:
	@dune build $(WATCH) -w
