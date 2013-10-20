
LIBDIR="/usr/local/lib/"
INCLUDEDIR="/usr/local/include/assimp/"

OUTFILE=main

# This guy automatically links with the C++ runtime.
# There's an option for this on gcc
CC=g++

.PHONY: clean

all: $(OUTFILE)

glue.a: glue.lisp
	ecl -norc \
	    -eval '(require :asdf)' \
	    -eval '(push "./" asdf:*central-registry*)' \
	    -eval '(asdf:make-build :glue :type :static-library :move-here "./$@")' \
	    -eval '(quit)'

$(OUTFILE): main.cc exporting.h exporting.cc glue.a
	$(CC) `ecl-config --cflags` -o $@ $^ `ecl-config --ldflags` -lecl -lassimp

clean:
	rm -f $(OUTFILE) *.o *.a

