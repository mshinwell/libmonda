############################################################################
##                                                                        ##
##                Make OCaml native debugging awesome!                    ##
##                                                                        ##
##                  Mark Shinwell, Jane Street Europe                     ##
##                                                                        ##
## Copyright (c) 2013--2016 Jane Street Group, LLC                        ##
##                                                                        ##
## Permission is hereby granted, free of charge, to any person obtaining  ##
## a copy of this software and associated documentation files             ##
## (the "Software"), to deal in the Software without restriction,         ##
## including without limitation the rights to use, copy, modify, merge,   ##
## publish, distribute, sublicense, and/or sell copies of the Software,   ##
## and to permit persons to whom the Software is furnished to do so,      ##
## subject to the following conditions:                                   ##
##                                                                        ##
## The above copyright notice and this permission notice shall be         ##
## included in all copies or substantial portions of the Software.        ##
##                                                                        ##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        ##
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF     ##
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ##
## IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY   ##
## CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,   ##
## TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE      ##
## SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                 ##
##                                                                        ##
############################################################################

# Building libmonda requires a PIC-enabled OCaml toolchain (configure
# with -fPIC).

#GDB_ROOT=$(PREFIX)/libexec/gdb-ocaml.7.11/src
#OCAML_ROOT=$(PREFIX)
#GDB_ROOT=/usr/local/home/mshinwell/mshinwell-gdb
#OCAML_ROOT=/usr/local/home/mshinwell/ocaml-gdb-pic-install
GDB_ROOT=/Users/mark/dev/mshinwell-gdb
OCAML_ROOT=/Users/mark/dev/mshinwell-ocaml3-install

OCAMLOPT=$(OCAML_ROOT)/bin/ocamlopt.byte -verbose -I +compiler-libs -I ./src \
  -g -fPIC -warn-error +a -I ./gdb_backend

# CR mshinwell: Install gdb headers, which will hopefully remove the
# requirement for reading from the gdb build tree.

# Linux
DSYMUTIL=echo
#CC=g++ -x c++ -std=gnu++11 -g -fPIC -O0 \
#	-DLOCALEDIR="/Users/mark/dev/mshinwell-gdb-install/share/locale" \
#	-DHAVE_CONFIG_H \
#  -I$(OCAML_ROOT)/lib/ocaml \
#	-I$(GDB_ROOT)/gdb \
#  -I$(GDB_ROOT)/gdb/cli \
#	-I$(GDB_ROOT)/intl \
#	-I$(GDB_ROOT)/gdb/common \
#	-I$(GDB_ROOT)/gdb/config \
#  -I$(GDB_ROOT)/include/opcode \
#	-I$(GDB_ROOT)/zlib \
#  -I$(GDB_ROOT)/bfd \
#  -I$(GDB_ROOT)/include \
#  -I$(GDB_ROOT)/libdecnumber \
#	-I$(GDB_ROOT)/gdb/gnulib/import \
#  -I$(GDB_ROOT)/build-gnulib/import \
#  -DTUI=1 \
#	-I/usr/include/python2.7 -I/usr/include/python2.7 -Wall -Wpointer-arith \
#	-Wno-unused -Wunused-value -Wunused-function -Wno-switch \
#  -Wno-char-subscripts \
#	-Wempty-body -Wunused-but-set-parameter -Wunused-but-set-variable \
#	-Wno-sign-compare -Wno-narrowing -Wno-error=maybe-uninitialized

# macOS
DSYMUTIL=dsymutil
CC=g++ -x c++ -std=gnu++11 -g -fPIC -O0 \
  -I$(OCAML_ROOT)/lib/ocaml \
  -I$(GDB_ROOT)/gdb \
  -I$(GDB_ROOT)/gdb/common \
  -I$(GDB_ROOT)/gdb/cli \
  -I$(GDB_ROOT)/gdb/config \
  -DLOCALEDIR="/Users/mark/dev/mshinwell-gdb-install/share/locale" \
  -DHAVE_CONFIG_H \
  -I$(GDB_ROOT)/include/opcode \
  -I$(GDB_ROOT)/opcodes/.. \
  -I$(GDB_ROOT)/readline/.. \
  -I$(GDB_ROOT)/zlib \
  -I../bfd \
  -I$(GDB_ROOT)/bfd \
  -I$(GDB_ROOT)/include -I../libdecnumber \
  -I$(GDB_ROOT)/libdecnumber \
  -I$(GDB_ROOT)/intl \
  -I$(GDB_ROOT)/gdb/gnulib/import \
  -I$(GDB_ROOT)/build-gnulib/import \
  -DTUI=1 \
  -I/usr/local/Cellar/python@2/2.7.15_1/Frameworks/Python.framework/Versions/2.7/include/python2.7 \
  -I/usr/local/Cellar/python@2/2.7.15_1/Frameworks/Python.framework/Versions/2.7/include/python2.7 \
  -Wall -Wpointer-arith -Wno-unused -Wunused-value -Wunused-function -Wno-switch \
  -Wno-char-subscripts -Wempty-body -Wno-sign-compare -Wno-narrowing -Wno-mismatched-tags \
  -Wno-error=deprecated-register -Wformat-nonliteral  -c

GDB_BACKEND=gdb_backend/gdb_debugger.mli \
  gdb_backend/gdb_debugger.ml \
  gdb_backend/from_gdb_ocaml.ml

SRC=src/monda_debug.ml \
    src/cmt_file.mli \
    src/cmt_file.ml \
    src/debugger.mli \
    src/load_path_intf.ml \
    src/load_path.mli \
    src/load_path.ml \
    src/cmt_cache_intf.ml \
    src/cmt_cache.mli \
    src/cmt_cache.ml \
    src/naming_conventions.mli \
    src/naming_conventions.ml \
    src/abstraction_breaker.mli \
    src/abstraction_breaker.ml \
    src/unified_value_traversal.mli \
    src/unified_value_traversal.ml \
    src/type_oracle.mli \
    src/type_oracle.ml \
    src/type_helper_intf.ml \
    src/type_helper.mli \
    src/type_helper.ml \
    src/type_printer_intf.ml \
    src/type_printer.mli \
    src/type_printer.ml \
    src/value_copier.mli \
    src/value_copier.ml \
    src/value_printer.mli \
    src/value_printer.ml \
    src/follow_path.mli \
    src/follow_path.ml

# libmonda is versioned according to the compiler version and digest of its
# static configuration parameters.

_DEFAULT: all

src/our_name: src/our_name.ml $(OCAML_ROOT)/bin/ocamlopt
	$(OCAMLOPT) -o src/our_name ocamlcommon.cmxa src/our_name.ml

src/our_name.out: src/our_name
	src/our_name > src/our_name.out

all: $(GDB_BACKEND) $(SRC) src/our_name.out
	$(CC) -c -o gdb_backend/to_gdb.o gdb_backend/to_gdb.c
	$(CC) -c -o gdb_backend/from_gdb.o gdb_backend/from_gdb.c
	$(OCAMLOPT) -output-obj -g -o $(shell cat src/our_name.out)_gdb.so \
	  ocamlcommon.cmxa ocamloptcomp.cmxa dynlink.cmxa bigarray.cmxa unix.cmxa \
	  $(SRC) $(GDB_BACKEND) gdb_backend/to_gdb.o \
    gdb_backend/from_gdb.o
	$(DSYMUTIL) $(shell cat src/our_name.out)_gdb.so

.PHONY: clean
clean: 
	@rm -f gdb_backend/*.cm? gdb_backend/*.cm?? gdb_backend/*.o
	@rm -f src/*.cm? src/*.cm?? src/*.o src/our_name.out src/our_name
	@rm -f $(LIBMONDA)
