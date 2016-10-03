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

GDB_ROOT=$(PREFIX)/libexec/gdb-ocaml.7.11/src
OCAML_ROOT=$(PREFIX)

OCAMLOPT=$(OCAML_ROOT)/bin/ocamlopt -verbose -I +compiler-libs -I ./src \
  -g -fPIC -I ./gdb_backend

CC=gcc -O0 -fPIC -Werror -g \
  -I$(OCAML_ROOT)/lib/ocaml \
  -I$(GDB_ROOT)/gdb \
  -I$(GDB_ROOT)/gdb/common \
  -I$(GDB_ROOT)/gdb/config \
  -DHAVE_CONFIG_H \
  -I$(GDB_ROOT)/include \
  -I$(GDB_ROOT)/gdb/gnulib/import \
  -I$(GDB_ROOT)/gdb/build-gnulib/import

#  -I$(GDB_ROOT)/bfd \
#  -I$(GDB_ROOT)/intl \

GDB_BACKEND=gdb_backend/gdb_debugger.mli \
  gdb_backend/gdb_debugger.ml \
  gdb_backend/from_gdb_ocaml.ml

SRC=src/monda_debug.ml \
    src/cmt_file.mli \
    src/cmt_file.ml \
    src/cmt_cache.mli \
    src/cmt_cache.ml \
    src/naming_conventions.mli \
    src/naming_conventions.ml \
    src/abstraction_breaker.mli \
    src/abstraction_breaker.ml \
    src/debugger.mli \
    src/unified_value_traversal.mli \
    src/unified_value_traversal.ml \
    src/type_oracle.mli \
    src/type_oracle.ml \
    src/type_helper.mli \
    src/type_helper.ml \
    src/value_printer.mli \
    src/value_printer.ml \
    src/follow_path.mli \
    src/follow_path.ml

LIBMONDA_GDB=libmonda_gdb.so

all: $(GDB_BACKEND) $(SRC)
	$(CC) -c -o gdb_backend/to_gdb.o gdb_backend/to_gdb.c
	$(CC) -c -o gdb_backend/from_gdb.o gdb_backend/from_gdb.c
	$(OCAMLOPT) -output-obj -g -o $(LIBMONDA_GDB) \
	  ocamlcommon.cmxa ocamloptcomp.cmxa dynlink.cmxa bigarray.cmxa \
	  $(SRC) $(GDB_BACKEND) gdb_backend/to_gdb.o \
          gdb_backend/from_gdb.o

.PHONY: clean
clean: 
	@rm -f gdb_backend/*.cm? gdb_backend/*.cm?? gdb_backend/*.o
	@rm -f src/*.cm? src/*.cm?? src/*.o
	@rm -f $(LIBMONDA)
