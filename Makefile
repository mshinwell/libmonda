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

# Building libmonda requires a PIC-enabled OCaml toolchain.  Produce that
# by adding "-fPIC" to asmrun/Makefile and configuring thus:
#   ./configure -prefix ... -cc "gcc -fPIC" -aspp "gcc -c -fPIC"

GDB_ROOT=/mnt/local/sda1/mshinwell/mshinwell-gdb

OCAML_ROOT=`ocamlopt -where`

OCAMLOPT=ocamlopt -verbose -I +compiler-libs -g -fPIC -ccopt -fPIC -ccopt -g \
  -ccopt -I$(OCAML_ROOT) \
  -ccopt -I$(GDB_ROOT)/gdb \
  -ccopt -I$(GDB_ROOT)/gdb/common \
  -ccopt -I$(GDB_ROOT)/gdb/config \
  -ccopt -DHAVE_CONFIG_H \
  -ccopt -I$(GDB_ROOT)/include \
  -ccopt -I$(GDB_ROOT)/gdb/gnulib/import \
  -ccopt -I$(GDB_ROOT)/gdb/build-gnulib/import

GDB_BACKEND=gdb_backend/from_gdb.c \
  gdb_backend/to_gdb.c \
  gdb_backend/gdb_debugger.ml

SRC=src/abstraction_breaker.ml \
    src/cmt_cache.ml \
    src/cmt_file.ml \
    src/monda_debug.ml \
    src/naming_conventions.ml \
    src/type_oracle.ml \
    src/value_printer.ml

LIBMONDA_GDB=libmonda_gdb.so

all: $(GDB_BACKEND) $(SRC)
	$(OCAMLOPT) -output-obj -o $(LIBMONDA_GDB) \
	  ocamlcommon.cmxa ocamloptcomp.cmxa dynlink.cmxa bigarray.cmxa \
	  $(GDB_BACKEND) $(SRC)

.PHONY: clean
clean: 
	@rm -f gdb_backend/*.cm? gdb_backend/*.cm?? gdb_backend/*.o
	@rm -f src/*.cm? src/*.cm?? src/*.o
	@rm -f $(LIBMONDA)
