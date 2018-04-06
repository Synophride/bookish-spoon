#
# sample Makefile for Objective Caml
# Copyright (C) 2001 Jean-Christophe FILLIATRE
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License version 2, as published by the Free Software Foundation.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# 
# See the GNU Library General Public License version 2 for more details
# (enclosed in the file LGPL).

# where to install the binaries
prefix=/usr/local
exec_prefix=${prefix}
BINDIR=${exec_prefix}/bin

# where to install the man page
MANDIR=${prefix}/man

# other variables set by ./configure
OCAMLC   = ocamlc.opt
OCAMLOPT = ocamlopt.opt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex.opt
OCAMLYACC= ocamlyacc
OCAMLLIB = /usr/lib/ocaml
OCAMLBEST= opt
OCAMLVERSION = 4.02.3
OCAMLWEB = ocamlweb
OCAMLWIN32 = no
EXT = 

INCLUDES = 
BFLAGS = -annot -g $(INCLUDES)
OFLAGS = -annot $(INCLUDES)

# main target
#############

NAME = top
EXE= $(NAME)$(EXT)
ifeq ($(CAMLBEST),byte)
BYTE	= $(EXE)
OPT	= $(NAME).opt$(EXT)
else
BYTE	= $(NAME).byte$(EXT)
OPT	= $(EXE)
endif

all: $(EXE)

# bytecode and native-code compilation
######################################

CMO = 	parser.cmo \
	lexer.cmo \
	top.cmo

CMX = $(CMO:.cmo=.cmx)

GENERATED = lexer.ml parser.ml parser.mli

$(BYTE): $(CMO)
	$(OCAMLC) $(BFLAGS) -o $@ $^

$(OPT): $(CMX) 
	$(OCAMLOPT) $(OFLAGS) -o $@ $^

# generic rules
###############

.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly

.mli.cmi:
	$(OCAMLC) -c $(BFLAGS) $<

.ml.cmo:
	$(OCAMLC) -c $(BFLAGS) $<

.ml.o:
	$(OCAMLOPT) -c $(OFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(OFLAGS) $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<


# Makefile is rebuilt whenever Makefile.in or configure.in is modified
######################################################################

Makefile: Makefile.in config.status
	./config.status

config.status: configure
	./config.status --recheck

# clean
#######

clean:: 
	rm -f *.cm[iox] *.o *~ *.annot
	rm -f $(GENERATED) parser.output
	rm -f $(BYTE) $(OPT)
	rm -f *.aux *.log

dist-clean distclean:: clean
	rm -f Makefile config.cache config.log config.status

# depend
########

.depend depend:: $(GENERATED)
	rm -f .depend
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .depend

include .depend
