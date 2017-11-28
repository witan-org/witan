##########################################################################
#  This file is part of Witan.                                           #
#                                                                        #
#  Copyright (C) 2017                                                    #
#    CEA   (Commissariat à l'énergie atomique et aux énergies            #
#           alternatives)                                                #
#    INRIA (Institut National de Recherche en Informatique et en         #
#           Automatique)                                                 #
#    CNRS  (Centre national de la recherche scientifique)                #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
##########################################################################

.PHONY: clean build build-dev test test-dev

J?=3
TIMEOUT?=30
TARGETS=src/bin/witan.exe
OPTS= -j $(J)

build:
	jbuilder build $(TARGETS) $(OPTS)

build-install:
	jbuilder build @install

build-dev:
	jbuilder build $(TARGETS) $(OPTS) --dev

build-all-supported-version:
	jbuilder build $(TARGETS) $(OPTS) --dev --workspace jbuild-workspace.dev


clean:
	jbuilder clean
	cd tests && $(MAKE) clean

install: build-install
	jbuilder install

uninstall:
	jbuilder uninstall

doc:
	jbuilder build @doc

test: build
	@echo "run API tests…"
	@jbuilder runtest
	@echo "run BIN tests…"
	@cd tests && $(MAKE) --no-print-directory

test-dev: build-dev
	@echo "run API tests…"
	@jbuilder runtest --dev
	@echo "run BIN tests…"
	@cd tests && $(MAKE) --no-print-directory

reinstall: | uninstall install

ocp-indent:
	@which ocp-indent > /dev/null || { \
	  	echo 'ocp-indent not found; please run `opam install ocp-indent`'; \
		exit 1 ; \
	  }

reindent: ocp-indent
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make build-dev ; \
	done


###############
# file headers
###############

WHY3_FILES = $(addprefix src/popop_lib/, cmdline.ml cmdline.mli debug.ml	\
	debug.mli exn_printer.ml exn_printer.mli hashcons.ml		\
	hashcons.mli lists.ml lists.mli loc.ml loc.mli number.ml	\
	number.mli opt.ml opt.mli plugin.ml plugin.mli pp.ml pp.mli	\
	print_tree.ml print_tree.mli stdlib.ml stdlib.mli strings.ml	\
	strings.mli sysutil.ml sysutil.mli util.ml util.mli		\
	warning.ml warning.mli weakhtbl.ml weakhtbl.mli )

OCAML_FILES = $(addprefix src/popop_lib/, map_intf.ml exthtbl.ml	\
	exthtbl.mli extmap.ml extmap.mli extset.ml extset.mli )

FRAMAC_FILES = $(addprefix src/popop_lib/, intmap.ml intmap_hetero.ml	\
	intmap.mli intmap_hetero.mli bag.ml bag.mli)

JC_FILES = $(addprefix src/popop_lib/, leftistheap.ml leftistheap.mli)

WITAN_FILES = Makefile  \
	$(filter-out $(WHY3_FILES) $(OCAML_FILES) $(FRAMAC_FILES) $(JC_FILES) \
	, $(wildcard src/*/*.ml* src/*.ml*))

headers:
	headache -c misc/headache_config.txt -h misc/header.txt	\
		$(WITAN_FILES)
	headache -c misc/headache_config.txt -h misc/header_why3.txt	\
		$(WHY3_FILES)
	headache -c misc/headache_config.txt -h misc/header_ocaml.txt	\
		$(OCAML_FILES)
	headache -c misc/headache_config.txt -h		\
		misc/header_framac.txt $(FRAMAC_FILES)
	headache -c misc/headache_config.txt -h		\
		misc/header_jc.txt $(JC_FILES)

.PHONY: clean doc all install uninstall remove reinstall test
