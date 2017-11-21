
.PHONY: clean build build-dev

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

.PHONY: clean doc all install uninstall remove reinstall test
