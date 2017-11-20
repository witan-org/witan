
BIN=witan.exe

all: $(BIN)
	@cd parsing && $(MAKE) --no-print-directory

clean:
	rm -rf $(BIN)
	cd parsing && $(MAKE) clean

$(BIN):
	@cd .. && $(MAKE) build --no-print-directory
	@ln -f -s ../_build/default/src/bin/witan.exe witan.exe

