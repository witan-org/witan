
BIN=../witan.exe
TESTS=$(patsubst %.p,%.res,$(wildcard *.p))

all: $(TESTS)

%.res: %.p
	@if ! $(BIN) $< > $@; then 											\
			echo -e "\e[1;31m[KO]\e[0m [parsing] $f";		\
		else																					\
			echo -e "\e[32m[OK]\e[0m [parsing] $f";			\
		fi

clean:
	rm -rf *.res

