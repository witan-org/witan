
BIN=../witan.exe
TESTS=$(patsubst %.p,%.res,$(wildcard *.p))

all: $(TESTS)

%.res: %.p $(BIN)
	@if ! $(BIN) --type-only $< > /dev/null; then 							\
			/bin/echo -e "\e[31m[KO]\e[0m [parsing] $<";			\
			exit 1;																			\
		else																					\
			/bin/echo -e "\e[32m[OK]\e[0m [parsing] $<";			\
		fi

clean:
