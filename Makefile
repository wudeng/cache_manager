REBAR = ./rebar3

.PHONY: all rel clean prod gen run

all:
	${REBAR} compile

deps:
	${REBAR} get-deps

release:
	${REBAR} release

clean:
	${REBAR} clean
	rm -f erl_crash.dump rebar3.crashdump

prod:rel
	${REBAR} as prod tar

gen:
	erl -pa _build/default/lib/*/ebin -config config/sys.config -s db_utils gen_header_file -s init stop -noshell
	mv table.hrl apps/cache_manager/include/

run:
	_build\default\rel\cache_manager\bin\cache_manager.cmd console
