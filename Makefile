REBAR = rebar

.PHONY: all compile clean

all: deps compile

deps:
	@$(REBAR) get-deps compile

compile:
	@$(REBAR) compile

clean:
	rm -rf ebin/

cleanall: clean
	rm -rf deps/

docker:
	docker run -it --rm -v `pwd`:/code elixir /bin/bash
