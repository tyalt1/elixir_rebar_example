REBAR = rebar3
WORKDIR = /code

.PHONY: all compile clean cleanall docker

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

cleanall: clean
	rm -rf _build/ _elixir_build/

docker:
	-docker run -it --rm -v `pwd`:$(WORKDIR) -w $(WORKDIR) elixir /bin/bash
