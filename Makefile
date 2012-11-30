REBAR=`which rebar || which ./rebar`
DIALYZER=`which dialyzer`
CTRUN=`which ct_run`

all: deps compile
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
app.plt:
	@$(DIALYZER) --build_plt --output_plt app.plt --apps erts kernel stdlib
dialyze: app.plt compile
	@$(DIALYZER) -q --plt app.plt -n ebin -Wunmatched_returns -Werror_handling -Wrace_conditions
xref:
	@$(REBAR) xref
test: dialyze xref
	@$(REBAR) eunit
clean:
	@$(REBAR) clean

.PHONY: all test clean dialyze deps
