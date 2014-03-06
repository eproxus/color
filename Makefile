all: compile

compile:
	@./rebar compile skip_deps=true

readme:
	@./rebar -C rebar.doc.config get-deps
	@./rebar -C rebar.doc.config compile
	@./rebar -C rebar.doc.config doc

clean:
	@./rebar clean