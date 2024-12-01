
.PHONY: build
build:
	dune build -w --terminal-persistence=clear-on-rebuild-and-flush-history
	dune build --terminal-persistence=clear-on-rebuild-and-flush-history

.PHONY: watch
watch:
	dune build -w --terminal-persistence=clear-on-rebuild-and-flush-history

.PHONY: test-watch
test-watch:
	dune runtest -w --terminal-persistence=clear-on-rebuild-and-flush-history

.PHONY: lock
lock:
	nix develop -f default.nix lock

.PHONY: shell
shell:
	nix develop -f default.nix -j auto -i -k TERM -k PATH -k HOME -v shell
