
.PHONY: lock
lock:
	nix develop -f default.nix lock

.PHONY: shell
shell:
	nix develop -f default.nix -j auto -i -k TERM -k PATH -k HOME -v shell

.PHONY: all
all:
	nix build -f default.nix -j auto -v

.PHONY: watch-test
watch-test:
	dune runtest -w --terminal-persistence=clear-on-rebuild-and-flush-history

.PHONY: watch
watch:
	dune build -w --terminal-persistence=clear-on-rebuild-and-flush-history
