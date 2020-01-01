
SITEXEC=./dist-newstyle/build/x86_64-linux/ghc-8.6.4/dwarfmaster-website-0.1.0.0/x/site/build/site/site

all: build

build:
	nix-shell --command "cabal new-build --reorder-goals site.hs"
	$(SITEXEC) build

configure:
	nix-shell --command "cabal new-configure"

doc:
	nix-shell --command "cabal new-haddock --executables"

hoogle:
	nix-shell --command "cabal new-haddock --executables --hoogle"

ghci:
	nix-shell --command "cabal new-repl"

exec:
	$(SITEXEC) server

.PHONY: all build configure doc hoogle ghci


