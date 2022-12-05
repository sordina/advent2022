
objects := $(shell ls src/ | sed 's/hs/o/' | grep Advent | grep -v b | xargs echo)

.PHONY: all
all: build
	cabal exec -- make every

.PHONY: build
build:
	cabal build

.PHONY: run
run:
	@[ "$(day)" ] || ( echo "Usage: make run day=NN" && exit 1 )
	cabal build
	@echo
	cabal exec advent2022 $(day) < "data/Advent$(day)/input"
	@echo
	cabal exec advent2022 $(day)b < "data/Advent$(day)/input"
	@echo

.PHONY: every
every: $(objects)

.PHONY: doctest-interactive
doctest-interactive:
	find {src,*.cabal} | entr -- cabal exec -- doctest -fdefer-typed-holes -isrc src/*.hs

.PRECIOUS: data/%/input
data/%/input:
	echo $@
	$(eval x = $(shell echo $* | sed -E 's/Advent(..)/\1/'))
	mkdir -p data/Advent$x
	cd data/Advent$x && aoc d -y 2022 -d $x

.PHONY: %.o
%.o: src/%.hs data/%/input
	$(eval x = $(shell echo $< | sed -E 's/src.//; s/Advent(..)\.hs/\1/; s/(0[0-9])/\1/g'))
	@echo
	@echo make $@
	@echo
	advent2022 $(x)  < "data/Advent$(x)/input"
	@echo
	advent2022 $(x)b < "data/Advent$(x)/input"
	@echo
