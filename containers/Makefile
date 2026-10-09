# Thin wrappers around bin/kc so the build logic lives in exactly one place.
# Runtime: KC_RUNTIME=podman|docker (default: podman if installed, else docker).
#
#   make build-base            kc-base
#   make build-render          kc-render (chezmoi render stage)
#   make build TOOL=nvim       one tool image (builds base/render as needed)
#   make build-toolbox         kc-toolbox
#   make build-all             base + every tool in toolbox/tools.list + toolbox
#   make test TOOL=nvim        smoke.sh offline (--network none); TOOL=toolbox works too
#   make test-all              every built tool + toolbox
#   make lint                  shellcheck the shell sources
#   make clean                 remove kc-*-state volumes

KC := ./bin/kc
TOOL ?=

.PHONY: build-base build-render build build-toolbox build-all test test-all lint clean doctor

build-base:
	$(KC) build base

build-render:
	$(KC) build render

build:
	@test -n "$(TOOL)" || { echo "usage: make build TOOL=<name>"; exit 2; }
	$(KC) build $(TOOL)

build-toolbox:
	$(KC) build toolbox

build-all:
	$(KC) build all

test:
	@test -n "$(TOOL)" || { echo "usage: make test TOOL=<name>"; exit 2; }
	$(KC) test $(TOOL)

test-all:
	@for t in $$(cat toolbox/tools.list) toolbox; do $(KC) test $$t || exit 1; done

lint:
	shellcheck -s sh -e SC2046,SC2086,SC1090,SC1091 bin/kc base/entrypoint.sh base/kc-bake \
	    base/kc-tool-install render/render.sh tools/*/install.sh tools/*/smoke.sh toolbox/*.sh

clean:
	$(KC) clean

doctor:
	$(KC) doctor
