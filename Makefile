.PHONY: examples

all: examples

examples: $(COMPILER)
	$(MAKE) -C ./examples
