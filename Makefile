#
#
#

ifeq ($(OS),"Windows_NT")
FSC := $(shell where fsc)
else
FSC := $(shell which fsharpc)
endif

makefiles := $(wildcard **/Makefile)

.PHONY: all clean

all: examples introductory_problems problem_set_1 problem_set_2

problem_set_2:
	$(MAKE) -C problem_set_2

problem_set_1:
	$(MAKE) -C problem_set_1

introductory_problems:
	$(MAKE) -C introductory_problems

examples:
	$(MAKE) -C examples

clean: $(output)
	$(foreach executable, $(output), $(RM) $(executable))
	$(foreach dll, $(shell find . -type f -name "FSharp.Core.dll"), $(RM) $(dll))
