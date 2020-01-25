#
#
#

ifeq ($(OS),"Windows_NT")
FSC := $(shell where fsc)
else
FSC := $(shell which fsharpc)
endif

INPUT = $(wildcard **/*.fs)
OUTPUT = $(INPUT:.fs=)

.PHONY: build clean

build: $(OUTPUT)

$(OUTPUT): $(INPUT)
	$(FSC) --nologo --target:exe --out:$@ $<

clean:
	$(foreach executable, $(INPUT:.fs=), $(RM) $(executable))
	$(foreach dll, $(shell find . -type f -name "FSharp.Core.dll"), $(RM) $(dll))
