#
#
#

ifeq ($(OS),Windows_NT)
	FSC := $(shell where fsc)
	MONO := ""
else
	FSC := $(shell which fsharpc)
	MONO := $(shell which mono)
endif

INPUTS = $(shell find . -type f -name '*.fs')
OUTPUTS = ${INPUTS:.fs=}

.PHONY: all clean

all: ${OUTPUTS}

${OUTPUTS}: ${INPUTS}
	$(FSC) --nologo --nocopyfsharpcore --target:exe -o:$@ $<

clean:
	$(RM) -f $(OUTPUTS) $(shell find . -type f -name 'FSharp.Core.dll')
