#
#
#

ifeq ($(OS),"Windows_NT")
FSC := $(shell where fsc)
else
FSC := $(shell which fsharpc)
endif

input := $(shell find . -type f -name "*.fs" -size +0b)
output := $(input:.fs=)

.PHONY: build clean

build: $(output)

$(output): $(input)
	if [ $$(@:%=%.fs) -nt $$@ ]; then \
		$(FSC) --nologo --target:exe --out:$@ $(@:%=%.fs) \
	fi

clean:
	$(foreach executable, $(input:%.fs=%), $(RM) $(executable))
	$(foreach dll, $(shell find . -type f -name "FSharp.Core.dll"), $(RM) $(dll))
