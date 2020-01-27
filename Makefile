#
#
#

ifeq ($(OS),"Windows_NT")
FSC := $(shell where fsc)
else
FSC := $(shell which fsharpc)
endif

input := $(wildcard **/*.fs)
output := $(basename $(input))

.PHONY: build clean

build: $(output)

$(output): $(input)
	$(if [[ $@ -ot $(@:%=%.fs) && -s $(@:%=%.fs) ]], $(FSC) --nologo --target:exe --out:$@ $(@:%=%.fs))


clean:
	$(foreach executable, $(input:%.fs=%), $(RM) $(executable))
	$(foreach dll, $(shell find . -type f -name "FSharp.Core.dll"), $(RM) $(dll))
