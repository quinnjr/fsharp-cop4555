#
#
#

ifeq ($(OS),"Windows_NT")
FSC := $(shell where fsc)
else
FSC := $(shell which fsharpc)
endif

#input := $(shell find . -type f -name "*.fs" -size +0b)
input := $(wildcard **/*.fs)
output := $(basename $(input))

.PHONY: build clean

build: $(output)

$(output): $(input)
	[[ $@ -ot $(addsuffix .fs, $@) && -s $(addsuffix .fs, $@) ]] && \
	  $(FSC) --nologo --target:exe --out:$@ $(addsuffix .fs, $@)


clean:
	$(foreach executable, $(input:%.fs=%), $(RM) $(executable))
	$(foreach dll, $(shell find . -type f -name "FSharp.Core.dll"), $(RM) $(dll))
