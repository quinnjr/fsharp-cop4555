#
#
#

.PHONY: all clean

all: examples introductory_problems problem_set_1 problem_set_2

examples:
	$(MAKE) -C examples

introductory_problems:
	$(MAKE) -C introductory_problems

problem_set_1:
	$(MAKE) -C problem_set_1

problem_set_2:
	$(MAKE) -C problem_set_2

clean:
	$(MAKE) -C examples clean
	$(MAKE) -C introductory_problems clean
	$(MAKE) -C problem_set_1 clean
	$(MAKE) -C problem_set_2 clean
