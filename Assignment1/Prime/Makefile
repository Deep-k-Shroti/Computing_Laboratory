all_Part1: compile_PrimeFactorisation run_PrimeFactorisation

all_Part2: compile_PrimeFactMultProcess run_PrimeFactMultProcess

all: compile_PrimeFactorisation run_PrimeFactorisation compile_PrimeFactMultProcess run_PrimeFactMultProcess

compile_PrimeFactorisation:
	@echo **************************Prime Factorisation**************************
	@echo Using a single process:
	@gcc -o PrimeFactorisation.out PrimeFactorisation.c -lm

run_PrimeFactorisation:
	@./PrimeFactorisation.out

compile_PrimeFactMultProcess:
	@echo
	@echo Using multiple process:
	@gcc -o PrimeFactMultProcess.out PrimeFactMultProcess.c -lm

run_PrimeFactMultProcess:
	@./PrimeFactMultProcess.out

clean:
	@ rm -r *.out