all_Part1: compile_VowelsConsonant run_VowelsConsonant

all_Part2: compile_VowelConsonantMultProcess run_VowelConsonantMultProcess

all: compile_VowelsConsonant run_VowelsConsonant compile_VowelConsonantMultProcess run_VowelConsonantMultProcess
	
compile_VowelsConsonant:
	@echo
	@echo
	@echo **************************Counting vowels and consonants**************************
	@echo Using a single process:
	@gcc -o VowelsConsonant.out VowelsConsonant.c 

run_VowelsConsonant:
	@./VowelsConsonant.out $(INPUT)
	
compile_VowelConsonantMultProcess:
	@echo
	@echo Using multiple process:
	@mkdir result 			#To make a result folder to store the vowel and consonant values of input files in a <process_id.txt> file
	@gcc -o VowelConsonantMultProcess.out VowelConsonantMultProcess.c 

run_VowelConsonantMultProcess:
	@./VowelConsonantMultProcess.out $(INPUT)
	@rm -r result 			#To delete the result directory + previous existing <process_id.txt> files

clean:
	@ rm -r *.out
