all : compile_1 run_1

compile_1:
	@gcc -o SkipList.out SkipList.c

run_1:
	@./SkipList.out
	@dot -Tps -o GraphViz.out GraphViz.dot 
clean:
	@rm -r *.out
