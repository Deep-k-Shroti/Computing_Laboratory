all : compile_1 run_1

compile_1:
	@gcc -o A5.1.out A5.1.c

run_1:
	@./A5.1.out

clean:
	@rm -r *.out
show_graph:
	@dot -Tps -o GraphViz1.out GraphViz1.dot
	@dot -Tps -o GraphViz2.out GraphViz2.dot
	@dot -Tps -o GraphViz3.out GraphViz3.dot
	@dot -Tps -o GraphViz4.out GraphViz4.dot
