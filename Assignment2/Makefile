all: compile_all run__shell
	
compile_all:
	
	@echo Compliling all the Processes...
	@gcc -o mypwd.out mypwd.c
	@gcc -o mymkdir.out mymkdir.c 	#gcc -o mycd.out mycd.c
	@gcc -o myrm.out myrm.c
	@gcc -o mymv.out mymv.c
	@gcc -o myps.out myps.c
	@gcc -o myls.out myls.c
	@gcc -o mytail.out mytail.c
	@gcc -o shell.out shell.c

run__shell:
	@echo **************************SHELL**************************
	@echo
	@echo
	@./shell.out 

clean:
	@ rm -r *.out