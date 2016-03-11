all: compile_all run_shell

compile_all:

	@echo Compliling all...
	@gcc -o manager.out manager.c
	@gcc -o chef.out chef.c -pthread
	@gcc -o waiter.out waiter.c -pthread
	@gcc -o reception.out reception.c
	@gcc -o service_room_1.out service_room_1.c -pthread
	@gcc -o service_room_2.out service_room_2.c -pthread
	@gcc -o service_room_3.out service_room_3.c -pthread



run_shell:
	@echo **************************Assignment_4**************************
	@./manager.out $(INPUT)


clean:
	@ rm -r *.out
