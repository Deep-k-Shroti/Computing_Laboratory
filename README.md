# Computing_Laboratory
##Computing_Laboratory_IITKGP Autumn 2015.

This repository contains all Computing Lab Assignments. The Experiment are written in C and R.

##Directories included <br />
/Assignment1 <br />
/Assignment2 <br />
/Assignment3 <br />
/Assignment4 <br />
/Assignment5 <br />
/Assignment6 <br />
/Assignment7 <br />
/Assignment8 <br />
/Assignment9 <br />
/Assignment10 <br />
Each directory includes the experiment problem statement(.pdf), Makefile, Doxyfile and source code(.c).

##The list of experiments:
1. Forking <br />
__Objective:__
To exercise students in finding parallelism in a problem and using multiple processes to efficiently solve the problem. Implement the below problems using Single Process & using Multiple Process

	1. Prime Factorization  
	To exercise students in parallel programming through the use of fork system call and use of file handling functions.
	Input​:  A suitably large integer , say “n”, where the number is a product of exactly two prime numbers
	Output​:
		* The two prime factors of the input integer.  
		* Time taken to execute the program.

	2. Counting vowels and consonants
	Data Analytics often involves processing large number of huge files. This takes a lot of time.The time can be reduced using parallel processing. This assignment illustrates how parallel programming can be used to efficiently solve problems of this genre.
		1. Input​:  
			* One or more text (.txt) files.
			* sample input : ­ ./a.out inFile1.txt inFile2.txt inFile3.txt
		2.	Output:
 			* Total number of vowels in the input files.
			* Total number of consonants in the input files.
			* Time taken to execute the program.
​
2. DEVELOPING A CUSTOM LINUX SHELL <br />
__Objective:__
To   understand   the   working   of   a   command   shell   involving   aspects   such   as  creating a process, loading a program and also get an insight into the /proc directory.
To implement linux commands as follows:
	1. mypwd ­ Print the present working directory to ​STDOUT.
	2. mymkdir ​Creates a single directory  or multiple directories.  <br />
		Sample Inputs:
	 	* mymkdir dir1
		* mymkdir dir1 dir2 dir2/dir3
		* mymkdir  /rootpath/subdir/subdir2
	3. mycd ­ Change Current Working Directory (CWD) to specified directory.
	4. myrm ​ Remove a directory or a file. If a directory is to be removed, the files inside the directory should be deleted prior to deletion of the directory.<br />
		Sample Inputs:  ​
		* myrm file1 file2 file3
		* myrm ­ ​d dir1 (Remoce an empty directory)
		* myrm  ​­r dir2 (Remove a directory and its contents) ​
	5. mymv ­ Move a file/Directory from one location to another​​. <br />
		Sample Inputs:
		* mymv sourceDirectory  targetDirectory  
		* mymv ​sourceFile  targetFile
	6. myps ​List all processes for the current user without regards to the present terminal. <br />  
		Options: a List all processes running on the system.
	7. myls ​ List the Directory contents. Output should be same as ​ls –l on a linux shell.
	8. mytail ­​Print the last ‘n’ lines of the input file. <br />
	Options:
		* <N> Number of lines from the end to print
		* <filename> Filename of the input file <br />

3. Linux process management <br />
 __Objective:__        ​
To understand Linux process management and how i/o redirection and pipes work. Augment your mini command shell to include the facility for redirecting the output to a file
and taking the input from a file. Additionally, your shell should also support the use of pipes i.e “|”.
The Complete Linux Shell must contains
	* Input / Output Redirection​
	* Pipes
	* Background Process
	* Command History (not done).

4. The Maach­ Dal ­Bhaat Problem  (Smoker Problem)
Refer to the experiment problem statement in the Assignment4 folder for detailed explanation.

5. Skiplist <br />
__Objective:__
To introduce you to the Skip List data structure including implementation of various operations on it.
The operations are described below:­
	1. insertp(SkipList*, int key, char * val, float p) Insert   a   node   into   the   skip   list   with   the   key   and   value   as   given   using   ​p  as   the   probability   for   calculating   node   height.   If   the   key   already   exists, then just update the value with this value.  
	2. inserth(SkipList*, int key, char * val, int h) Insert   a   node   into   the   skip   list   with   the   key   and   value   as   given   using   ​h  as   the   node   height.   If   the   key   already   exists,   then   just   update   the   value  with this value.  
	3. delete(SkipList*, int key) Delete the node from the SkipList  with the given key  
	4. find(SkipList*, int key)
	5. Find in the SkipList the value corresponding to the key provided.
	6. display(SkipList*)  
Output the skip list as a GraphViz DOT document.

6.  SkipList Continued <br />
__Objective:__
Given   multiple   skip   lists   containing   the   details   of   students   enrolled   in   different subjects   and   a   master   list   (containing   all   the   students’   details),   you   need   to   provide  
answers   to   different   queries, display   the   number   of  comparisons   taken   for   the   same   and   produce   the   trace   for   the   path   taken   as   a   graphviz  DOT File.

7. Text Processing and Search with R <br />
__Objectives:__
	* Learn basics of text processing in R
	* Indexing Documents (TF­IDF)
	* Plotting in R
Given   a   corpus   of   Amazon   food   reviews,   Use   R   programming   language   to  process   the   data   and   build   a   simple   command   line   search   engine.   When   given   a   query, it should retrieve top 25 documents relevant to the query.


8. Ranking Documents with Eigenvector Centrality <br />
__Objectives:__
Static ranking of authors of reviews (Eigenvector Centrality)
You are given a corpus of Amazon food reviews. Using R, process the data and build a command line search engine. The system when given a query, should retrieve the top 10 documents relevant to the query.

9. Introduction to Hadoop <br />
__Objective:__
	1. Study Hadoop and MapReduce.
	2. Write   the   record­reader   routine,   to   read   a   csv   file   and   extract   information.
	3. Write   the   Mapper,   Combiner   and   Reducer   routines   (those   necessary)   to   implement   the  queries mentioned in the following sections.
	4. Write the output­format routine to print results of queries to a ​.txt file. <br />
Write the code for these queries in R.

10. Write K-Means in R
Write a version of simple K-Means in R and name the file KmeansHomework.R. The function signatures should be similar to those of simpleKMeans(), simpleAssignToCentroids(), and simpleDetermineCentroids().
Execute the built in tests and verify that your code works:
	* plotClustering() – already implemented
	* simpleKMeans() – already implemented
	* simpleDetermineCentroids() – needs to be implemented
	* simpleAssignToCentroids() – needs to be implemented

##Setup
1. For C codes
	* Download the directory
	* Open the terminal.
	* Go to the directory containing the C code .
	*  Run from linux terminal Make\

2. R Codes
	* Download the directory
	* Use RStudio
