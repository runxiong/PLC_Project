---------------------------------------------------------------------------
Compiling and loading the micro-ML evaluator and parser (README.TXT)
---------------------------------------------------------------------------


--------------------
Windows Instructions
--------------------

1. 
Move the whole folder containing this file (Hw6) to a folder of 
your choice. Here we assume that you have put it on the Desktop.

2.
To generate and compile the lexer and parser for the PLC language, run
the Command Prompt application. At the prompt, enter the following commands,
one at time:

   cd H:\Desktop\Hw6
   bin\fslex --unicode PlcLexer.fsl
   bin\fsyacc --module PlcParser PlcParser.fsy

This assumes that your Desktop folder is in drive H, which is the case for the
CS Windows server. If it is in another drive, use the name of that drive instead.

3. 
To use the parser inside F# interactive, see main.fsx


-----------------
MacOS instructions
-----------------
1.
Move the whole folder containing this file (Hw6) to a folder of 
your choice. 

2.
To generate and compile the lexer and parser for the PLC language, run
the Terminal application. Enter the following commands at the terminal prompt, 
one at time, replacing "runxiongdong" with your user name:

   cd /Users/runxiongdong/Desktop/Hw6
   mono bin/fslex.exe --unicode PlcLexer.fsl
   mono bin/fsyacc.exe --module PlcParser PlcParser.fsy

3. 
To use the parser inside F# interactive, see main.fsx
