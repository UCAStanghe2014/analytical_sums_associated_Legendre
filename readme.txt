The source code in the three folders (Fortran90, GNU_Octave, and Mathematica) can correctly calculate the sums of the series listed in the paper.
The file named "text.*" in the directory of Fortran90 and GNU_Octave is the main program to generate Table 1 and other files named "*pnm_sum.*" are the key subroutines of the analytical sums.
Please reference the paper for the introduction of all formulae

How to use it?
1. Within the directory of Fortran90, type the following two commands
gfortran *.f90 -o test.exe
./test.exe
2. Within GNU_Octave, run the test.m for an example.
3. Within Mathematica, run "sums_formulas.nb". All the formulas are given by three variable p, dp, and ddp. Take p[[1,;;]] as an example, the first element of it is ID, the second of it is the explicit form of the series, and the third of it is the analytical sum. dp and ddp are the first and second derivates of p with respect to theta.

The files in the folder (For_this_paper) contains the source files to generate the tables and figures in this paper.

By Dr. He Tang and Pro. Wenke Sun from the University of Chinese Academy of Sciences.
Please feel free to Email me (tanghe@ucas.edu.cn) if it exists bugs. Thank you!
Please use it under the GNU General Public License.