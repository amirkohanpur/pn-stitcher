# PN-Stitcher
PN-Stitcher is the source code of pore-network stitching method (PNSM). Details of this method are explained in this [preprint](https://arxiv.org/pdf/2004.01523.pdf).
<br/><br/>
The original code is written in Fortran (.f90 files) and compiled via GFortran.
The format of input and output pore-network files is StatOil format. For more details on this format, please see the appendix of [Sochi (2007)](https://arxiv.org/pdf/1011.0760.pdf).
<br/><br/>
In order to run the code, after having appropriate input file and pore-networks files, type this line in Windows Command:
```
pn_stitcher.exe input.txt
```
The file 'input.txt' has eight lines. Line 1 and 5 are Fortran comments. Line 2 and 3 are the prefix of pore-networks at the left and right, respectively. Line 4 is the prefix of the stitched pore-network. Line 6 and 7 are the folder address of original pore-networks and Line 8 is the folder address where the stitched pore-network will be written at.
<br/><br/>
For any questions or comments, please contact me at [kohanpu2@illinois.edu](kohanpu2@illinois.edu)
