# PN-Stitcher
PN-Stitcher is the source code of pore-network stitching method (PNSM).
This method provides large-enough representative pore-network (PN) for a core that encompasses a larger scale of heterogeneities than is possible using conventional pore-scale modeling approaches while it is still computationally efficient. Details of this method are explained in this [paper](https://doi.org/10.1007/s11242-020-01491-0) and [preprint](https://arxiv.org/pdf/2004.01523.pdf).
<br/><br/>
The original code is written in Fortran (.f90 files) and compiled via GFortran.
The format of input and output PN files is StatOil format. For more details on this format, please see the appendix of [Sochi (2007)](https://arxiv.org/pdf/1011.0760.pdf).
<br/><br/>
In order to compile the source code, you can download and install GNU Compiler Collection from [MinGW](http://www.mingw.org/). After completing the installation, type this line in Windows Command Prompt:
```
gfortran pn_stitcher.f90 > pn_stitcher
```
This should generate the executable file ('pn_stitcher.exe'). In order to run the executable file, after having appropriate input file and PN files in the same folder as the executable file, type this line in Windows Command Prompt:
```
pn_stitcher.exe input.txt
```
The input file ('input.txt') has eight lines. Line 1 and 5 are Fortran comments. Line 2 and 3 are the prefix of PNs at the left and right, respectively. Line 4 is the prefix of the stitched PN. Line 6 and 7 are the folder address of original PNs and Line 8 is the folder address where the stitched PN will be written at.
<br/><br/>
The PN two-phase flow simulation is carried out using [PoreFlow](http://www.imperial.ac.uk/earth-science/research/research-groups/perm/research/pore-scale-modelling/software/two-phase-network-modelling-code/). For more details on this flow solver, please check out [Valvatne and Blunt (2004)](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2003WR002627). Other PN flow solvers can also be used, such as [PN-Flow](https://github.com/aliraeini/pnextract) and [OpenPNM](http://openpnm.org/), that can read PN files in StatOil format.
<br/><br/>
For any questions or comments, please contact me at [kohanpu2@illinois.edu](mailto:kohanpu2@illinois.edu)
