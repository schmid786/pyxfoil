# pyxfoil
Python interface to XFOIL core functions

Goal of this project is to provide a fast interface to the [XFOIL](https://web.mit.edu/drela/Public/web/xfoil/) code without the need of file-IO or the need to call subprocesses. Therefore the XFOIL source code is stripped down to its core functions and a python module is created via the numpy [f2py](https://numpy.org/doc/stable/f2py/) tool.

Unnecessary functions, subroutines and global variables of XFOIL, like procedures for plotting or user interaction, have been removed. And new subroutines have been inserted to enable the use of the core XFOIL functionality. Most of XFOIL's common blocks have been replaced by module variables to ease the use from within python. Not available are all XFOIL functions which needed user interaction via mouse inputs. And also the polar accumulation functionality of the OPER submenu has been removed as this feature can be realized with a simple python loop (see example [polar](https://github.com/schmid786/pyxfoil/blob/main/exp/polar.ipynb)).

Currently only the OPER and MDES submenus are accessable via the python interface. Futher submenus can be added if necessary.
