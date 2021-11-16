@echo off

set SRCDIR=src
set OUTDIR=py

del /q *.pyf

REM Generate signature files
REM for global variables
python -m numpy.f2py -h globals.pyf %SRCDIR%\globals.f90
REM for fortran procedures
python -m numpy.f2py -h routines.pyf %SRCDIR%\xfoil.f only: x_reve x_load x_norm x_xycm x_pcop x_pane x_init x_naca x_inte : ^
                                     %SRCDIR%\xoper.f only: o_visc o_re o_mach o_type o_iter bl_init o_alfa o_cli o_cl o_fmom o_fnew o_vels : ^
                                     %SRCDIR%\xmdes.f only: m_init m_qset m_aq m_cq m_symm m_tgap m_tang m_smoo m_filt m_slop m_exec m_pert

(
    echo python module libxfoil
    echo     interface
    echo         include 'globals.pyf'
    echo         include 'routines.pyf'
    echo     end interface
    echo end python module libxfoil
) > libxfoil.pyf

REM Create module library
python -m numpy.f2py -c --compiler=mingw32 --fcompiler=gfortran --f90flags="-ffixed-form" libxfoil.pyf %SRCDIR%\globals.f90 ^
    %SRCDIR%\aread.f %SRCDIR%\naca.f %SRCDIR%\spline.f %SRCDIR%\xbl.f %SRCDIR%\xblsys.f %SRCDIR%\xfoil.f %SRCDIR%\xgdes.f ^
    %SRCDIR%\xgeom.f %SRCDIR%\xmdes.f %SRCDIR%\xoper.f %SRCDIR%\xpanel.f %SRCDIR%\xqdes.f %SRCDIR%\xsolve.f %SRCDIR%\xtcam.f %SRCDIR%\xutils.f

move *.pyf %OUTDIR%
move libxfoil* %OUTDIR%