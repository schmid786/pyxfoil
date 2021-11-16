#!/bin/bash

SRCDIR=src
OUTDIR=py

rm -f *.pyf

## Generate signature files
# for global variables
python -m numpy.f2py -h globals.pyf ${SRCDIR}/globals.f90
# for fortran procedures
python -m numpy.f2py -h routines.pyf ${SRCDIR}/xfoil.f only: x_reve x_load x_norm x_xycm x_pcop x_pane x_init x_naca x_inte : \
                                     ${SRCDIR}/xoper.f only: o_visc o_re o_mach o_type o_iter bl_init o_alfa o_cli o_cl o_fmom o_fnew o_vels : \
                                     ${SRCDIR}/xmdes.f only: m_init m_qset m_aq m_cq m_symm m_tgap m_tang m_smoo m_filt m_slop m_exec m_pert

# Replace "(nc)" with ":" in file routines.pyf
sed -i 's/(nc)/:/g' routines.pyf

# Generate libxfoil.pyf
cat > "libxfoil.pyf" <<EOM
python module libxfoil
    interface
        include 'globals.pyf'
        include 'routines.pyf'
    end interface 
end python module libxfoil
EOM

# Create module library
python -m numpy.f2py -c --fcompiler=gfortran --f90flags="-ffixed-form" "libxfoil.pyf" ${SRCDIR}/globals.f90 ${SRCDIR}/*.f
    
mv *.pyf ${OUTDIR}
mv libxfoil* ${OUTDIR}