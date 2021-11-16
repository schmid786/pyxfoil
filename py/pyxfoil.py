import numpy as np
import sys

import libxfoil

class PyXFoilMDes:
    def __init__(self, lib):
        libxfoil = lib
        libxfoil.m_init()
    
    def qset(self):
        libxfoil.m_qset()
    
    def aq(self, alphas):
        alphas = np.asarray(alphas, dtype=np.float32)
        libxfoil.m_aq(alphas)
    
    def cq(self, cls):
        cls = np.asarray(cls, dtype=np.float32)
        libxfoil.m_cq(cls)
        
    def symm(self):
        libxfoil.m_symm()
        
    def tgap(self, dx, dy):
        libxfoil.m_tgap(dx, dy)
        
    def tang(self, ang):
        self._lib.m_tang(ang)
    
    def smoo(self):
        libxfoil.m_smoo()
        
    def filt(self):
        libxfoil.m_filt()
        
    def slop(self):
        libxfoil.m_slop()
        
    def exec(self):
        libxfoil.m_exec()
        
    def pert(self):
        libxfoil.m_pert()
    
    def __getattr__(self, name):
        attobj = getattr(self.__class__, name, None)
        if attobj is not None:
            # If attribute is defined with decorators (@property and @???.setter)
            return attobj.__get__(name)
        elif hasattr(libxfoil.cmod, name):
            # If attribute is part of global circle variables
            return getattr(libxfoil.cmod, name)
        else:
            # If attribute is unknown
            raise AttributeError()
    

class PyXFoilOper:
    def __init__(self, lib):
        libxfoil = lib
    
    @property
    def mach(self):
        return self.minf1
        
    @mach.setter
    def mach(self, val):
        libxfoil.o_mach(val)
        
    @property
    def re(self):
        return libxfoil.xmod.reinf1
        
    @re.setter
    def re(self, val):
        # Set new Reynolds number
        libxfoil.o_re(val)
        
    def alpha(self, alpha):
        libxfoil.o_alfa(alpha)
        return bool(self.lvisc) and bool(self.lvconv)
            
    def visc(self, re=None):
        re = self.re if re is None else re
        libxfoil.o_visc(re)
        
    def __getattr__(self, name):
        attobj = getattr(self.__class__, name, None)
        if attobj is not None:
            # If attribute is defined with decorators (@property and @???.setter)
            return attobj.__get__(name)
        elif hasattr(libxfoil.xmod, name):
            # If attribute is part of global xfoil variables
            return getattr(libxfoil.xmod, name)
        else:
            # If attribute is unknown
            raise AttributeError()
        

class PyXFoil:
    def __init__(self):
        # Initialize global variables
        libxfoil.x_init()
        
        self._mdes = None
        self._oper = None
        
    @property
    def mdes(self):
        if self._mdes is None:
            if self.n == 0:
                raise RuntimeError('No airfoil available')
            self._mdes = PyXFoilMDes(libxfoil)
        return self._mdes
        
    @property
    def oper(self):
        if self._oper is None:
            if self.n == 0:
                raise RuntimeError('No airfoil available')
            self._oper = PyXFoilOper(libxfoil)
        return self._oper
        
    def inte(self, af1, af2, frac):
        libxfoil.x_inte(af1, af2, frac)
    
    def load(self, filename, element=1):
        # Load buffer airfoil coordinates from file
        libxfoil.x_load(filename, element)
        self._mdes = None
        self._oper = None
    
    def naca(self, ides):
        libxfoil.x_naca(ides)
        self._mdes = None
        self._oper = None
        
    def norm(self):
        libxfoil.x_norm()
        
    def pane(self):
        libxfoil.x_pane()
        
    def pcop(self):
        libxfoil.x_pcop()
        
    def reve(self):
        libxfoil.x_reve()
        
    def xycm(self, x, y):
        libxfoil.x_xycm(x, y)
    
    def __getattr__(self, name):
        if (attobj:= getattr(self.__class__, name, None)) is not None:
            # If attribute is defined with decorators (@property and @???.setter)
            return attobj.__get__(name)
        elif hasattr(libxfoil.xmod, name):
            # If attribute is part of global xfoil variables
            return getattr(libxfoil.xmod, name)
        else:
            # If attribute is unknown
            raise AttributeError()