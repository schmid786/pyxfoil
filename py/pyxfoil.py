import numpy as np
import sys

import libxfoil

class PyXFoilMDes:
    def __init__(self, lib):
        self.__lib = lib
        self.__lib.m_init()
    
    def qset(self):
        self.__lib.m_qset()
    
    def aq(self, alphas):
        alphas = np.asarray(alphas, dtype=np.float32)
        self.__lib.m_aq(alphas)
    
    def cq(self, cls):
        cls = np.asarray(cls, dtype=np.float32)
        self.__lib.m_cq(cls)
        
    def symm(self):
        self.__lib.m_symm()
        
    def tgap(self, dx, dy):
        self.__lib.m_tgap(dx, dy)
        
    def tang(self, ang):
        self._lib.m_tang(ang)
    
    def smoo(self):
        self.__lib.m_smoo()
        
    def filt(self):
        self.__lib.m_filt()
        
    def slop(self):
        self.__lib.m_slop()
        
    def exec(self):
        self.__lib.m_exec()
        
    def pert(self):
        self.__lib.m_pert()
    
    def __getattr__(self, name):
        attobj = getattr(self.__class__, name, None)
        if attobj is not None:
            # If attribute is defined with decorators (@property and @???.setter)
            return attobj.__get__(name)
        elif hasattr(self.__lib.cmod, name):
            # If attribute is part of global circle variables
            return getattr(self.__lib.cmod, name)
        else:
            # If attribute is unknown
            raise AttributeError()
    

class PyXFoilOper:
    def __init__(self, lib):
        self.__lib = lib
    
    @property
    def mach(self):
        return self.minf1
        
    @mach.setter
    def mach(self, val):
        self.__lib.o_mach(val)
        
    @property
    def re(self):
        return self.__lib.xmod.reinf1
        
    @re.setter
    def re(self, val):
        # Set new Reynolds number
        self.__lib.o_re(val)
        
    def alpha(self, alpha):
        self.__lib.o_alfa(alpha)
        return bool(self.lvisc) and bool(self.lvconv)
            
    def visc(self, re=None):
        re = self.re if re is None else re
        self.__lib.o_visc(re)
        
    def __getattr__(self, name):
        attobj = getattr(self.__class__, name, None)
        if attobj is not None:
            # If attribute is defined with decorators (@property and @???.setter)
            return attobj.__get__(name)
        elif hasattr(self.__lib.xmod, name):
            # If attribute is part of global xfoil variables
            return getattr(self.__lib.xmod, name)
        else:
            # If attribute is unknown
            raise AttributeError()
        

class PyXFoil:
    def __init__(self):
        # Initialize global variables
        self.__lib.x_init()
        
        self._mdes = None
        self._oper = None
        
    @property
    def mdes(self):
        if self._mdes is None:
            if self.n == 0:
                raise RuntimeError('No airfoil available')
            self._mdes = PyXFoilMDes(self.__lib)
        return self._mdes
        
    @property
    def oper(self):
        if self._oper is None:
            if self.n == 0:
                raise RuntimeError('No airfoil available')
            self._oper = PyXFoilOper(self.__lib)
        return self._oper
        
    def inte(self, af1, af2, frac):
        self.__lib.x_inte(af1, af2, frac)
    
    def load(self, filename, element=1):
        # Load buffer airfoil coordinates from file
        self.__lib.x_load(filename, element)
        self._mdes = None
        self._oper = None
    
    def naca(self, ides):
        self.__lib.x_naca(ides)
        self._mdes = None
        self._oper = None
        
    def norm(self):
        self.__lib.x_norm()
        
    def pane(self):
        self.__lib.x_pane()
        
    def pcop(self):
        self.__lib.x_pcop()
        
    def reve(self):
        self.__lib.x_reve()
        
    def xycm(self, x, y):
        self.__lib.x_xycm(x, y)
    
    def __getattr__(self, name):
        if (attobj:= getattr(self.__class__, name, None)) is not None:
            # If attribute is defined with decorators (@property and @???.setter)
            return attobj.__get__(name)
        elif hasattr(self.__lib.xmod, name):
            # If attribute is part of global xfoil variables
            return getattr(self.__lib.xmod, name)
        else:
            # If attribute is unknown
            raise AttributeError()