C***********************************************************************
C    Module:  xfoil.f
C 
C    Copyright (C) 2000 Mark Drela 
C 
C    This program is free software; you can redistribute it and/or modify
C    it under the terms of the GNU General Public License as published by
C    the Free Software Foundation; either version 2 of the License, or
C    (at your option) any later version.
C
C    This program is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU General Public License for more details.
C
C    You should have received a copy of the GNU General Public License
C    along with this program; if not, write to the Free Software
C    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C***********************************************************************
C
C       PROGRAM XFOIL
C C
C       USE XMOD
C       CHARACTER*4 COMAND
C       CHARACTER*128 COMARG
C C
C       DIMENSION IINPUT(20)
C       DIMENSION RINPUT(20)
C       LOGICAL ERROR
C C
C C---- max panel angle threshold for warning
C       DATA ANGTOL / 40.0 /
C C
C       VERSION = 6.99
C       WRITE(*,1005) VERSION
C  1005 FORMAT(
C      &  /' ==================================================='
C      &  /'  XFOIL Version', F5.2
C      &  /'  Copyright (C) 2000   Mark Drela, Harold Youngren'
C      & //'  This software comes with ABSOLUTELY NO WARRANTY,' 
C      &  /'    subject to the GNU General Public License.'
C      & //'  Caveat computor'
C      &  /' ===================================================')
C C
C       CALL INIT
C C
C       FNAME = ' '
C C
C       WRITE(*,1100) XCMREF,YCMREF,NPAN
C  1100 FORMAT(
C      &  /'   QUIT    Exit program'
C      & //'  .OPER    Direct operating point(s)'
C      &  /'  .MDES    Complex mapping design routine'
C      &  /'  .QDES    Surface speed design routine'
C      &  /'  .GDES    Geometry design routine'
C      & //'   SAVE f  Write airfoil to labeled coordinate file'
C      &  /'   PSAV f  Write airfoil to plain coordinate file'
C      &  /'   ISAV f  Write airfoil to ISES coordinate file'
C      &  /'   REVE    Reverse written-airfoil node ordering'
C      & //'   LOAD f  Read buffer airfoil from coordinate file'
C      &  /'   NACA i  Set NACA 4,5-digit airfoil and buffer airfoil'
C      &  /'   INTE    Set buffer airfoil by interpolating two airfoils'
C      &  /'   NORM    Buffer airfoil normalization toggle'
C      &  /'   XYCM rr Change CM reference location, currently ',2F8.5
C      & //'   BEND    Display structural properties of current airfoil' 
C      & //'   PCOP    Set current-airfoil panel nodes directly',
C      &                ' from buffer airfoil points'
C      &  /'   PANE    Set current-airfoil panel nodes (',I4,' )',
C      &                ' based on curvature'
C      &  /'  .PPAR    Change paneling'
C      &  /'   NAME s  Specify new airfoil name'
C      &  /'   NINC    Increment name version number')
C C
C C---- start of menu loop
C   500 CONTINUE
C       CALL ASKC(' XFOIL^',COMAND,COMARG)
C C
C C---- get command line numeric arguments, if any
C       DO I=1, 20
C         IINPUT(I) = 0
C         RINPUT(I) = 0.0
C       ENDDO
C       NINPUT = 0
C       CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
C       NINPUT = 0
C       CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C C
C C===============================================
C       IF(COMAND.EQ.'    ') THEN
C        GO TO 500
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'?   ') THEN
C        WRITE(*,1100) XCMREF, YCMREF, NPAN
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'QUIT') THEN
C        STOP
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'OPER') THEN
C        CALL OPER
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'MDES') THEN
C        CALL MDES
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'QDES') THEN
C        CALL QDES
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'GDES') THEN
C        CALL GDES
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'RSAV') THEN
C        CALL RSAVE(COMARG)
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'SAVE') THEN
C        CALL SAVE(1,COMARG)
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'PSAV') THEN
C        CALL SAVE(0,COMARG)
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'USAV') THEN
C        CALL SAVE(-1,COMARG)
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'ISAV') THEN
C        CALL SAVE(2,COMARG)
C
C===============================================
      SUBROUTINE X_REVE
C       Reverse written-airfoil node ordering
       USE XMOD
C
       LCLOCK = .NOT.LCLOCK
      END SUBROUTINE X_REVE
C
C===============================================
      SUBROUTINE X_LOAD(FILNAM, IEL)
C       Read buffer airfoil from coordinate file
       USE XMOD
       CHARACTER*(*), INTENT(IN   ) :: FILNAM
       INTEGER, INTENT(IN   ) :: IEL
C
       CALL LOAD(FILNAM,ITYPE,IEL)
       IF(ITYPE.GT.0 .AND. NB.GT.0) THEN
        CALL ABCOPY
       ENDIF
      END SUBROUTINE X_LOAD
C
C===============================================
      SUBROUTINE X_NORM
C       Buffer airfoil normalization toggle
       USE XMOD
       LNORM = .NOT.LNORM
       IF(LNORM) THEN
        WRITE(*,*) 'Loaded airfoil will  be normalized'
       ELSE
        WRITE(*,*) "Loaded airfoil won't be normalized"
       ENDIF
      END SUBROUTINE X_NORM
C
C==========================================
      SUBROUTINE X_XYCM(XCMREF2, YCMREF2)
C       Change CM reference location
       USE XMOD
       REAL, INTENT(IN   ) :: XCMREF2, YCMREF2
C
       XCMREF = XCMREF2
       YCMREF = YCMREF2
      END SUBROUTINE X_XYCM
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'BEND') THEN
C         IF(N.EQ.0) THEN
C          WRITE(*,*)
C          WRITE(*,*) ' No airfoil available'
C          GO TO 500
C         ENDIF
C C
C         CALL IJSECT(N,X,Y,
C      &    AREA, SLEN, 
C      &    XC, XMIN, XMAX,
C      &    YC, YMIN, YMAX,
C      &    AIXX, AIXXT,
C      &    AIYY, AIYYT,
C      &    AJ  , AJT   )
C C
C       WRITE(*,*) 
C       WRITE(*,*) 'Area =', AREA
C       WRITE(*,*) 'Slen =', SLEN
C       WRITE(*,*)
C       WRITE(*,*) 'X-bending parameters:'
C       WRITE(*,*) '  centroid   Xc  =', XC
C       WRITE(*,*) '       max X-Xc  =', XMAX-XC
C       WRITE(*,*) '       min X-Xc  =', XMIN-XC
C       WRITE(*,*) '     solid Iyy   =', AIYY
C       WRITE(*,*) '     skin  Iyy/t =', AIYYT
C       XBAR = MAX( ABS(XMAX-XC) , ABS(XMIN-XC) )
C       WRITE(*,*) ' solid Iyy/(X-Xc)=', AIYY /XBAR
C       WRITE(*,*) ' skin Iyy/t(X-Xc)=', AIYYT/XBAR
C C
C       WRITE(*,*)
C       WRITE(*,*) 'Y-bending parameters:'
C       WRITE(*,*) '  centroid   Yc  =', YC
C       WRITE(*,*) '       max Y-Yc  =', YMAX-YC
C       WRITE(*,*) '       min Y-Yc  =', YMIN-YC
C       WRITE(*,*) '     solid Ixx   =', AIXX
C       WRITE(*,*) '     skin  Ixx/t =', AIXXT
C       YBAR = MAX( ABS(YMAX-YC) , ABS(YMIN-YC) )
C       WRITE(*,*) ' solid Ixx/(Y-Yc)=', AIXX /YBAR
C       WRITE(*,*) ' skin Ixx/t(Y-Yc)=', AIXXT/YBAR
C C
C c      WRITE(*,*)
C c      WRITE(*,*) '  power-avg X-Xc =', XEXINT
C c      WRITE(*,*) '  power-avg Y-Yc =', YEXINT
C C
C       WRITE(*,*)
C       WRITE(*,*) '    solid J     =', AJ
C       WRITE(*,*) '    skin  J/t   =', AJT
C
C===============================================
      SUBROUTINE X_PCOP
C       Set current-airfoil panel nodes directly from buffer airfoil points
       CALL ABCOPY
      END SUBROUTINE X_PCOP
C
C===============================================
      SUBROUTINE X_PANE
C       Set current-airfoil panel nodes based on curvature
       CALL PANGEN
      END SUBROUTINE X_PANE
C
C===============================================
C       ELSEIF(COMAND.EQ.'PPAR') THEN
C        CALL GETPAN
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'NAME') THEN
C        IF(COMARG.EQ.' ') THEN
C         CALL NAMMOD(NAME,0,-1)
C        ELSE
C         NAME = COMARG
C        ENDIF
CCC        CALL STRIP(NAME,NNAME)
C        NAME = ADJUSTL(NAME)
C        NNAME = LEN_TRIM(NAME)
C C
C C===============================================
C       ELSEIF(COMAND.EQ.'NINC') THEN
C        CALL NAMMOD(NAME,1,1)
CCC        CALL STRIP(NAME,NNAME)
C        NAME = ADJUSTL(NAME)
C        NNAME = LEN_TRIM(NAME)
C C
C C===============================================
C       ELSE
C        WRITE(*,1050) COMAND
C  1050  FORMAT(1X,A4,' command not recognized.  Type a "?" for list')
C C
C       ENDIF
C C
C C===============================================
C       GO TO 500
C       END XFOIL

 
      SUBROUTINE X_INIT
C---------------------------------------------------
C     Variable initialization/default routine.
C     See file XFOIL.INC for variable description.
C---------------------------------------------------
      USE XMOD
C
      VERSION = 6.99
C
      PI = 4.0*ATAN(1.0)
      HOPI = 0.50/PI
      QOPI = 0.25/PI
      DTOR = PI/180.0
C
C---- default Cp/Cv (air)
      GAMMA = 1.4
      GAMM1 = GAMMA - 1.0
C
C---- set unity freestream speed
      QINF = 1.0
C
C---- initialize freestream Mach number to zero
      MATYP = 1
      MINF1 = 0.
C
      ALFA = 0.0
      COSA = 1.0
      SINA = 0.0
C
      DO 10 I=1, IQX
        GAMU(I,1) = 0.
        GAMU(I,2) = 0.
        GAM(I) = 0.
        GAM_A(I) = 0.
   10 CONTINUE
      PSIO = 0.
C
      CL = 0.
      CM = 0.
      CD = 0.
C
      SIGTE = 0.0
      GAMTE = 0.0
C
      DO 20 I=1, IZX
        SIG(I) = 0.
   20 CONTINUE
C
      NQSP = 0
      DO 30 K=1, IPX
        ALQSP(K) = 0.
        CLQSP(K) = 0.
        CMQSP(K) = 0.
        DO 302 I=1, IBX
          QSPEC(I,K) = 0.
 302    CONTINUE
 30   CONTINUE
C
      AWAKE = 0.0
      AVISC = 0.0
C
      LGAMU  = .FALSE.
      LVISC  = .FALSE.
      LWAKE  = .FALSE.
      LBLINI = .FALSE.
      LIPAN  = .FALSE.
      LQAIJ  = .FALSE.
      LADIJ  = .FALSE.
      LWDIJ  = .FALSE.
      LCPXX  = .FALSE.
      LQSPEC = .FALSE.
      LVCONV = .FALSE.
      LBFLAP = .FALSE.
      LFLAP  = .FALSE.
      LEIW   = .FALSE.
      LSCINI = .FALSE.
C
      LGSAME = .FALSE.
C
      LGPARM = .TRUE.
C
C---- input airfoil will not be normalized
      LNORM = .FALSE.
C
C---- airfoil will not be forced symmetric
      LQSYM = .FALSE.
      LGSYM = .FALSE.
C
C---- endpoint slopes will be matched
      LQSLOP = .TRUE.
      LGSLOP = .TRUE.
      LCSLOP = .TRUE.
C
C---- buffer and current airfoil flap hinge coordinates
      XBF = 0.0
      YBF = 0.0
      XOF = 0.0
      YOF = 0.0
C                                               n
C---- circle plane array size (257, or largest 2  + 1 that will fit array size)
      ANN = LOG(FLOAT((2*IQX)-1))/LOG(2.0)
      NN = INT( ANN + 0.00001 )
      NC1 = 2**NN + 1
      NC1 = MIN( NC1 , 257 )
C
C---- default paneling parameters
      NPAN = 160
      CVPAR = 1.0
      CTERAT = 0.15
      CTRRAT = 0.2
C
C---- default paneling refinement zone x/c endpoints
      XSREF1 = 1.0
      XSREF2 = 1.0
      XPREF1 = 1.0
      XPREF2 = 1.0
C
C---- default Cm reference location
      XCMREF = 0.25
      YCMREF = 0.
C
C---- default viscous parameters
      RETYP = 1
      REINF1 = 0.
      ACRIT = 9.0
      XSTRIP(1) = 1.0
      XSTRIP(2) = 1.0
      XOCTR(1) = 1.0
      XOCTR(2) = 1.0
      WAKLEN = 1.0
C
C---- set BL calibration parameters
      CALL BLPINI
C
C---- Newton iteration limit
      ITMAX = 10
C
C---- drop tolerance for BL system solver
      VACCEL = 0.01
C
C---- default filename prefix
      PREFIX = ' '
C
      NNAME  = 32
      NAME   = '                                '
CCC             12345678901234567890123456789012
C
C---- MSES domain parameters (not used in XFOIL)
      ISPARS = ' -2.0  3.0  -2.5  3.5'
C
C---- set MINF, REINF, based on current CL-dependence
      CALL MRCL(1.0,MINF_CL,REINF_CL)
C
C---- set various compressibility parameters from MINF
      CALL COMSET
C
      RETURN
      END SUBROUTINE X_INIT


      SUBROUTINE MRCL(CLS,M_CLS,R_CLS)
C-------------------------------------------
C     Sets actual Mach, Reynolds numbers
C     from unit-CL values and specified CLS
C     depending on MATYP,RETYP flags.
C-------------------------------------------
      USE XMOD
      REAL M_CLS
C
      CLA = MAX( CLS , 0.000001 )
C
      IF(RETYP.LT.1 .OR. RETYP.GT.3) THEN
C         WRITE(*,*) 'MRCL:  Illegal Re(CL) dependence trigger.'
C         WRITE(*,*) '       Setting fixed Re.'
        RETYP = 1
      ENDIF
      IF(MATYP.LT.1 .OR. MATYP.GT.3) THEN
C         WRITE(*,*) 'MRCL:  Illegal Mach(CL) dependence trigger.'
C         WRITE(*,*) '       Setting fixed Mach.'
        MATYP = 1
      ENDIF
C
      IF(MATYP.EQ.1) THEN
        MINF  = MINF1
        M_CLS = 0.
      ELSE IF(MATYP.EQ.2) THEN
        MINF  =  MINF1/SQRT(CLA)
        M_CLS = -0.5*MINF/CLA
      ELSE IF(MATYP.EQ.3) THEN
        MINF  = MINF1
        M_CLS = 0.
      ENDIF
C
      IF(RETYP.EQ.1) THEN
        REINF = REINF1
        R_CLS = 0.
      ELSE IF(RETYP.EQ.2) THEN
        REINF =  REINF1/SQRT(CLA)
        R_CLS = -0.5*REINF/CLA
      ELSE IF(RETYP.EQ.3) THEN
        REINF =  REINF1/CLA
        R_CLS = -REINF /CLA
      ENDIF
C
C      WRITE(*,*) 'MRCL: MATYP, MINF,  M_CLS = ', MATYP, MINF, M_CLS
C      WRITE(*,*) 'MRCL: RETYP, REINF, R_CLS = ', RETYP, REINF, R_CLS
C
C
      IF(MINF .GE. 0.99) THEN
C         WRITE(*,*)
C         WRITE(*,*) 'MRCL: CL too low for chosen Mach(CL) dependence'
C         WRITE(*,*) '      Aritificially limiting Mach to  0.99'
        MINF = 0.99
        M_CLS = 0.
      ENDIF
C
      RRAT = 1.0
      IF(REINF1 .GT. 0.0) RRAT = REINF/REINF1
C
      IF(RRAT .GT. 100.0) THEN
C         WRITE(*,*)
C         WRITE(*,*) 'MRCL: CL too low for chosen Re(CL) dependence'
C         WRITE(*,*) '      Aritificially limiting Re to ',REINF1*100.0
        REINF = REINF1*100.0
        R_CLS = 0.
      ENDIF
C
      RETURN
      END SUBROUTINE MRCL


      SUBROUTINE COMSET
      USE XMOD
C
C---- set Karman-Tsien parameter TKLAM
      BETA = SQRT(1.0 - MINF**2)
      BETA_MSQ = -0.5/BETA
C
      TKLAM   = MINF**2 / (1.0 + BETA)**2
      TKL_MSQ =     1.0 / (1.0 + BETA)**2
     &    - 2.0*TKLAM/ (1.0 + BETA) * BETA_MSQ
C
C---- set sonic Pressure coefficient and speed
      IF(MINF.EQ.0.0) THEN
       CPSTAR = -999.0
       QSTAR = 999.0
      ELSE
       CPSTAR = 2.0 / (GAMMA*MINF**2)
     &        * (( (1.0 + 0.5*GAMM1*MINF**2)
     &            /(1.0 + 0.5*GAMM1        ))**(GAMMA/GAMM1) - 1.0)
       QSTAR = QINF/MINF
     &       * SQRT( (1.0 + 0.5*GAMM1*MINF**2)
     &              /(1.0 + 0.5*GAMM1        ) )
      ENDIF
C
      RETURN
      END SUBROUTINE COMSET


      SUBROUTINE CPCALC(N,Q,QINF,MINF,CP)
C---------------------------------------------
C     Sets compressible Cp from speed.
C---------------------------------------------
      DIMENSION Q(N),CP(N)
      REAL MINF
C
      LOGICAL DENNEG
C
      BETA = SQRT(1.0 - MINF**2)
      BFAC = 0.5*MINF**2 / (1.0 + BETA)
C
      DENNEG = .FALSE.
C
      DO 20 I=1, N
        CPINC = 1.0 - (Q(I)/QINF)**2
        DEN = BETA + BFAC*CPINC
        CP(I) = CPINC / DEN
        IF(DEN .LE. 0.0) DENNEG = .TRUE.
  20  CONTINUE
C
C       IF(DENNEG) THEN
C        WRITE(*,*)
C        WRITE(*,*) 'CPCALC: Local speed too large. ',
C      &            'Compressibility corrections invalid.'
C       ENDIF
C
      RETURN
      END SUBROUTINE CPCALC

 
      SUBROUTINE CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, 
     &                  XREF,YREF,
     &                  CL,CM,CDP, CL_ALF,CL_MSQ)
C-----------------------------------------------------------
C     Integrates surface pressures to get CL and CM.
C     Integrates skin friction to get CDF.
C     Calculates dCL/dAlpha for prescribed-CL routines.
C-----------------------------------------------------------
      DIMENSION X(N),Y(N), GAM(N), GAM_A(N)
      REAL MINF
C
ccC---- moment-reference coordinates
cc      XREF = 0.25
cc      YREF = 0.
C
      SA = SIN(ALFA)
      CA = COS(ALFA)
C
      BETA = SQRT(1.0 - MINF**2)
      BETA_MSQ = -0.5/BETA
C
      BFAC     = 0.5*MINF**2 / (1.0 + BETA)
      BFAC_MSQ = 0.5         / (1.0 + BETA)
     &         - BFAC        / (1.0 + BETA) * BETA_MSQ
C
      CL = 0.0
      CM = 0.0

      CDP = 0.0
C
      CL_ALF = 0.
      CL_MSQ = 0.
C
      I = 1
      CGINC = 1.0 - (GAM(I)/QINF)**2
      CPG1     = CGINC/(BETA + BFAC*CGINC)
      CPG1_MSQ = -CPG1/(BETA + BFAC*CGINC)*(BETA_MSQ + BFAC_MSQ*CGINC)
C
      CPI_GAM = -2.0*GAM(I)/QINF**2
      CPC_CPI = (1.0 - BFAC*CPG1)/ (BETA + BFAC*CGINC)
      CPG1_ALF = CPC_CPI*CPI_GAM*GAM_A(I)
C
C      WRITE(*,'(7A9)') 'CGINC', 'CPG1', 'CPG2', 'DG', 'DX', 'DY', 'AG'
C
      DO 10 I=1, N
        IP = I+1
        IF(I.EQ.N) IP = 1
C
        CGINC = 1.0 - (GAM(IP)/QINF)**2
        CPG2     = CGINC/(BETA + BFAC*CGINC)
        CPG2_MSQ = -CPG2/(BETA + BFAC*CGINC)*(BETA_MSQ + BFAC_MSQ*CGINC)
C
        CPI_GAM = -2.0*GAM(IP)/QINF**2
        CPC_CPI = (1.0 - BFAC*CPG2)/ (BETA + BFAC*CGINC)
        CPG2_ALF = CPC_CPI*CPI_GAM*GAM_A(IP)
C
        DX = (X(IP) - X(I))*CA + (Y(IP) - Y(I))*SA
        DY = (Y(IP) - Y(I))*CA - (X(IP) - X(I))*SA
        DG = CPG2 - CPG1
C
        AX = (0.5*(X(IP)+X(I))-XREF)*CA + (0.5*(Y(IP)+Y(I))-YREF)*SA
        AY = (0.5*(Y(IP)+Y(I))-YREF)*CA - (0.5*(X(IP)+X(I))-XREF)*SA
        AG = 0.5*(CPG2 + CPG1)
C
        DX_ALF = -(X(IP) - X(I))*SA + (Y(IP) - Y(I))*CA
        AG_ALF = 0.5*(CPG2_ALF + CPG1_ALF)
        AG_MSQ = 0.5*(CPG2_MSQ + CPG1_MSQ)
C
C        WRITE(*,'(7F9.4)') CGINC, CPG1, CPG2, DG, DX, DY, AG
C
        CL     = CL     + DX* AG
        CDP    = CDP    - DY* AG
        CM     = CM     - DX*(AG*AX + DG*DX/12.0)
     &                  - DY*(AG*AY + DG*DY/12.0)
C
        CL_ALF = CL_ALF + DX*AG_ALF + AG*DX_ALF
        CL_MSQ = CL_MSQ + DX*AG_MSQ
C
        CPG1 = CPG2
        CPG1_ALF = CPG2_ALF
        CPG1_MSQ = CPG2_MSQ
   10 CONTINUE
C
      RETURN
      END SUBROUTINE CLCALC



      SUBROUTINE CDCALC
      USE XMOD
C
      SA = SIN(ALFA)
      CA = COS(ALFA)
C
      IF(LVISC .AND. LBLINI) THEN
C
C----- set variables at the end of the wake
       THWAKE = THET(NBL(2),2)
       URAT   = UEDG(NBL(2),2)/QINF
       UEWAKE = UEDG(NBL(2),2) * (1.0-TKLAM) / (1.0 - TKLAM*URAT**2)
       SHWAKE = DSTR(NBL(2),2)/THET(NBL(2),2)
C
C----- extrapolate wake to downstream infinity using Squire-Young relation
C      (reduces errors of the wake not being long enough)
       CD = 2.0*THWAKE * (UEWAKE/QINF)**(0.5*(5.0+SHWAKE))
      ELSE
       CD = 0.0
      ENDIF
C
C---- calculate friction drag coefficient
      CDF = 0.0
      DO 20 IS=1, 2
        DO 205 IBL=3, IBLTE(IS)
          I  = IPAN(IBL  ,IS)
          IM = IPAN(IBL-1,IS)
          DX = (X(I) - X(IM))*CA + (Y(I) - Y(IM))*SA
          CDF = CDF + 0.5*(TAU(IBL,IS)+TAU(IBL-1,IS))*DX * 2.0/QINF**2
 205    CONTINUE
 20   CONTINUE
C
      RETURN
      END SUBROUTINE CDCALC
C
C
C
      SUBROUTINE LOAD(FILNAM,ITYPE,IEL)
C------------------------------------------------------
C     Reads airfoil file into buffer airfoil
C     and does various initial processesing on it.
C------------------------------------------------------
      USE XMOD
      CHARACTER*(*) FILNAM
C
      FNAME = FILNAM
C      IF(FNAME(1:1) .EQ. ' ') CALL ASKS('Enter filename^',FNAME)
C
      LU = 9
      CALL AREAD(LU,FNAME,IBX,XB,YB,NB,NAME,ISPARS,ITYPE,0,IEL)
      IF(ITYPE.EQ.0) RETURN
C
C      IF(ITYPE.EQ.1) CALL ASKS('Enter airfoil name^',NAME)
      NAME = 'DEFAULT'
      NNAME = 7
C
C---- set default prefix for other filenames
      KDOT = INDEX(FNAME,'.')
      IF(KDOT.EQ.0) THEN
       PREFIX = FNAME
      ELSE
       PREFIX = FNAME(1:KDOT-1)
      ENDIF
CCC      CALL STRIP(PREFIX,NPREFIX)
      PREFIX = ADJUSTL(PREFIX)
      NPREFIX = LEN_TRIM(PREFIX)
C
C---- calculate airfoil area assuming counterclockwise ordering
      AREA = 0.0
      DO 50 I=1, NB
        IP = I+1
        IF(I.EQ.NB) IP = 1
        AREA = AREA + 0.5*(YB(I)+YB(IP))*(XB(I)-XB(IP))
   50 CONTINUE
C
      IF(AREA.GE.0.0) THEN
       LCLOCK = .FALSE.
C       WRITE(*,1010) NB
      ELSE
C----- if area is negative (clockwise order), reverse coordinate order
       LCLOCK = .TRUE.
C       WRITE(*,1011) NB
       DO 55 I=1, NB/2
         XTMP = XB(NB-I+1)
         YTMP = YB(NB-I+1)
         XB(NB-I+1) = XB(I)
         YB(NB-I+1) = YB(I)
         XB(I) = XTMP
         YB(I) = YTMP
   55  CONTINUE
      ENDIF
C
      IF(LNORM) THEN
       CALL NORM(XB,XBP,YB,YBP,SB,NB)
C       WRITE(*,1020)
      ENDIF
C
      CALL SCALC(XB,YB,SB,NB)
      CALL SEGSPL(XB,XBP,SB,NB)
      CALL SEGSPL(YB,YBP,SB,NB)
C
      CALL GEOPAR(XB,XBP,YB,YBP,SB,NB, W1,
     &            SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
     &            EI11BA,EI22BA,APX1BA,APX2BA,
     &            EI11BT,EI22BT,APX1BT,APX2BT,
     &            THICKB,CAMBRB )
C
      XBLE = SEVAL(SBLE,XB,XBP,SB,NB)
      YBLE = SEVAL(SBLE,YB,YBP,SB,NB)
      XBTE = 0.5*(XB(1) + XB(NB))
      YBTE = 0.5*(YB(1) + YB(NB))
C
C       WRITE(*,1050) XBLE,YBLE, CHORDB,
C      &              XBTE,YBTE
C
C---- set reasonable MSES domain parameters for non-MSES coordinate file
      IF(ITYPE.LE.2) THEN
        XBLE = SEVAL(SBLE,XB,XBP,SB,NB)
        YBLE = SEVAL(SBLE,YB,YBP,SB,NB)
        XINL = XBLE - 2.0*CHORDB
        XOUT = XBLE + 3.0*CHORDB
        YBOT = YBLE - 2.5*CHORDB
        YTOP = YBLE + 3.5*CHORDB
        XINL = AINT(20.0*ABS(XINL/CHORDB)+0.5)/20.0 * SIGN(CHORDB,XINL)
        XOUT = AINT(20.0*ABS(XOUT/CHORDB)+0.5)/20.0 * SIGN(CHORDB,XOUT)
        YBOT = AINT(20.0*ABS(YBOT/CHORDB)+0.5)/20.0 * SIGN(CHORDB,YBOT)
        YTOP = AINT(20.0*ABS(YTOP/CHORDB)+0.5)/20.0 * SIGN(CHORDB,YTOP)
        WRITE(ISPARS,1005) XINL, XOUT, YBOT, YTOP
 1005   FORMAT(1X, 4F8.2 )
      ENDIF
C
C---- wipe out old flap hinge location
      XBF = 0.0
      YBF = 0.0
      LBFLAP = .FALSE.
C
C---- wipe out off-design alphas, CLs
cc      NALOFF = 0
cc      NCLOFF = 0
C
      RETURN
C...............................................................
C  1010 FORMAT(/' Number of input coordinate points:', I4
C      &       /' Counterclockwise ordering')
C  1011 FORMAT(/' Number of input coordinate points:', I4
C      &       /' Clockwise ordering')
C 1020 FORMAT(/' Airfoil has been normalized')
C 1050 FORMAT(/'  LE  x,y  =', 2F10.5,'  |   Chord =',F10.5
C     &       /'  TE  x,y  =', 2F10.5,'  |'                 )
      END SUBROUTINE LOAD


 
      SUBROUTINE RSAVE(FNAME1)
C-------------------------------------------------------------
C     Writes out current airfoil as a Rhino Macro command file
C     that interpolates curve through airfoil points 
C-------------------------------------------------------------
      USE XMOD
      CHARACTER*(*) FNAME1, STR1*50, STR2*50
C
      CHARACTER*1 ANS
C
      FNAME = FNAME1
C
      LU = 2
      OPEN(LU,FILE=FNAME,STATUS='REPLACE')
      REWIND(LU)
C
C----- write name to first line
       WRITE(LU,1000) ';'//NAME(1:NNAME)
C----- write command to interpolate curve points
       WRITE(LU,1000) 'INTERPCRV'
C
      IF(LCLOCK) THEN
C----- write out in clockwise order (reversed from internal XFOIL order)
       IBEG = N
       IEND = 1
       IENDP = 2
       INCR = -1
      ELSE
C----- write out in counterclockwise order (same as internal XFOIL order)
       IBEG = 1
       IEND = N
       IENDP = N-1
       INCR = 1
      ENDIF
      DO I=IBEG, IENDP, INCR
        WRITE(STR1,1100) X(I),Y(I)
        CALL TRIMBLANKS(STR1,STR2)
        WRITE(LU,1000) TRIM(STR2)
      ENDDO
C
      TGAP = SQRT((X(N)-X(1))**2 + (Y(N)-Y(1))**2)
C----- if TE is closed add command to NOT spline around TE
      IF(TGAP.EQ.0.0)  WRITE(LU,1000) 'S'
C
      WRITE(STR1,1100) X(IEND),Y(IEND)
      CALL TRIMBLANKS(STR1,STR2)
      WRITE(LU,1000) TRIM(STR2)
      WRITE(LU,1000) 'ENTER'
      CLOSE(LU)
      RETURN
C
 90   WRITE(*,*) 'Bad filename.'
      WRITE(*,*) 'Current airfoil not saved.'
      RETURN
C
 1000 FORMAT(A)
 1100 FORMAT(F12.7,',',F12.7)
      END SUBROUTINE RSAVE
 

      subroutine trimblanks(instr,outstr)
C---remove blanks (spaces) from string
      character ch*1, instr*(*), outstr*(*)
      outstr = ' '
      do j=1, len_trim(instr)
C---assemble output string from non-blanks
       ch = instr(j:j)
       if (ch .ne. " ") then
         outstr = trim(outstr) // ch
       endif
      end do
      return
      end SUBROUTINE trimblanks


      SUBROUTINE SAVE(IFTYP,FNAME1)
C--------------------------------
C     Writes out current airfoil 
C--------------------------------
      USE XMOD
      CHARACTER*(*) FNAME1
C
      CHARACTER*1 ANS
C
      FNAME = FNAME1
C
      LU = 2
      OPEN(LU,FILE=FNAME,STATUS='REPLACE')
      REWIND(LU)
C
      IF(IFTYP.GE.1) THEN
C----- write name to first line
       WRITE(LU,1000) NAME(1:NNAME)
      ENDIF
C
      IF(IFTYP.GE.2) THEN
C----- write MSES domain parameters to second line
       DO K=80, 1, -1
         IF(INDEX(ISPARS(K:K),' ') .NE. 1) GO TO 11
       ENDDO
 11    CONTINUE
C
       WRITE(LU,1000) ISPARS(1:K)
      ENDIF
C
      IF(LCLOCK) THEN
C----- write out in clockwise order (reversed from internal XFOIL order)
       IBEG = N
       IEND = 1
       INCR = -1
      ELSE
C----- write out in counterclockwise order (same as internal XFOIL order)
       IBEG = 1
       IEND = N
       INCR = 1
      ENDIF
C
      IF(IFTYP.EQ.-1) THEN
       DO I=IBEG, IEND, INCR
         WRITE(LU,1400) INT(X(I)+SIGN(0.5,X(I))), 
     &                  INT(Y(I)+SIGN(0.5,Y(I)))
       ENDDO
      ELSE
       DO I=IBEG, IEND, INCR
         WRITE(LU,1100) X(I),Y(I)
       ENDDO
      ENDIF
C
      CLOSE(LU)
      RETURN
C
 90   WRITE(*,*) 'Bad filename.'
      WRITE(*,*) 'Current airfoil not saved.'
      RETURN
C
 1000 FORMAT(A)
 1100 FORMAT(1X,2F12.6)
 1400 FORMAT(1X,2I12  )
      END SUBROUTINE SAVE


 
      SUBROUTINE ROTATE(X,Y,N,ALFA)
      DIMENSION X(N), Y(N)
C
      SA = SIN(ALFA)
      CA = COS(ALFA)
CCC      XOFF = 0.25*(1.0-CA)
CCC      YOFF = 0.25*SA
      XOFF = 0.
      YOFF = 0.
      DO 8 I=1, N
        XT = X(I)
        YT = Y(I)
        X(I) = CA*XT + SA*YT + XOFF
        Y(I) = CA*YT - SA*XT + YOFF
    8 CONTINUE
C
      RETURN
      END SUBROUTINE ROTATE


      SUBROUTINE X_NACA(IDES)
C       Set NACA 4,5-digit airfoil and buffer airfoil
      USE XMOD
C
C---- number of points per side
      NSIDE = IQX/3
C
      ITYPE = 0
      IF(IDES.LE.25099) ITYPE = 5
      IF(IDES.LE.9999 ) ITYPE = 4
C
      IF(ITYPE.EQ.0) THEN
       WRITE(*,*) 'This designation not implemented.'
       RETURN
      ENDIF
C
      IF(ITYPE.EQ.4) CALL NACA4(IDES,W1,W2,W3,NSIDE,XB,YB,NB,NAME)
      IF(ITYPE.EQ.5) CALL NACA5(IDES,W1,W2,W3,NSIDE,XB,YB,NB,NAME)
CCC      CALL STRIP(NAME,NNAME)
      NAME = ADJUSTL(NAME)
      NNAME = LEN_TRIM(NAME)
C
C---- see if routines didn't recognize designator
      IF(IDES.EQ.0) RETURN
C
      LCLOCK = .FALSE.
C
      XBF = 0.0
      YBF = 0.0
      LBFLAP = .FALSE.
C
      CALL SCALC(XB,YB,SB,NB)
      CALL SEGSPL(XB,XBP,SB,NB)
      CALL SEGSPL(YB,YBP,SB,NB)
C
      CALL GEOPAR(XB,XBP,YB,YBP,SB,NB, W1,
     &            SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
     &            EI11BA,EI22BA,APX1BA,APX2BA,
     &            EI11BT,EI22BT,APX1BT,APX2BT,
     &            THICKB,CAMBRB )
C
C       WRITE(*,1200) NB
C  1200 FORMAT(/' Buffer airfoil set using', I4,' points')
C
C---- set paneling
      CALL PANGEN
C
      RETURN
      END SUBROUTINE X_NACA


      SUBROUTINE PANGEN
C---------------------------------------------------
C     Set paneling distribution from buffer airfoil
C     geometry, thus creating current airfoil.
C 
C     If REFINE=True, bunch points at x=XSREF on
C     top side and at x=XPREF on bottom side
C     by setting a fictitious local curvature of
C     CTRRAT*(LE curvature) there.
C---------------------------------------------------
      USE XMOD
C
      IF(NB.LT.2) THEN
C       WRITE(*,*) 'PANGEN: Buffer airfoil not available.'
       N = 0
       RETURN
      ENDIF
C
C---- Number of temporary nodes for panel distribution calculation
C       exceeds the specified panel number by factor of IPFAC.
      IPFAC = 3
C
C---- number of airfoil panel points
      N = NPAN
C
C---- set arc length spline parameter
      CALL SCALC(XB,YB,SB,NB)
C
C---- spline raw airfoil coordinates
      CALL SEGSPL(XB,XBP,SB,NB)
      CALL SEGSPL(YB,YBP,SB,NB)
C
C---- normalizing length (~ chord)
      SBREF = 0.5*(SB(NB)-SB(1))
C
C---- set up curvature array
      DO I = 1, NB
        W5(I) = ABS( CURV(SB(I),XB,XBP,YB,YBP,SB,NB) ) * SBREF
      ENDDO
C
C---- locate LE point arc length value and the normalized curvature there
      CALL LEFIND(SBLE,XB,XBP,YB,YBP,SB,NB)
      CVLE = ABS( CURV(SBLE,XB,XBP,YB,YBP,SB,NB) ) * SBREF
C
C---- check for doubled point (sharp corner) at LE
      IBLE = 0
      DO I = 1, NB-1
        IF(SBLE.EQ.SB(I) .AND. SBLE.EQ.SB(I+1)) THEN
         IBLE = I
C          WRITE(*,*)
C          WRITE(*,*) 'Sharp leading edge'
         GO TO 21
        ENDIF
      ENDDO
 21   CONTINUE
C
C---- set LE, TE points
      XBLE = SEVAL(SBLE,XB,XBP,SB,NB)
      YBLE = SEVAL(SBLE,YB,YBP,SB,NB)
      XBTE = 0.5*(XB(1)+XB(NB))
      YBTE = 0.5*(YB(1)+YB(NB))
      CHBSQ = (XBTE-XBLE)**2 + (YBTE-YBLE)**2
C
C---- set average curvature over 2*NK+1 points within Rcurv of LE point
      NK = 3
      CVSUM = 0.
      DO K = -NK, NK
        FRAC = FLOAT(K)/FLOAT(NK)
        SBK = SBLE + FRAC*SBREF/MAX(CVLE,20.0)
        CVK = ABS( CURV(SBK,XB,XBP,YB,YBP,SB,NB) ) * SBREF
        CVSUM = CVSUM + CVK
      ENDDO
      CVAVG = CVSUM/FLOAT(2*NK+1)
C
C---- dummy curvature for sharp LE
      IF(IBLE.NE.0) CVAVG = 10.0
C
C---- set curvature attraction coefficient actually used
      CC = 6.0 * CVPAR
C
C---- set artificial curvature at TE to bunch panels there
      CVTE = CVAVG * CTERAT
      W5(1)  = CVTE
      W5(NB) = CVTE
C
C
C**** smooth curvature array for smoother panel size distribution  ****
C
CCC      CALL ASKR('Enter curvature smoothing length/c^',SMOOL)
CCC      SMOOL = 0.010
C
C---- set smoothing length = 1 / averaged LE curvature, but 
C-    no more than 5% of chord and no less than 1/4 average panel spacing
      SMOOL = MAX( 1.0/MAX(CVAVG,20.0) , 0.25 /FLOAT(NPAN/2) )
C
      SMOOSQ = (SMOOL*SBREF) ** 2
C
C---- set up tri-diagonal system for smoothed curvatures
      W2(1) = 1.0
      W3(1) = 0.0
      DO I=2, NB-1
        DSM = SB(I) - SB(I-1)
        DSP = SB(I+1) - SB(I)
        DSO = 0.5*(SB(I+1) - SB(I-1))
C
        IF(DSM.EQ.0.0 .OR. DSP.EQ.0.0) THEN
C------- leave curvature at corner point unchanged
         W1(I) = 0.0
         W2(I) = 1.0
         W3(I) = 0.0
        ELSE
         W1(I) =  SMOOSQ * (         - 1.0/DSM) / DSO
         W2(I) =  SMOOSQ * ( 1.0/DSP + 1.0/DSM) / DSO  +  1.0
         W3(I) =  SMOOSQ * (-1.0/DSP          ) / DSO
        ENDIF
      ENDDO
C
      W1(NB) = 0.0
      W2(NB) = 1.0
C
C---- fix curvature at LE point by modifying equations adjacent to LE
      DO I=2, NB-1
        IF(SB(I).EQ.SBLE .OR. I.EQ.IBLE .OR. I.EQ.IBLE+1) THEN
C------- if node falls right on LE point, fix curvature there
         W1(I) = 0.
         W2(I) = 1.0
         W3(I) = 0.
         W5(I) = CVLE
        ELSE IF(SB(I-1).LT.SBLE .AND. SB(I).GT.SBLE) THEN
C------- modify equation at node just before LE point
         DSM = SB(I-1) - SB(I-2)
         DSP = SBLE    - SB(I-1)
         DSO = 0.5*(SBLE - SB(I-2))
C
         W1(I-1) =  SMOOSQ * (         - 1.0/DSM) / DSO
         W2(I-1) =  SMOOSQ * ( 1.0/DSP + 1.0/DSM) / DSO  +  1.0
         W3(I-1) =  0.
         W5(I-1) = W5(I-1) + SMOOSQ*CVLE/(DSP*DSO)
C
C------- modify equation at node just after LE point
         DSM = SB(I) - SBLE
         DSP = SB(I+1) - SB(I)
         DSO = 0.5*(SB(I+1) - SBLE)
         W1(I) =  0.
         W2(I) =  SMOOSQ * ( 1.0/DSP + 1.0/DSM) / DSO  +  1.0
         W3(I) =  SMOOSQ * (-1.0/DSP          ) / DSO
         W5(I) = W5(I) + SMOOSQ*CVLE/(DSM*DSO)
C
         GO TO 51
        ENDIF
      ENDDO
   51 CONTINUE
C
C---- set artificial curvature at bunching points and fix it there
      DO I=2, NB-1
C------ chord-based x/c coordinate
        XOC = (  (XB(I)-XBLE)*(XBTE-XBLE)
     &         + (YB(I)-YBLE)*(YBTE-YBLE) ) / CHBSQ
C
        IF(SB(I).LT.SBLE) THEN
C------- check if top side point is in refinement area
         IF(XOC.GT.XSREF1 .AND. XOC.LT.XSREF2) THEN
          W1(I) = 0.
          W2(I) = 1.0
          W3(I) = 0.
          W5(I) = CVLE*CTRRAT
         ENDIF
        ELSE
C------- check if bottom side point is in refinement area
         IF(XOC.GT.XPREF1 .AND. XOC.LT.XPREF2) THEN
          W1(I) = 0.
          W2(I) = 1.0
          W3(I) = 0.
          W5(I) = CVLE*CTRRAT
         ENDIF
        ENDIF
      ENDDO
C
C---- solve for smoothed curvature array W5
      IF(IBLE.EQ.0) THEN
       CALL TRISOL(W2,W1,W3,W5,NB)
      ELSE
       I = 1
       CALL TRISOL(W2(I),W1(I),W3(I),W5(I),IBLE)
       I = IBLE+1
       CALL TRISOL(W2(I),W1(I),W3(I),W5(I),NB-IBLE)
      ENDIF
C
C---- find max curvature
      CVMAX = 0.
      DO I=1, NB
        CVMAX = MAX( CVMAX , ABS(W5(I)) )
      ENDDO
C
C---- normalize curvature array
      DO I=1, NB
        W5(I) = W5(I) / CVMAX
      ENDDO
C
C---- spline curvature array
      CALL SEGSPL(W5,W6,SB,NB)
C
C---- Set initial guess for node positions uniform in s.
C     More nodes than specified (by factor of IPFAC) are 
C     temporarily used  for more reliable convergence.
      NN = IPFAC*(N-1)+1
C
C---- ratio of lengths of panel at TE to one away from the TE
      RDSTE = 0.667
      RTF = (RDSTE-1.0)*FLOAT(IPFAC) + 1.0
C
      IF(IBLE.EQ.0) THEN
C
       DSAVG = (SB(NB)-SB(1))/(FLOAT(NN-3) + 2.0*RTF)
       SNEW(1) = SB(1)
       DO I=2, NN-1
         SNEW(I) = SB(1) + DSAVG * (FLOAT(I-2) + RTF)
       ENDDO
       SNEW(NN) = SB(NB)
C
      ELSE
C
       NFRAC1 = (N * IBLE) / NB
C
       NN1 = IPFAC*(NFRAC1-1)+1
       DSAVG1 = (SBLE-SB(1))/(FLOAT(NN1-2) + RTF)
       SNEW(1) = SB(1)
       DO I=2, NN1
         SNEW(I) = SB(1) + DSAVG1 * (FLOAT(I-2) + RTF)
       ENDDO
C
       NN2 = NN - NN1 + 1
       DSAVG2 = (SB(NB)-SBLE)/(FLOAT(NN2-2) + RTF)
       DO I=2, NN2-1
         SNEW(I-1+NN1) = SBLE + DSAVG2 * (FLOAT(I-2) + RTF)
       ENDDO
       SNEW(NN) = SB(NB)
C
      ENDIF
C
C---- Newton iteration loop for new node positions
      DO 10 ITER=1, 20
C
C------ set up tri-diagonal system for node position deltas
        CV1  = SEVAL(SNEW(1),W5,W6,SB,NB)
        CV2  = SEVAL(SNEW(2),W5,W6,SB,NB)
        CVS1 = DEVAL(SNEW(1),W5,W6,SB,NB)
        CVS2 = DEVAL(SNEW(2),W5,W6,SB,NB)
C
        CAVM = SQRT(CV1**2 + CV2**2)
        IF(CAVM .EQ. 0.0) THEN
          CAVM_S1 = 0.
          CAVM_S2 = 0.
        ELSE
          CAVM_S1 = CVS1 * CV1/CAVM
          CAVM_S2 = CVS2 * CV2/CAVM
        ENDIF
C
        DO 110 I=2, NN-1
          DSM = SNEW(I) - SNEW(I-1)
          DSP = SNEW(I) - SNEW(I+1)
          CV3  = SEVAL(SNEW(I+1),W5,W6,SB,NB)
          CVS3 = DEVAL(SNEW(I+1),W5,W6,SB,NB)
C
          CAVP = SQRT(CV3**2 + CV2**2)
          IF(CAVP .EQ. 0.0) THEN
            CAVP_S2 = 0.
            CAVP_S3 = 0.
          ELSE
            CAVP_S2 = CVS2 * CV2/CAVP
            CAVP_S3 = CVS3 * CV3/CAVP
          ENDIF
C
          FM = CC*CAVM + 1.0
          FP = CC*CAVP + 1.0
C
          REZ = DSP*FP + DSM*FM
C
C-------- lower, main, and upper diagonals
          W1(I) =      -FM  +  CC*               DSM*CAVM_S1
          W2(I) =  FP + FM  +  CC*(DSP*CAVP_S2 + DSM*CAVM_S2)
          W3(I) = -FP       +  CC* DSP*CAVP_S3
C
C-------- residual, requiring that
C         (1 + C*curv)*deltaS is equal on both sides of node i
          W4(I) = -REZ
C
          CV1 = CV2
          CV2 = CV3
          CVS1 = CVS2
          CVS2 = CVS3
          CAVM    = CAVP
          CAVM_S1 = CAVP_S2
          CAVM_S2 = CAVP_S3
  110   CONTINUE
C
C------ fix endpoints (at TE)
        W2(1) = 1.0
        W3(1) = 0.0
        W4(1) = 0.0
        W1(NN) = 0.0
        W2(NN) = 1.0
        W4(NN) = 0.0
C
        IF(RTF .NE. 1.0) THEN
C------- fudge equations adjacent to TE to get TE panel length ratio RTF
C
         I = 2
         W4(I) = -((SNEW(I) - SNEW(I-1)) + RTF*(SNEW(I) - SNEW(I+1)))
         W1(I) = -1.0
         W2(I) =  1.0 + RTF
         W3(I) =      - RTF
C
         I = NN-1
         W4(I) = -((SNEW(I) - SNEW(I+1)) + RTF*(SNEW(I) - SNEW(I-1)))
         W3(I) = -1.0
         W2(I) =  1.0 + RTF
         W1(I) =      - RTF
        ENDIF
C
C
C------ fix sharp LE point
        IF(IBLE.NE.0) THEN
         I = NN1
         W1(I) = 0.0
         W2(I) = 1.0
         W3(I) = 0.0
         W4(I) = SBLE - SNEW(I)
        ENDIF
C
C------ solve for changes W4 in node position arc length values
        CALL TRISOL(W2,W1,W3,W4,NN)
C
C------ find under-relaxation factor to keep nodes from changing order
        RLX = 1.0
        DMAX = 0.0
        DO I=1, NN-1
          DS  = SNEW(I+1) - SNEW(I)
          DDS = W4(I+1) - W4(I)
          DSRAT = 1.0 + RLX*DDS/DS
          IF(DSRAT.GT.4.0) RLX = (4.0-1.0)*DS/DDS
          IF(DSRAT.LT.0.2) RLX = (0.2-1.0)*DS/DDS
          DMAX = MAX(ABS(W4(I)),DMAX)
        ENDDO
C
C------ update node position
        DO I=2, NN-1
          SNEW(I) = SNEW(I) + RLX*W4(I)
        ENDDO
C
CCC        IF(RLX.EQ.1.0) WRITE(*,*) DMAX
CCC        IF(RLX.NE.1.0) WRITE(*,*) DMAX,'    RLX =',RLX
        IF(ABS(DMAX).LT.1.E-3) GO TO 11
   10 CONTINUE
C      WRITE(*,*) 'Paneling convergence failed.  Continuing anyway...'
C
   11 CONTINUE
C
C---- set new panel node coordinates
      DO I=1, N
        IND = IPFAC*(I-1) + 1
        S(I) = SNEW(IND)
        X(I) = SEVAL(SNEW(IND),XB,XBP,SB,NB)
        Y(I) = SEVAL(SNEW(IND),YB,YBP,SB,NB)
      ENDDO
C
C
C---- go over buffer airfoil again, checking for corners (double points)
      NCORN = 0
      DO 25 IB=1, NB-1
        IF(SB(IB) .EQ. SB(IB+1)) THEN
C------- found one !
C
         NCORN = NCORN+1
         XBCORN = XB(IB)
         YBCORN = YB(IB)
         SBCORN = SB(IB)
C
C------- find current-airfoil panel which contains corner
         DO 252 I=1, N
C
C--------- keep stepping until first node past corner
           IF(S(I) .LE. SBCORN) GO TO 252
C
C---------- move remainder of panel nodes to make room for additional node
            DO 2522 J=N, I, -1
              X(J+1) = X(J)
              Y(J+1) = Y(J)
              S(J+1) = S(J)
 2522       CONTINUE
            N = N+1
C
            IF(N .GT. IQX-1)
     &       STOP 'PANEL: Too many panels. Increase IQX in XFOIL.INC'
C
            X(I) = XBCORN
            Y(I) = YBCORN
            S(I) = SBCORN
C
C---------- shift nodes adjacent to corner to keep panel sizes comparable
            IF(I-2 .GE. 1) THEN
             S(I-1) = 0.5*(S(I) + S(I-2))
             X(I-1) = SEVAL(S(I-1),XB,XBP,SB,NB)
             Y(I-1) = SEVAL(S(I-1),YB,YBP,SB,NB)
            ENDIF
C
            IF(I+2 .LE. N) THEN
             S(I+1) = 0.5*(S(I) + S(I+2))
             X(I+1) = SEVAL(S(I+1),XB,XBP,SB,NB)
             Y(I+1) = SEVAL(S(I+1),YB,YBP,SB,NB)
            ENDIF
C
C---------- go on to next input geometry point to check for corner
            GO TO 25
C
  252    CONTINUE
        ENDIF
   25 CONTINUE
C
      CALL SCALC(X,Y,S,N)
      CALL SEGSPL(X,XP,S,N)
      CALL SEGSPL(Y,YP,S,N)
      CALL LEFIND(SLE,X,XP,Y,YP,S,N)
C
      XLE = SEVAL(SLE,X,XP,S,N)
      YLE = SEVAL(SLE,Y,YP,S,N)
      XTE = 0.5*(X(1)+X(N))
      YTE = 0.5*(Y(1)+Y(N))
      CHORD  = SQRT( (XTE-XLE)**2 + (YTE-YLE)**2 )
C
C---- calculate panel size ratios (user info)
      DSMIN =  1000.0
      DSMAX = -1000.0
      DO 40 I=1, N-1
        DS = S(I+1)-S(I)
        IF(DS .EQ. 0.0) GO TO 40
          DSMIN = MIN(DSMIN,DS)
          DSMAX = MAX(DSMAX,DS)
   40 CONTINUE
C
      DSMIN = DSMIN*FLOAT(N-1)/S(N)
      DSMAX = DSMAX*FLOAT(N-1)/S(N)
ccc      WRITE(*,*) 'DSmin/DSavg = ',DSMIN,'     DSmax/DSavg = ',DSMAX
C
C---- set various flags for new airfoil
      LGAMU = .FALSE.
      LWAKE = .FALSE.
      LQAIJ = .FALSE.
      LADIJ = .FALSE.
      LWDIJ = .FALSE.
      LIPAN = .FALSE.
      LBLINI = .FALSE.
      LVCONV = .FALSE.
      LSCINI = .FALSE.
      LQSPEC = .FALSE.
      LGSAME = .FALSE.
C
      IF(LBFLAP) THEN
       XOF = XBF
       YOF = YBF
       LFLAP = .TRUE.
      ENDIF
C
C---- determine if TE is blunt or sharp, calculate TE geometry parameters
      CALL TECALC
C
C---- calculate normal vectors
      CALL NCALC(X,Y,S,N,NX,NY)
C
C---- calculate panel angles for panel routines
      CALL APCALC
C
      RETURN
      END SUBROUTINE PANGEN



C       SUBROUTINE GETPAN
C       USE XMOD
C       LOGICAL LCHANGE
C       CHARACTER*4 VAR
C       CHARACTER*128 COMARG
C C
C       DIMENSION IINPUT(20)
C       DIMENSION RINPUT(20)
C       LOGICAL ERROR
C C
C       IF(NB.LE.1) THEN
C        WRITE(*,*) 'GETPAN: Buffer airfoil not available.'
C        RETURN
C       ENDIF
C C
C  5    CONTINUE
C       IF(N.LE.1) THEN
C        WRITE(*,*) 'No current airfoil to plot'
C       ENDIF
C       LCHANGE = .FALSE.
C C
C    10 WRITE(*,1000) NPAN, CVPAR, CTERAT, CTRRAT,
C      &              XSREF1, XSREF2, XPREF1, XPREF2
C  1000 FORMAT(
C      & /'    Present paneling parameters...'
C      & /'  N  i   Number of panel nodes      ' , I4
C      & /'  P  r   Panel bunching parameter   ' , F6.3
C      & /'  T  r   TE/LE panel density ratio  ' , F6.3
C      & /'  R  r   Refined area/LE  panel density ratio  ' , F6.3
C      & /'  XT rr  Top    side refined area x/c limits   ' , 2F6.3
C      & /'  XB rr  Bottom side refined area x/c limits   ' , 2F6.3)
C C
C    12 CALL ASKC('Change what ? (<cr> if nothing else)^',VAR,COMARG)
C C
C       DO I=1, 20
C         IINPUT(I) = 0
C         RINPUT(I) = 0.0
C       ENDDO
C       NINPUT = 0
C       CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
C       NINPUT = 0
C       CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C C
C       IF     (VAR.EQ.'    ') THEN
C C
C         IF(LCHANGE) THEN
C C
C C-------- set new panel distribution, and display max panel corner angle
C           CALL PANGEN
C           IF(N.GT.0) CALL CANG(X,Y,N,1,IMAX,AMAX)
C C
C C-------- go back to paneling menu
C           GO TO 5
C         ENDIF
C C
C         RETURN
C C
C       ELSE IF(VAR.EQ.'N   ' .OR. VAR.EQ.'n   ') THEN
C C
C         IF(NINPUT.GE.1) THEN
C          NPAN = IINPUT(1)
C         ELSE
C          CALL ASKI('Enter number of panel nodes^',NPAN)
C         ENDIF
C         IF(NPAN .GT. IQX-6) THEN
C           NPAN = IQX - 6
C           WRITE(*,1200) NPAN
C  1200     FORMAT(1X,' Number of panel nodes reduced to array limit:',I4)
C         ENDIF
C         LCHANGE = .TRUE.
C C
C       ELSE IF(VAR.EQ.'P   ' .OR. VAR.EQ.'p   ') THEN
C C
C         IF(NINPUT.GE.1) THEN
C          CVPAR = RINPUT(1)
C         ELSE
C          CALL ASKR('Enter panel bunching parameter (0 to ~1)^',CVPAR)
C         ENDIF
C         LCHANGE = .TRUE.
C C
C       ELSE IF(VAR.EQ.'T   ' .OR. VAR.EQ.'t   ') THEN
C C
C         IF(NINPUT.GE.1) THEN
C          CTERAT = RINPUT(1)
C         ELSE
C          CALL ASKR('Enter TE/LE panel density ratio^',CTERAT)
C         ENDIF
C         LCHANGE = .TRUE.
C C
C       ELSE IF(VAR.EQ.'R   ' .OR. VAR.EQ.'r   ') THEN
C C
C         IF(NINPUT.GE.1) THEN
C          CTRRAT = RINPUT(1)
C         ELSE
C          CALL ASKR('Enter refined-area panel density ratio^',CTRRAT)
C         ENDIF
C         LCHANGE = .TRUE.
C C
C       ELSE IF(VAR.EQ.'XT  ' .OR. VAR.EQ.'xt  ') THEN
C C
C         IF(NINPUT.GE.2) THEN
C          XSREF1 = RINPUT(1)
C          XSREF2 = RINPUT(2)
C         ELSE
C          CALL ASKR('Enter left   top   side refinement limit^',XSREF1)
C          CALL ASKR('Enter right  top   side refinement limit^',XSREF2)
C         ENDIF
C         LCHANGE = .TRUE.
C C
C       ELSE IF(VAR.EQ.'XB  ' .OR. VAR.EQ.'xb  ') THEN
C C
C         IF(NINPUT.GE.2) THEN
C          XPREF1 = RINPUT(1)
C          XPREF2 = RINPUT(2)
C         ELSE
C          CALL ASKR('Enter left  bottom side refinement limit^',XPREF1)
C          CALL ASKR('Enter right bottom side refinement limit^',XPREF2)
C         ENDIF
C         LCHANGE = .TRUE.
C C
C       ELSE
C C
C         WRITE(*,*)
C         WRITE(*,*) '***  Input not recognized  ***'
C         GO TO 10
C C
C       ENDIF
C C
C       GO TO 12
C C
C       END ! GETPAN


      SUBROUTINE TECALC
C-------------------------------------------
C     Calculates total and projected TE gap 
C     areas and TE panel strengths.
C-------------------------------------------
      USE XMOD
C
C---- set TE base vector and TE bisector components
      DXTE = X(1) - X(N)
      DYTE = Y(1) - Y(N)
      DXS = 0.5*(-XP(1) + XP(N))
      DYS = 0.5*(-YP(1) + YP(N))
C
C---- normal and streamwise projected TE gap areas
      ANTE = DXS*DYTE - DYS*DXTE
      ASTE = DXS*DXTE + DYS*DYTE
C
C---- total TE gap area
      DSTE = SQRT(DXTE**2 + DYTE**2)
C
      SHARP = DSTE .LT. 0.0001*CHORD
C
      IF(SHARP) THEN
       SCS = 1.0
       SDS = 0.0
      ELSE
       SCS = ANTE/DSTE
       SDS = ASTE/DSTE
      ENDIF
C
C       WRITE(*,'(10A9)') 'DXTE', 'DYTE', 'DXS', 'DYS',
C      &            'ANTE', 'ASTE','DSTE', 'SCS', 'SDS', 'SHARP'
C       WRITE(*,'(9F9.5,L3)') DXTE, DYTE, DXS, DYS, ANTE, ASTE,
C      &           DSTE, SCS, SDS, SHARP
C
C---- TE panel source and vorticity strengths
      SIGTE = 0.5*(GAM(1) - GAM(N))*SCS
      GAMTE = -.5*(GAM(1) - GAM(N))*SDS
C
      RETURN
      END SUBROUTINE TECALC
 


      SUBROUTINE X_INTE(FAIR1, FAIR2, FRAC)
C       Set buffer airfoil by interpolating two airfoils
C-----------------------------------------------------------
C     Interpolates two airfoils into an intermediate shape.
C     Extrapolation is also possible to a reasonable extent.
C-----------------------------------------------------------
      USE XMOD
      CHARACTER*(*) FAIR1
      CHARACTER*(*) FAIR2
      REAL FRAC
      CHARACTER*2 CAIR
      INTEGER NINT(2)
      REAL SINT(IBX,2),  
     &     XINT(IBX,2), XPINT(IBX,2),
     &     YINT(IBX,2), YPINT(IBX,2),
     &     SLEINT(2)
      CHARACTER*20 PROMPTN
      CHARACTER*48 NAMEINT(2)
      CHARACTER*80 ISPARST
      CHARACTER*80 FAIR(2)
C
      LU = 21
      FAIR(1) = FAIR1
      FAIR(2) = FAIR2
C
      DO 40 K = 1, 2
        IF (FAIR(K).EQ.'C' .OR. FAIR(K).EQ.'c') THEN
C        Use current airfoil
         IF(N.LE.1) THEN
C          WRITE(*,*) 'No current airfoil available'
          RETURN
         ENDIF
C
         NINT(K) = N
         DO I = 1, N
           XINT(I,K) = X(I)
           YINT(I,K) = Y(I)
         ENDDO
         NAMEINT(K) = NAME
C
        ELSE
C        Read airfoil from file
         CALL AREAD(LU,FAIR(K),IBX,
     &              XINT(1,K),YINT(1,K),NINT(K),
     &              NAMEINT(K),ISPARST,ITYPE,0,1)
         IF(ITYPE.EQ.0) RETURN

        ENDIF
C
        CALL SCALC(XINT(1,K),YINT(1,K),SINT(1,K),NINT(K))
        CALL SEGSPLD(XINT(1,K),XPINT(1,K),SINT(1,K),NINT(K),-999.,-999.)
        CALL SEGSPLD(YINT(1,K),YPINT(1,K),SINT(1,K),NINT(K),-999.,-999.)
        CALL LEFIND(SLEINT(K),
     &              XINT(1,K),XPINT(1,K),
     &              YINT(1,K),YPINT(1,K),SINT(1,K),NINT(K))
 40   CONTINUE
C
      CALL INTER(XINT(1,1),XPINT(1,1),
     &           YINT(1,1),YPINT(1,1),SINT(1,1),NINT(1),SLEINT(1),
     &           XINT(1,2),XPINT(1,2),
     &           YINT(1,2),YPINT(1,2),SINT(1,2),NINT(2),SLEINT(2),
     &           XB,YB,NB,FRAC)
C
      CALL SCALC(XB,YB,SB,NB)
      CALL SEGSPL(XB,XBP,SB,NB)
      CALL SEGSPL(YB,YBP,SB,NB)
C
      CALL GEOPAR(XB,XBP,YB,YBP,SB,NB, W1,
     &            SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
     &            EI11BA,EI22BA,APX1BA,APX2BA,
     &            EI11BT,EI22BT,APX1BT,APX2BT,
     &            THICKB,CAMBRB )
C
      NAME = 'INTER'
      NNAME = 5
C       WRITE(*,*)
C       WRITE(*,*) 'Result has been placed in buffer airfoil'
C       WRITE(*,*) 'Execute PCOP or PANE to set new current airfoil'
      RETURN
      END SUBROUTINE X_INTE
