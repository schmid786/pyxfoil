C
C====  XFOIL code global INCLUDE file  =====
C
C------ Primary dimensioning limit parameters
C IQX   number of surface panel nodes + 6
C IWX   number of wake panel nodes
C IPX   number of Qspec(s) distributions
C ISX   number of airfoil sides
C
C------ Derived dimensioning limit parameters
C IBX   number of buffer airfoil nodes
C IMX   number of complex mapping coefficients  Cn
C IZX   number of panel nodes (airfoil + wake)
C IVX   number of nodes along BL on one side of airfoil and wake
C NAX   number of points in stored polar
C NPX   number of polars and reference polars
C NFX   number of points in one reference polar
C NTX   number of points in thickness/camber arrays
C
C---- include polar variable indexing parameters
C
      MODULE XMOD
        PARAMETER (IQX=500, IWX=36, IPX=5, ISX=2)
Cc      PARAMETER (IQX=1000, IWX=72, IPX=5, ISX=2)
        PARAMETER (IBX=2*IQX)
        PARAMETER (IZX=IQX+IWX)
        PARAMETER (IVX=IQX/2 + IWX + 50)
        PARAMETER (NAX=800,NPX=12,NFX=128)
        PARAMETER (NTX=2*IBX)
C---- dimension temporary work and storage arrays (EQUIVALENCED below)
        REAL W1(6*IQX),W2(6*IQX),W3(6*IQX),W4(6*IQX),
     &       W5(6*IQX),W6(6*IQX),W7(6*IQX),W8(6*IQX)
        REAL BIJ(IQX,IZX), CIJ(IWX,IQX)
C       /CR01/
        REAL       VERSION
C       /CC01/
        CHARACTER*64 FNAME, PREFIX
        CHARACTER*48 NAME
        CHARACTER*80 ISPARS
C       /QMAT/ 
        REAL       Q(IQX,IQX),DQ(IQX),
     &             DZDG(IQX),DZDN(IQX),DZDM(IZX),
     &             DQDG(IQX),DQDM(IZX),QTAN1,QTAN2,
     &             Z_QINF,Z_ALFA,Z_QDOF0,Z_QDOF1,Z_QDOF2,Z_QDOF3
C       /CR03/ 
        REAL       AIJ(IQX,IQX),DIJ(IZX,IZX)
C       /CR04/ 
        REAL       QINV(IZX),QVIS(IZX),CPI(IZX),CPV(IZX),
     &             QINVU(IZX,2), QINV_A(IZX)
C       /CR05/ 
        REAL       X(IZX),Y(IZX),XP(IZX),YP(IZX),S(IZX),
     &             SLE,XLE,YLE,XTE,YTE,CHORD,
     &             WGAP(IWX),WAKLEN
C       /CR06/ 
        REAL       GAM(IQX),GAMU(IQX,2),GAM_A(IQX),SIG(IZX),
     &             NX(IZX),NY(IZX),APANEL(IZX),
     &             SST,SST_GO,SST_GP,
     &             GAMTE,SIGTE,
     &             DSTE,ANTE,ASTE
C       /CR07/
        REAL       SSPEC(IBX),QGAMM(IBX),
     &             QSPEC(IBX,IPX),QSPECP(IBX,IPX),
     &             ALGAM,CLGAM,CMGAM,
     &             ALQSP(IPX),CLQSP(IPX),CMQSP(IPX),
     &             QF0(IQX),QF1(IQX),QF2(IQX),QF3(IQX),
     &             QDOF0,QDOF1,QDOF2,QDOF3,CLSPEC
C       /CI01/ 
        INTEGER    IQ1,IQ2,NSP,NQSP,KQTARG,IACQSP,NC1,NNAME,NPREFIX
C       /CR09/
        REAL       ADEG,ALFA,AWAKE,MVISC,AVISC,
     &             XCMREF,YCMREF,
     &             CL,CM,CD,CDP,CDF,CL_ALF,CL_MSQ,
     &             PSIO,COSA,SINA,QINF,
     &             GAMMA,GAMM1,
     &             MINF1,MINF,MINF_CL,TKLAM,TKL_MSQ,CPSTAR,QSTAR,
     &             CPMNI,CPMNV,XCPMNI,XCPMNV
C
C       /CR11/ 
        REAL       PI,HOPI,QOPI,DTOR
C       /CR12/
        REAL       CVPAR,CTERAT,CTRRAT,XSREF1,XSREF2,XPREF1,XPREF2
C       /CI04/ 
        INTEGER    N,NB,NW,NPAN,IST,
     &             ITMAX,RETYP,MATYP,AIJPIV(IQX),
     &             NCAM,NCM,NTK
C       /CL01/
        LOGICAL    OK,SHARP,
     &             LGAMU,LVISC,LALFA,LWAKE,
     &             LBLINI,LIPAN,LQAIJ,LADIJ,LWDIJ,LCPXX,
     &             LQSPEC,LVCONV,LCLOCK,
     &             LBFLAP,LFLAP,LEIW,LSCINI,LNORM,LGSAME,LDCPLOT,
     &             LQSYM ,LGSYM, 
     &             LQSLOP,LGSLOP,LCSLOP,LGPARM,
     &             LIQSET
C       /CR14/
        REAL       XB(IBX),YB(IBX),
     &             XBP(IBX),YBP(IBX),SB(IBX),SNEW(4*IBX),
     &             XBF,YBF,XOF,YOF,HMOM,HFX,HFY,
     &             XBMIN,XBMAX,
     &             SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
     &             EI11BA,EI22BA,APX1BA,APX2BA,
     &             EI11BT,EI22BT,APX1BT,APX2BT,
     &             THICKB,CAMBRB,
     &             XCM(2*IBX),YCM(2*IBX),YCMP(2*IBX),
     &             XTK(2*IBX),YTK(2*IBX),YTKP(2*IBX)
C
C       /CR15/
        REAL       XSSI(IVX,ISX),UEDG(IVX,ISX),UINV(IVX,ISX),
     &             MASS(IVX,ISX),THET(IVX,ISX),DSTR(IVX,ISX),
     &             CTAU(IVX,ISX),
     &             TAU(IVX,ISX),DIS(IVX,ISX),CTQ(IVX,ISX),
     &             VTI(IVX,ISX),
     &             REINF1,REINF,REINF_CL,ACRIT,
     &             XSTRIP(ISX),XOCTR(ISX),
     &             UINV_A(IVX,ISX)
C       /CI05/
        INTEGER    IBLTE(ISX),NBL(ISX),IPAN(IVX,ISX),ISYS(IVX,ISX),NSYS,
     &             ITRAN(ISX)
C       /CL02/
        LOGICAL    TFORCE(ISX)
C       /CR17/
        REAL       RMSBL,RMXBL,RLX,VACCEL
C       /CI06/
        INTEGER    IMXBL,ISMXBL
C       /CC03/
        CHARACTER*1 VMXBL
C       /CR19/
        REAL       XCADD(NTX), YCADD(NTX), YCADDP(NTX),
     &             XPADD(NTX), YPADD(NTX), YPADDP(NTX),
     &             XCAM(NTX),
     &             YCAM(NTX), YCAMP(NTX),
     &             PCAM(NTX), PCAMP(NTX)
C       /VMAT/
        REAL       VA(3,2,IZX),VB(3,2,IZX),VDEL(3,2,IZX),
     &             VM(3,IZX,IZX),VZ(3,2)
C       from xbl>UPDATE
        REAL       UNEW(IVX,2), U_AC(IVX,2)
        REAL       QNEW(IQX),   Q_AC(IQX)
        EQUIVALENCE (VA(1,1,1), UNEW(1,1)) ,
     &              (VB(1,1,1), QNEW(1)  )
        EQUIVALENCE (VA(1,1,IVX), U_AC(1,1)) ,
     &              (VB(1,1,IVX), Q_AC(1)  )
C
C---- save storage space
        EQUIVALENCE (Q(1,1 ),W1(1)), (Q(1,7 ),W2(1)),
     &              (Q(1,13),W3(1)), (Q(1,19),W4(1)),
     &              (Q(1,25),W5(1)), (Q(1,31),W6(1)),
     &              (Q(1,37),W7(1)), (Q(1,43),W8(1))
        EQUIVALENCE (VM(1,1,1),BIJ(1,1)), (VM(1,1,IZX/2),CIJ(1,1))
      END MODULE XMOD
C
C
C
      MODULE BLPMOD
C       /BLPAR/
        REAL    SCCON, GACON, GBCON, GCCON, DLCON,
     &          CTRCON, CTRCEX, DUXCON, CTCON
      END MODULE BLPMOD
C
C
C
      MODULE BLMOD
C
        USE BLPMOD
C
        PARAMETER (NCOM=73)
        REAL COM1(NCOM), COM2(NCOM)
        REAL M1, M1_U1, M1_MS, M2, M2_U2, M2_MS
C
      COMMON/VAR1/ X1,  U1,  T1,  D1,  S1, AMPL1, U1_UEI, U1_MS, DW1
     &        , H1, H1_T1, H1_D1
     &        , M1, M1_U1,                            M1_MS
     &        , R1, R1_U1,                            R1_MS
     &        , V1, V1_U1,                            V1_MS,  V1_RE
     &        , HK1, HK1_U1, HK1_T1, HK1_D1,         HK1_MS
     &        , HS1, HS1_U1, HS1_T1, HS1_D1,         HS1_MS, HS1_RE
     &        , HC1, HC1_U1, HC1_T1, HC1_D1,         HC1_MS
     &        , RT1, RT1_U1, RT1_T1,                 RT1_MS, RT1_RE
     &        , CF1, CF1_U1, CF1_T1, CF1_D1,         CF1_MS, CF1_RE
     &        , DI1, DI1_U1, DI1_T1, DI1_D1, DI1_S1, DI1_MS, DI1_RE
     &        , US1, US1_U1, US1_T1, US1_D1,         US1_MS, US1_RE
     &        , CQ1, CQ1_U1, CQ1_T1, CQ1_D1,         CQ1_MS, CQ1_RE
     &        , DE1, DE1_U1, DE1_T1, DE1_D1,         DE1_MS
      COMMON/VAR2/ X2,  U2,  T2,  D2,  S2, AMPL2, U2_UEI, U2_MS, DW2
     &        , H2, H2_T2, H2_D2
     &        , M2, M2_U2,                            M2_MS
     &        , R2, R2_U2,                            R2_MS
     &        , V2, V2_U2,                            V2_MS,  V2_RE
     &        , HK2, HK2_U2, HK2_T2, HK2_D2,         HK2_MS
     &        , HS2, HS2_U2, HS2_T2, HS2_D2,         HS2_MS, HS2_RE
     &        , HC2, HC2_U2, HC2_T2, HC2_D2,         HC2_MS
     &        , RT2, RT2_U2, RT2_T2,                 RT2_MS, RT2_RE
     &        , CF2, CF2_U2, CF2_T2, CF2_D2,         CF2_MS, CF2_RE
     &        , DI2, DI2_U2, DI2_T2, DI2_D2, DI2_S2, DI2_MS, DI2_RE
     &        , US2, US2_U2, US2_T2, US2_D2,         US2_MS, US2_RE
     &        , CQ2, CQ2_U2, CQ2_T2, CQ2_D2,         CQ2_MS, CQ2_RE
     &        , DE2, DE2_U2, DE2_T2, DE2_D2,         DE2_MS
        EQUIVALENCE (X1,COM1(1)), (X2,COM2(1))
C       /VARA/
        REAL    CFM, CFM_MS, CFM_RE
     &        , CFM_U1, CFM_T1, CFM_D1
     &        , CFM_U2, CFM_T2, CFM_D2
     &        , XT,    XT_A1, XT_MS, XT_RE, XT_XF
     &        , XT_X1, XT_T1, XT_D1, XT_U1
     &        , XT_X2, XT_T2, XT_D2, XT_U2
C       /SAV/
        REAL    C1SAV(NCOM), C2SAV(NCOM)
C       /VAR/
        REAL    QINFBL
     &        , TKBL  , TKBL_MS
     &        , RSTBL , RSTBL_MS
     &        , HSTINV, HSTINV_MS
     &        , REYBL , REYBL_MS, REYBL_RE
     &        , GM1BL, HVRAT
     &        , BULE, XIFORC, AMCRIT
        LOGICAL SIMI,TRAN,TURB,WAKE
        LOGICAL TRFORC,TRFREE
C       /SYS/
        REAL    VS1(4,5),VS2(4,5),VSREZ(4),VSR(4),VSM(4),VSX(4)
      END MODULE BLMOD
C
C
C
      MODULE CMOD
C        PARAMETER (ICX=257)
        INTEGER, PARAMETER :: ICX=257
C       SET IMX = (ICX-1)/4
C        PARAMETER (IMX=64)
        INTEGER, PARAMETER :: IMX=64
C
        COMPLEX DZTE, CHORDZ, ZLEOLD,
     &          ZC(ICX), ZC_CN(ICX, IMX/4),
     &          PIQ(ICX), CN(0:IMX), EIW(ICX,0:IMX)
C
        INTEGER NC,MC,MCT
C
        REAL PI, AGTE, AG0, QIM0, QIMOLD, DWC, WC(ICX), SC(ICX),
     &       SCOLD(ICX), XCOLD(ICX), YCOLD(ICX)
      END MODULE CMOD
C
C
C   VERSION     version number of this XFOIL implementation
C
C   FNAME       airfoil data filename
C   PFNAME(.)   polar append filename
C   PFNAMX(.)   polar append x/c dump filename
C   ONAME       default overlay airfoil filename
C   PREFIX      default filename prefix
C   NAME        airfoil name
C
C   ISPARS      ISES domain parameters  (not used in XFOIL)
C
C   Q(..)       generic coefficient matrix
C   DQ(.)       generic matrix righthand side
C
C   DZDG(.)     dPsi/dGam
C   DZDN(.)     dPsi/dn
C   DZDM(.)     dPsi/dSig
C
C   DQDG(.)     dQtan/dGam
C   DQDM(.)     dQtan/dSig
C   QTAN1       Qtan at alpha =  0 deg.
C   QTAN2       Qtan at alpha = 90 deg.
C
C   Z_QINF      dPsi/dQinf
C   Z_ALFA      dPsi/dalfa
C   Z_QDOF0     dPsi/dQdof0
C   Z_QDOF1     dPsi/dQdof1
C   Z_QDOF2     dPsi/dQdof2
C   Z_QDOF3     dPsi/dQdof3
C
C   AIJ(..)     dPsi/dGam  influence coefficient matrix (factored if LQAIJ=t)
C   BIJ(..)     dGam/dSig  influence coefficient matrix
C   CIJ(..)     dQtan/dGam influence coefficient matrix
C   DIJ(..)     dQtan/dSig influence coefficient matrix
C   QINV(.)     tangential velocity due to surface vorticity
C   QVIS(.)     tangential velocity due to surface vorticity & mass sources
C   QINVU(..)   QINV for alpha = 0, 90 deg.
C   QINV_A(.)   dQINV/dalpha
C
C   X(.),Y(.)   airfoil (1<i<N) and wake (N+1<i<N+NW) coordinate arrays
C   XP(.),YP(.) dX/dS, dY/dS arrays for spline evaluation
C   S(.)        arc length along airfoil (spline parameter)
C   SLE         value of S at leading edge
C   XLE,YLE     leading  edge coordinates
C   XTE,YTE     trailing edge coordinates
C   WGAP(.)     thickness of "dead air" region inside wake just behind TE
C   WAKLEN      wake length to chord ratio
C
C   GAM(.)      surface vortex panel strength array
C   GAMU(.2)    surface vortex panel strength arrays for alpha = 0, 90 deg.
C   GAM_A(.)    dGAM/dALFA
C   SIG(.)      surface and wake mass defect array
C
C   NX(.),NY(.) normal unit vector components at airfoil and wake coordinates
C   APANEL(.)   surface and wake panel angle array (+ counterclockwise)
C
C   SST         S value at stagnation point
C   SST_GO      dSST/dGAM(IST)
C   SST_GP      dSST/dGAM(IST+1)
C
C   GAMTE       vortex panel strength across finite-thickness TE
C   SIGTE       source panel strength across finite-thickness TE
C   GAMTE_A     dGAMTE/dALFA
C   SIGTE_A     dSIGTE/dALFA
C   DSTE        TE panel length
C   ANTE,ASTE   projected TE thickness perp.,para. to TE bisector
C   SHARP       .TRUE.  if  DSTE.EQ.0.0 ,  .FALSE. otherwise
C
C   SSPEC(.)    normalized arc length around airfoil (QSPEC coordinate)
C   XSPOC(.)    x/c at SSPEC points
C   YSPOC(.)    y/c at SSPEC points
C   QSPEC(..)   specified surface velocity for inverse calculations
C   QSPECP(..)  dQSPEC/dSSPEC
C   QGAMM(.)    surface velocity for current airfoil geometry
C   SSPLE       SSPEC value at airfoil nose
C
C   IQ1,IQ2     target segment endpoint indices on Qspec(s) plot
C   NSP         number of points in QSPEC array
C   NQSP        number Qspec arrays
C   IACQSP      1:  ALQSP is prescribed for Qspec arrays
C               2:  CLQSP is prescribed for Qspec arrays
C   NC1         number of circle plane points, must be 2**n - 1
C
C   NNAME       number of characters in airfoil name
C   NPREFIX     number of characters in default filename prefix
C
C   ALQSP(.)    alpha,CL,CM corresponding to QSPEC distributions
C   CLQSP(.)    
C   CMQSP(.)    
C   ALGAM       alpha,CL,CM corresponding to QGAMM distribution
C   CLGAM
C   CMGAM
C
C   QF0(.)      shape function for QSPEC modification
C   QF1(.)        "
C   QF2(.)        "
C   QF3(.)        "
C   QDOF0       shape function weighting coefficient (inverse DOF)
C   QDOF1         "
C   QDOF2         "
C   QDOF3         "
C   CLSPEC      specified CL
C   FFILT       circle-plane mapping filter parameter
C
C   ADEG,ALFA   angle of attack in degrees, radians
C   AWAKE       angle of attack corresponding to wake geometry (radians)
C   AVISC       angle of attack corresponding to BL solution   (radians)
C   MVISC       Mach number corresponding to BL solution
C   CL,CM       current CL and CM calculated from GAM(.) distribution
C   CD          current CD from BL solution
C   CDF         current friction CD from BL solution
C   CL_ALF      dCL/dALFA
C   CL_MSQ      dCL/d(MINF^2)
C
C   PSIO        streamfunction inside airfoil
C   CIRC        circulation
C   COSA,SINA   cos(ALFA), sin(ALFA)
C   QINF        freestream speed    (defined as 1)
C   GAMMA,GAMM1 Gas constant Cp/Cv, Cp/Cv - 1
C   MINF1       freestream Mach number at CL=1
C   MINF        freestream Mach number at current CL
C   MINF_CL     dMINF/dCL
C   TKLAM       Karman-Tsien parameter Minf^2 / [1 + sqrt(1-Minf^2)]^2
C   TKL_MSQ     d(TKLAM)/d(MINF^2)
C   CPSTAR      sonic pressure coefficient
C   QSTAR       sonic speed
C
C   NCPREF      number of reference Cp vs x/c points
C   XPREF(.)    x/c array corresponding to reference Cp data array
C   CPREF(.)    reference Cp data array
C   LABREF      reference Cp data descriptor string
C
C   NLREF       number of characters in LABREF string
C   NAPOL(.)    number of points in each stored polar
C   NPOL        number of stored polars
C   IPACT       index of "active" polar being accumulated (0 if none are)
C   ILINP(.)    line style for each polar
C   ICOLP(.)    color for each polar
C   ISYMR(.)    symbol type for each reference polar
C   ICOLR(.)    color for each reference polar
C
C   NDREF(..)   number of points in each stored reference polar
C   NPOLREF     number of stored reference polars
C
C   VERSPOL(.)  version number of generating-code for each polar
C   CPOL(...)   CL,CD,and other parameters for each polar
C   CPOLXY(.1.) x,y coordinates of airfoil geometry which generated each polar
C   CPOLXY(.2.)
C   NXYPOL(.)   number of x,y points in CPOLXY array
C
C   PXTR(..)    transition locations for each polar
C   NAMEPOL(.)  airfoil names for each polar
C   CODEPOL(.)  generating-code names for each polar
C
C   NAMEREF(.)  name label of reference polar
C
C   PI          3.1415926...
C   HOPI,QOPI   1/(2 Pi) ,  1/(4 Pi)
C   DTOR        Pi / 180    (degrees to radians conversion factor)
C
C   CVPAR       curvature attraction parameter for airfoil paneling
C               0 = uniform panel node spacing around airfoil
C              ~1 = panel nodes strongly bunched in areas of large curvature
C   CTERAT      TE panel density / LE panel density ratio
C   CTRRAT      local refinement panel density / LE panel density ratio
C   XSREF1-2    suction  side local refinement x/c limits
C   XPREF1-2    pressure side local refinement x/c limits
C
C   N           number of points on airfoil
C   NB          number of points in buffer airfoil array
C   NW          number of points in wake
C   NPAN        default/specified number of points on airfoil
C
C   IST         stagnation point lies between S(IST), S(IST+1)
C   ITMAX       max number of Newton iterations
C   NSEQEX      max number of unconverged sequence points for early exit
C
C   RETYP       index giving type of Re variation with CL ...
C            ... 1  Re constant 
C            ... 2  Re ~ 1/sqrt(CL)    (fixed lift)
C            ... 3  Re ~ 1/CL          (fixed lift and dynamic pressure)
C
C   MATYP       index giving type of Ma variation with CL ...
C            ... 1  Ma constant 
C            ... 2  Ma ~ 1/sqrt(CL)    (fixed lift)
C
C   AIJPIV(.)   pivot index array for LU factoring routine
C
C   IDEV        "device" number for normal screen plotting
C   IDEVRP      "device" number for replotting (typically for hardcopy)
C   IPSLU       PostScript file specifier
C   NCOLOR      Number of defined colors in colormap
C   ICOLS(1)    color indices of top side
C   ICOLS(2)    color indices of bottom side
C
C   NOVER       number of airfoils overlaid on GDES geometry plot
C
C   SCRNFR      screen fraction taken up by initial plot window
C   SIZE        plot width (inches)
C   PLOTAR      plot aspect ratio
C   XWIND,YWIND window size in inches
C   XPAGE,YPAGE plot-page size in inches (for hardcopy)
C   XMARG,YMARG margin dimensions in inches
C   PFAC        scaling factor for  Cp
C   QFAC        scaling factor for  q  (surface speed)
C   VFAC        scaling factor for  Cp vectors
C   CH          character width / plot size  ratio
C   CHG         character width / plot size  ratio for geometry plot
C   CHQ         character width / plot size  ratio for Qspec(s) plot
C
C   XOFAIR      x offset for airfoil in  Cp vs x plots
C   YOFAIR      y offset for airfoil in  Cp vs x plots
C   FACAIR      scale factor for airfoil in  Cp vs x plots
C   XOFA        x offset for airfoil in  Cp vs x plots in airfoil units
C   YOFA        y offset for airfoil in  Cp vs x plots in airfoil units
C   FACA        scale factor for airfoil in  Cp vs x plots  in airfoil units
C   UPRWT       u/Qinf scale factor for profile plotting
C   CPMAX       max Cp  in  Cp vs x plots
C   CPMIN       min Cp  in  Cp vs x plots
C   CPDEL       delta Cp  in  Cp vs x plots
C
C   CPOLPLF(1,ICD)  min CD in CD-CL polar plot
C   CPOLPLF(2,ICD)  max CD in CD-CL polar plot
C   CPOLPLF(3,ICD)  delta CD in CD-CL polar plot
C
C   XCDWID      width of CD   -CL polar plot
C   XALWID      width of alpha-CL polar plot
C   XOCWID      width of Xtr/c-CL polar plot
C
C   OK          user question response
C   LGAMU       .TRUE. if GAMU  arrays exist for current airfoil geometry
C   LQINU       .TRUE. if QINVU arrays exist for current airfoil geometry
C   LVISC       .TRUE. if viscous option is invoked
C   LALFA       .TRUE. if alpha is specifed, .FALSE. if CL is specified
C   LWAKE       .TRUE. if wake geometry has been calculated
C   LPACC       .TRUE. if each point calculated is to be saved
C   LBLINI      .TRUE. if BL has been initialized
C   LIPAN       .TRUE. if BL->panel pointers IPAN have been calculated
C   LQAIJ       .TRUE. if dPsi/dGam matrix has been computed and factored
C   LADIJ       .TRUE. if dQ/dSig matrix for the airfoil has been computed
C   LWDIJ       .TRUE. if dQ/dSig matrix for the wake has been computed
C   LQVDES      .TRUE. if viscous Ue is to be plotted in QDES routines
C   LQSPEC      .TRUE. if Qspec has been initialized
C   LQREFL      .TRUE. if reflected Qspec is to be plotted in QDES routines
C   LVCONV      .TRUE. if converged BL solution exists
C   LCPREF      .TRUE. if reference data is to be plotted on Cp vs x/c plots
C   LCLOCK      .TRUE. if source airfoil coordinates are clockwise
C   LPFILE      .TRUE. if polar file is ready to be appended to
C   LPFILX      .TRUE. if polar dump file is ready to be appended to
C   LPPSHO      .TRUE. if CL-CD polar is plotted during point sequence
C   LBFLAP      .TRUE. if buffer  airfoil flap parameters are defined
C   LFLAP       .TRUE. if current airfoil flap parameters are defined
C   LEIW        .TRUE. if unit circle complex number array is initialized
C   LSCINI      .TRUE. if old-airfoil circle-plane arc length s(w) exists
C   LFOREF      .TRUE. if CL,CD... data is to be plotted on Cp vs x/c plots
C   LNORM       .TRUE. if input buffer airfoil is to be normalized
C   LGSAME      .TRUE. if current and buffer airfoils are identical
C   LDCPLOT     .TRUE. if delta(Cp) plot is to be plotted in CAMB menu
C
C   LPLCAM      .TRUE. if thickness and camber are to be plotted
C   LQSYM       .TRUE. if symmetric Qspec will be enforced
C   LGSYM       .TRUE. if symmetric geometry will be enforced
C   LQGRID      .TRUE. if grid is to overlaid on Qspec(s) plot
C   LGGRID      .TRUE. if grid is to overlaid on buffer airfoil geometry plot
C   LGTICK      .TRUE. if node tick marks are to be plotted on buffer airfoil
C   LQSLOP      .TRUE. if modified Qspec(s) segment is to match slopes
C   LGSLOP      .TRUE. if modified geometry segment is to match slopes
C   LCSLOP      .TRUE. if modified camber line segment is to match slopes
C   LQSPPL      .TRUE. if current Qspec(s) in in plot
C   LGEOPL      .TRUE. if current geometry in in plot
C   LCPGRD      .TRUE. if grid is to be plotted on Cp plots
C   LBLGRD      .TRUE. if grid is to be plotted on BL variable plots
C   LBLSYM      .TRUE. if symbols are to be plotted on BL variable plots
C   LCMINP      .TRUE. if min Cp is to be written to polar file for cavitation
C   LHMOMP      .TRUE. if hinge moment is to be written to polar file
C   LFREQP      .TRUE. if individual TS-wave frequencies are to be plotted
C
C   LPGRID      .TRUE. if polar grid overlay is enabled
C   LPCDW       .TRUE. if polar CDwave is plotted
C   LPLIST      .TRUE. if polar listing lines (at top of plot) are enabled
C   LPLEGN      .TRUE. if polar legend is enabled
C   
C   LPLOT       .TRUE. if plot page is open
C   LSYM        .TRUE. if symbols are to be plotted in QDES routines
C   LIQSET      .TRUE. if inverse target segment is marked off in QDES
C   LCLIP       .TRUE. if line-plot clipping is to be performed
C   LVLAB       .TRUE. if label is to be plotted on viscous-variable plots
C   LCURS       .TRUE. if cursor input is to be used for blowups, etc.
C   LLAND       .TRUE. if Landscape orientation for PostScript is used
C
C
C   XB(.),YB(.) buffer airfoil coordinate arrays
C   XBP(.)      dXB/dSB
C   YBP(.)      dYB/dSB
C   SB(.)       spline parameter for buffer airfoil
C   SNEW(.)     new panel endpoint arc length array
C
C   XBF,YBF     buffer  airfoil flap hinge coordinates
C   XOF,YOF     current airfoil flap hinge coordinates
C   HMOM        moment of flap about hinge point
C   HFX         x-force of flap on hinge point
C   HFY         y-force of flap on hinge point
C
C~~~~~~~~~~~~~~ properties of current buffer airfoil
C
C   XBMIN,XBMAX  limits of XB array
C   YBMIN,YBMAX  limits of YB array
C   SBLE         LE tangency-point SB location
C   CHORDB       chord
C   AREAB        area
C   RADBLE       LE radius
C   ANGBTE       TE angle  (rad)
C   
C   EI11BA       bending inertia about axis 1    x^2 dx dy
C   EI22BA       bending inertia about axis 2    y^2 dx dy
C   APX1BA       principal axis 1 angle 
C   APX2BA       principal axis 2 angle 
C
C   EI11BT       bending inertia about axis 1    x^2 t ds
C   EI22BT       bending inertia about axis 2    y^2 t ds
C   APX1BT       principal axis 1 angle 
C   APX2BT       principal axis 2 angle 
C
C   THICKB       max thickness
C   CAMBRB       max camber
C
C~~~~~~~~~~~~~~
C
C   XSSI(..)    BL arc length coordinate array on each surface
C   UEDG(..)    BL edge velocity array
C   UINV(..)    BL edge velocity array without mass defect influence
C   MASS(..)    BL mass defect array  ( = UEDG*DSTR )
C   THET(..)    BL momentum thickness array
C   DSTR(..)    BL displacement thickness array
C   CTAU(..)    sqrt(max shear coefficient) array
C               (in laminar regions, log of amplification ratio)
C
C   TAU(..)     wall shear stress array                 (for plotting only)
C   DIS(..)     dissipation array                       (for plotting only)
C   CTQ(..)     sqrt(equilibrium max shear coefficient) array (  "  )
C   VTI(..)     +/-1 conversion factor between panel and BL variables
C   UINV_A(..)  dUINV/dalfa array
C 
C   REINF1      Reynolds number  Vinf c / ve  for CL=1
C   REINF       Reynolds number for current CL
C   REINF_CL    dREINF/dCL
C
C   ACRIT       log (critical amplification ratio)
C   XSTRIP(.)   transition trip  x/c locations (if XTRIP > 0),
C               transition trip -s/s_side locations (if XTRIP < 0),
C   XOCTR(.)    actual transition x/c locations
C   YOCTR(.)    actual transition y/c locations
C   XSSITR(.)   actual transition xi locations
C
C   IBLTE(.)    BL array index at trailing edge
C   NBL(.)      max BL array index
C   IPAN(..)    panel index corresponding to BL location
C   ISYS(..)    BL Newton system line number corresponding to BL location
C   NSYS        total number of lines in BL Newton system
C   ITRAN(.)    BL array index of transition interval
C   TFORCE(.)   .TRUE. if transition is forced due to transition strip
C
C   VA,VB(...)  diagonal and off-diagonal blocks in BL Newton system
C   VZ(..)      way-off-diagonal block at TE station line
C   VM(...)     mass-influence coefficient vectors in BL Newton system
C   VDEL(..)    residual and solution vectors in BL Newton system
C
C   RMSBL       rms change from BL Newton system solution
C   RMXBL       max change from BL Newton system solution
C   IMXBL       location of max change
C   ISMXBL      index of BL side containing max change
C   VMXBL       character identifying variable with max change
C   RLX         underrelaxation factor for Newton update
C   VACCEL      parameter for accelerating BL Newton system solution
C               (any off-diagonal element < VACCEL is not eliminated,
C                which speeds up each iteration, but MAY increase
C                iteration count)
C                Can be set to zero for unadulterated Newton method
C
C   XOFF,YOFF   x and y offsets for windowing in QDES,GDES routines
C   XSF ,YSF    x and y scaling factors for windowing in QDES,GDES routines
C
C   XGMIN       airfoil grid plot limits
C   XGMAX
C   YGMIN
C   YGMAX
C   DXYG        airfoil grid-plot annotation increment
C   GTICK       airfoil-plot tick marks size (as fraction of arc length)
C
C~~~~~~~~~~~~~~~ BLPAR.INC
C
C   SCCON       shear coefficient lag constant
C   GACON       G-beta locus constants...
C   GBCON       G = GACON * sqrt(1.0 + GBCON*beta) 
C   GCCON              + GCCON / [H*Rtheta*sqrt(Cf/2)]   <-- wall term
C   DLCON       wall/wake dissipation length ratio  Lo/L
C   CTCON       Ctau weighting coefficient (implied by G-beta constants)
C
C~~~~~~~~~~~~~~ CIRCLE.INC
C
C   NC         number of circle plane points, must be 2**n + 1
C   MC         number of Fourier harmonics of P(w) + iQ(w)
C   MCT        number of Fourier harmonics for which dZC/dCN are calculated
C
C   PI         3.1415926
C   AGTE       trailing edge angle/pi
C   AG0        angle of airfoil surface at first point
C   QIM0       Q(w) offset   = Q(0)
C   QIMOLD     Q(w) offset for old airfoil
C   DWC        increment of circle-plane coordinate w,  DWC = 2 pi/(NC-1)
C   WC(.)      circle plane coordinate w for Fourier operations
C   SC(.)      normalized arc length array s(w)
C   SCOLD(.)   normalized arc length s(w) of old airfoil
C   XCOLD(.)   x coordinate x(w) of old airfoil
C   YCOLD(.)   y coordinate y(w) of old airfoil
C
C   DZTE       trailing edge gap specified in the complex plane
C   CHORDZ     airfoil chord specified in the complex plane
C   ZLEOLD     leading edge of old airfoil
C   ZC(.)      complex airfoil coordinates derived from P(w) + iQ(w)
C   ZC_CN(..)  sensitivities dZC/dCN for driving geometry constraints
C   PIQ(.)     complex harmonic function P(w) + iQ(w)
C   CN(.)      Fourier coefficients of P(w) + iQ(w)
C   EIW(..)    complex number  exp(inw)  array on the unit circle 