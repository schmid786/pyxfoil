C***********************************************************************
C    Module:  xmdes.f
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
      SUBROUTINE SORT(KK,S,W)
      DIMENSION S(KK), W(KK)
      LOGICAL DONE
C
C---- sort arrays
      DO 10 IPASS=1, 1234
        DONE = .TRUE.
        DO 101 N=1, KK-1
          NP = N+1
          IF(S(NP).GE.S(N)) GO TO 101
           TEMP = S(NP)
           S(NP) = S(N)
           S(N) = TEMP
           TEMP = W(NP)
           W(NP) = W(N)
           W(N) = TEMP
           DONE = .FALSE.
  101   CONTINUE
        IF(DONE) GO TO 11
   10 CONTINUE
      WRITE(*,*) 'Sort failed'
C
C---- search for duplicate pairs and eliminate each one
   11 KKS = KK
      DO 20 K=1, KKS
        IF(K.GE.KK) RETURN
        IF(S(K).NE.S(K+1)) GO TO 20
C------- eliminate pair
         KK = KK-2
         DO 201 KT=K, KK
           S(KT) = S(KT+2)
           W(KT) = W(KT+2)
  201    CONTINUE
   20 CONTINUE
C
      RETURN
      END SUBROUTINE SORT


C       SUBROUTINE MDES
C C------------------------------------
C C     Full-Inverse design routine.
C C     Based on circle plane mapping.
C C------------------------------------
C       USE XMOD
C C
C       CHARACTER*4 COMAND, COMOLD
C       CHARACTER*80 LINE
C C
C       CHARACTER*128 COMARG, ARGOLD
C C
C       DIMENSION IINPUT(20)
C       DIMENSION RINPUT(20)
C       LOGICAL ERROR
C C
C       SAVE COMOLD, ARGOLD
C C
C       COMAND = '****'
C       COMARG = ' '
C C
C       IF(N.EQ.0) THEN
C        WRITE(*,*)
C        WRITE(*,*) '***  No airfoil available  ***'
C        RETURN
C       ENDIF
C C
C     1 CONTINUE
C C
C C---- see if current Qspec, if any, didn't come from Mixed-Inverse
C       IF(NSP.NE.NC1) THEN
C        LQSPEC = .FALSE.
C        IQ1 = 1
C        IQ2 = NC1
C       ENDIF
C C
C C---- initialize Fourier transform arrays if it hasn't been done
C       IF(.NOT.LEIW  ) CALL EIWSET(NC1)
C       LEIW = .TRUE.
C C
C C---- if Qspec alpha has never been set, set it to current alpha
C       IF(NQSP .EQ. 0) THEN
C         IACQSP = 1
C         ALQSP(1) = ALFA
C         NQSP = 1
C       ENDIF
C C
C       IF(.NOT.LSCINI) THEN
C C------ initialize s(w) for current airfoil, generating its Cn coefficients
C         CALL SCINIT(N,X,XP,Y,YP,S,SLE)
C         LSCINI = .TRUE.
C C
C C------ set up to initialize Qspec to current conditions
C         LQSPEC = .FALSE.
C       ENDIF
C C
C C---- set initial Q for current alpha
C       ALGAM = ALFA
C       CALL MAPGAM(1,ALGAM,CLGAM,CMGAM)
C       WRITE(*,1150) ALGAM/DTOR, CLGAM
C C
C       IF(.NOT.LQSPEC) THEN
C C------ set Cn coefficients from current Q
C         CALL CNCALC(QGAMM,.FALSE.)
C C
C C------ set Qspec from Cn coefficients
C         CALL QSPCIR
C         WRITE(*,1190)
C       ENDIF
C C
C C====================================================
C C---- start of menu loop
C  500  CONTINUE
C       COMOLD = COMAND
C       ARGOLD = COMARG
C C
C  501  IF(LQSYM) THEN
C        CALL ASKC('.MDESs^',COMAND,COMARG)
C       ELSE
C        CALL ASKC('.MDES^',COMAND,COMARG)
C       ENDIF
C C
C  505  CONTINUE
C C
C C---- process previous command ?
C       IF(COMAND(1:1).EQ.'!') THEN
C         IF(COMOLD.EQ.'****') THEN
C           WRITE(*,*) 'Previous .MDES command not valid'
C           GO TO 501
C         ELSE
C           COMAND = COMOLD
C           COMARG = ARGOLD
C         ENDIF
C       ENDIF
C C
C       IF(COMAND.EQ.'    ') THEN
C C----- just <return> was typed... exit MDES
C        LQSYM = .FALSE.
C        RETURN
C       ENDIF
C C
C C---- extract command line numeric arguments
C       DO I=1, 20
C         IINPUT(I) = 0
C         RINPUT(I) = 0.0
C       ENDDO
C       NINPUT = 0
C       CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
C       NINPUT = 0
C       CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C C
C C--------------------------------------------------------
C       IF(COMAND.EQ.'?   ') THEN
C        WRITE(*,1050)
C  1050  FORMAT(
C      &   /'   <cr>   Return to Top Level'
C      &   /'   !      Redo previous command'
C      &  //'   INIT   Re-initialize mapping'
C      &   /'   QSET   Reset Qspec <== Q'
C      &   /'   AQ r.. Show/select alpha(s) for Qspec'
C      &   /'   CQ r.. Show/select  CL(s)   for Qspec'
C      &  //'   Symm   Toggle symmetry flag'
C      &   /'   TGAP r Set new TE gap'
C      &   /'   TANG r Set new TE angle'
C      &   /'   READ   Read in Qspec from file'
C      &   /'   WRIT   Write out Qspec to file'
C      &  //'   Modi   Modify Qspec'
C      &   /'   MARK   Mark off target segment for smoothing'
C      &   /'   SMOO   Smooth Qspec inside target segment'
C      &   /'   FILT   Apply Hanning filter to entire Qspec'
C      &   /'   SLOP   Toggle modified-Qspec slope matching flag'
C      &  //'   eXec   Execute  full-inverse calculation'
C      &  //'   PERT   Perturb one Cn and generate geometry')
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'INIT') THEN
      SUBROUTINE M_INIT
       USE XMOD
C
       LQSPEC = .FALSE.
       LSCINI = .FALSE.
C----  see if current Qspec, if any, didn't come from Mixed-Inverse
       IF(NSP.NE.NC1) THEN
        LQSPEC = .FALSE.
        IQ1 = 1
        IQ2 = NC1
       ENDIF
C
C----  initialize Fourier transform arrays if it hasn't been done
       IF(.NOT.LEIW  ) CALL EIWSET(NC1)
       LEIW = .TRUE.
C
C----  if Qspec alpha has never been set, set it to current alpha
       IF(NQSP .EQ. 0) THEN
         IACQSP = 1
         ALQSP(1) = ALFA
         NQSP = 1
       ENDIF
C
       IF(.NOT.LSCINI) THEN
C------  initialize s(w) for current airfoil, generating its Cn coefficients
         CALL SCINIT(N,X,XP,Y,YP,S,SLE)
         LSCINI = .TRUE.
C
C------  set up to initialize Qspec to current conditions
         LQSPEC = .FALSE.
       ENDIF
C
C----  set initial Q for current alpha
       ALGAM = ALFA
       CALL MAPGAM(1,ALGAM,CLGAM,CMGAM)
C
       IF(.NOT.LQSPEC) THEN
C------  set Cn coefficients from current Q
         CALL CNCALC(QGAMM,.FALSE.)
C
C------  set Qspec from Cn coefficients
         CALL QSPCIR
       ENDIF
      END SUBROUTINE M_INIT
C        GO TO 1
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'QSET') THEN
      SUBROUTINE M_QSET
       USE XMOD
C
       CALL CNCALC(QGAMM,.FALSE.)
       IF(LQSYM) CALL CNSYMM
       CALL QSPCIR
      END SUBROUTINE M_QSET
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'AQ  ') THEN
      SUBROUTINE M_AQ(NI, ALPHA_IN)
       USE XMOD
       INTEGER :: NI
       REAL, DIMENSION(NI) :: ALPHA_IN
C----- set Qspec(s) for specified alphas
       DO K=1, NI
         ALQSP(K) = ALPHA_IN(K)*DTOR
       ENDDO
       NQSP = NI
C
       IACQSP = 1
       CALL QSPCIR
      END SUBROUTINE M_AQ
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CQ  ') THEN
      SUBROUTINE M_CQ(NI, CL_IN)
       USE XMOD
       INTEGER :: NI
       REAL, DIMENSION(NI) :: CL_IN
C----- set Qspec(s) for specified CLs
       DO K=1, NI
        CLQSP(K) = CL_IN(K)
       ENDDO
       NQSP = NI
C
       IACQSP = 2
       CALL QSPCIR
      END SUBROUTINE M_CQ
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SYMM' .OR.
C      &       COMAND.EQ.'S   '      ) THEN
      SUBROUTINE M_SYMM
       USE XMOD
C
       LQSYM = .NOT.LQSYM
       IF(LQSYM) THEN
        CALL CNSYMM
        CALL QSPCIR
       ENDIF
      END SUBROUTINE M_SYMM
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'TGAP') THEN
      SUBROUTINE M_TGAP(DXNEW,DYNEW)
       CALL DZTSET(DXNEW,DYNEW)
      END SUBROUTINE M_TGAP
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'TANG') THEN
      SUBROUTINE M_TANG(AGTED)
       CALL AGTSET(AGTED)
      END SUBROUTINE M_TANG
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'READ') THEN
C C----- read in Qspec from file
C        KQSP = 1
C        CALL GETVOV(KQSP)  !!! modified
C        CALL CNCALC(QSPEC(1,KQSP),.FALSE.)
C        IF(LQSYM) CALL CNSYMM
C C
C        KQSP = 1
C        CALL QSPINT(ALQSP(KQSP),QSPEC(1,KQSP),QINF,MINF,
C      &             CLQSP(KQSP),CMQSP(KQSP))
C        WRITE(*,1200) ALGAM/DTOR,CLGAM,CMGAM
C        WRITE(*,1210) KQSP, ALQSP(KQSP)/DTOR,CLQSP(KQSP),CMQSP(KQSP)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'WRIT') THEN
C C----- write out Qspec to file
C        KQSP = 1
C        CALL PUTVOV(KQSP) !!! modified
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SMOO') THEN
      SUBROUTINE M_SMOO
       USE XMOD
C----- smooth Qspec within target segment
       KQSP = KQTARG
       CALL SMOOQ(IQ1,IQ2,KQSP)
       CALL CNCALC(QSPEC(1,KQSP),LQSYM)
       CALL QSPCIR
C
       DO KQSP=1, NQSP
         CALL QSPINT(ALQSP(KQSP),QSPEC(1,KQSP),QINF,MINF,
     &               CLQ,CMQSP(KQSP))
C
C------- set new CL only if alpha is prescribed
         IF(IACQSP.EQ.1) CLQSP(KQSP) = CLQ
       ENDDO
      END SUBROUTINE M_SMOO
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'FILT') THEN
      SUBROUTINE M_FILT
       USE XMOD
C----- apply modified Hanning filter to Cn coefficients
       CFILT = 0.2
       CALL CNFILT(CFILT)
       CALL PIQSUM
       CALL QSPCIR
C
       DO KQSP=1, NQSP
         CALL QSPINT(ALQSP(KQSP),QSPEC(1,KQSP),QINF,MINF,
     &               CLQ,CMQSP(KQSP))
C
C------- set new CL only if alpha is prescribed
         IF(IACQSP.EQ.1) CLQSP(KQSP) = CLQ
       ENDDO
      END SUBROUTINE M_FILT
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SLOP') THEN
      SUBROUTINE M_SLOP
       USE XMOD
C
       LQSLOP = .NOT.LQSLOP
      END SUBROUTINE M_SLOP
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'DUMP') THEN
C        FNAME = COMARG
C        IF(FNAME(1:1).EQ.' ')
C      &    CALL ASKS('Enter Cn output filename^',FNAME)
C C
C        LU = 19
C        OPEN(LU,FILE=FNAME,STATUS='UNKNOWN')
C        CALL CNDUMP(LU)
C        CLOSE(LU)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'EXEC' .OR.
C      &       COMAND.EQ.'X   '      ) THEN
      SUBROUTINE M_EXEC
       USE XMOD
C----- execute full-inverse calculation
       CALL MAPGEN(NB,XB,YB)
C
C----- spline new buffer airfoil
       CALL SCALC(XB,YB,SB,NB)
       CALL SPLIND(XB,XBP,SB,NB,-999.0,-999.0)
       CALL SPLIND(YB,YBP,SB,NB,-999.0,-999.0)
C
       CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
     &             SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
     &             EI11BA,EI22BA,APX1BA,APX2BA,
     &             EI11BT,EI22BT,APX1BT,APX2BT,
     &             THICKB,CAMBRB )
C
       LGSAME = .FALSE.
      END SUBROUTINE M_EXEC
C C
C        WRITE(*,1300)
C  1300  FORMAT(//' New buffer airfoil generated'
C      &        /' Execute PANE at Top Level to set new current airfoil'/)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'PERT') THEN
      SUBROUTINE M_PERT
       USE XMOD
       CALL PERT(QSPEC(1,1))
C----- set Q(s) for changed Cn
       CALL QSPCIR
C----- go generate perturbed geometry
       CALL M_EXEC
      END SUBROUTINE M_PERT
C C
C C--------------------------------------------------------
C       ELSE
C        WRITE(*,1100) COMAND
C  1100  FORMAT(' Command ',A4,' not recognized.  Type a " ? " for list.')
C        COMAND = '****'
C C
C       ENDIF
C C
C       GO TO 500
C C
C C....................................................
C C
C  1150 FORMAT(/' Current Q operating condition:',
C      &        '   alpha = ', F7.3, '     CL = ', F8.4 )
C  1190 FORMAT(/' Qspec initialized to current Q' )
C  1200 FORMAT(
C      & /' Current :  alpha =', F9.4,'    CL =',F11.6,'    CM =',F11.6)
C  1210 FORMAT(
C      &  ' Qspec',I2,
C      &          ' :  alpha =', F9.4,'    CL =',F11.6,'    CM =',F11.6)
C  5000 FORMAT(A)
C       END MDES


      SUBROUTINE DZTSET(DXNEW,DYNEW)
      USE CMOD
C
      DZTE = CMPLX(DXNEW,DYNEW)
      RETURN
      END SUBROUTINE DZTSET


      SUBROUTINE AGTSET(AGTED)
      USE CMOD
C
      AGTE = AGTED/180.0
      RETURN
      END SUBROUTINE AGTSET



      SUBROUTINE MAPGAM(IAC,ALG,CLG,CMG)
C--------------------------------------------
C     Sets mapped Q for current airfoil
C     for angle of attack or CL.
C
C       IAC=1: specified ALGAM
C       IAC=2: specified CLGAM
C--------------------------------------------
      USE XMOD
C
C---- calculate q(w), set number of circle points NSP
      CALL QCCALC(IAC,ALG,CLG,CMG,MINF,QINF,NSP,W1,W2,W5,W6)
C
C---- store q(w), s(w), x(w), y(w)
      DO 3 I=1, NSP
        QGAMM(I) = W6(I)
        SSPEC(I) = W5(I)
    3 CONTINUE
C
      RETURN
      END SUBROUTINE MAPGAM


      SUBROUTINE QSPCIR
C----------------------------------------------------
C     Sets Qspec arrays for all design alphas or CLs
C----------------------------------------------------
      USE XMOD
C
      DO 10 KQSP=1, NQSP
        CALL QCCALC(IACQSP,ALQSP(KQSP),CLQSP(KQSP),CMQSP(KQSP),
     &              MINF,QINF,NSP,W1,W2,W5,QSPEC(1,KQSP))
        CALL SPLQSP(KQSP)
 10   CONTINUE
      LQSPEC = .TRUE.
C
      RETURN
      END SUBROUTINE QSPCIR


      SUBROUTINE MAPGEN(N,X,Y)
C--------------------------------------------------------
C     Calculates the geometry from the speed function
C     Fourier coefficients Cn, modifying them as needed
C     to achieve specified constraints.
C--------------------------------------------------------
      USE CMOD
      DIMENSION X(NC), Y(NC)
C
      COMPLEX QQ(IMX/4,IMX/4),DCN(IMX/4)
C
C---- preset rotation offset of airfoil so that initial angle is close
C-    to the old airfoil's angle
      DX = XCOLD(2) - XCOLD(1)
      DY = YCOLD(2) - YCOLD(1)
      QIM0 = ATAN2( DX , -DY )  +  0.5*PI*(1.0+AGTE)
      QIMOFF = QIM0 - IMAG(CN(0))
      CN(0) = CN(0) + CMPLX( 0.0 , QIMOFF )
C
C---- inverse-transform and calculate geometry ZC = z(w)
      CALL PIQSUM
      CALL ZCCALC(MCT)
C
C---- scale,rotate z(w) to get previous chord and orientation
      CALL ZCNORM(MCT)
C
CCCC---- put back rotation offset so speed routine QCCALC gets the right alpha
CCC      CN(0) = CN(0) - CMPLX( 0.0 , QIMOFF )
C
C---- enforce Lighthill's first constraint
      CN(0) = CMPLX( 0.0, IMAG(CN(0)) )
C
C---- number of free coefficients
      NCN = 1
C
C---- Newton iteration loop for modified Cn's
      DO 100 ITERCN=1, 10
        DO M=1, NCN
          DO L=1, NCN
            QQ(M,L) = 0.
          ENDDO
          DCN(M) = 0.
          QQ(M,M) = 1.0
        ENDDO
C
C------ fix TE gap
        M = 1
        DCN(M) = ZC(1) - ZC(NC)  -  DZTE
        DO L=1, NCN
          QQ(M,L) = ZC_CN(1,L) - ZC_CN(NC,L)
        ENDDO
C
        CALL CGAUSS(IMX/4,NCN,QQ,DCN,1)
C
        DCNMAX = 0.
        DO M=1, NCN
          CN(M) = CN(M) - DCN(M)
          DCNMAX = MAX( ABS(DCN(M)) , DCNMAX )
        ENDDO
C
        CALL PIQSUM
C
        CALL ZCCALC(MCT)
        CALL ZCNORM(MCT)
C
        WRITE(*,*) ITERCN, DCNMAX
        IF(DCNMAX.LE.5.0E-5) GO TO 101
  100 CONTINUE
      WRITE(*,*)
      WRITE(*,*) 'MAPGEN: Geometric constraints not fully converged'
C
  101 CONTINUE
C
C---- return new airfoil coordinates
      N = NC
      DO 120 I=1, NC
        X(I) = REAL(ZC(I))
        Y(I) = IMAG(ZC(I))
 120  CONTINUE
C
      RETURN
      END SUBROUTINE MAPGEN


      SUBROUTINE SCINIT(N,X,XP,Y,YP,S,SLE)
C----------------------------------------------------------
C     Calculates the circle-plane coordinate s(w) = SC
C     at each point of the current geometry.  
C     A by-product is the complex-mapping coefficients Cn.
C     (see CNCALC header for more info).
C----------------------------------------------------------
      USE CMOD
      DIMENSION X(N),XP(N),Y(N),YP(N),S(N)
C
      COMPLEX DCN, ZLE, ZTE
cc      DATA CEPS, SEPS / 1.0E-5, 5.0E-5 /
      DATA CEPS, SEPS / 1.0E-7, 5.0E-7 /
C
C---- set TE angle parameter
      AGTE = ( ATAN2( XP(N) , -YP(N) )
     &       - ATAN2( XP(1) , -YP(1) ) )/PI - 1.0
C
C---- set surface angle at first point
      AG0 = ATAN2( XP(1) , -YP(1) )
C
C---- temporary offset Qo to make  Q(w)-Qo = 0  at  w = 0 , 2 pi
C-     --- avoids Gibbs problems with Q(w)'s Fourier sine transform
      QIM0 = AG0 + 0.5*PI*(1.0+AGTE)
C
      XLE = SEVAL(SLE,X,XP,S,N)
      YLE = SEVAL(SLE,Y,YP,S,N)
C
C---- save TE gap and airfoil chord
      DXTE = X(1) - X(N)
      DYTE = Y(1) - Y(N)
      DZTE = CMPLX(DXTE,DYTE)
C
      CHORDX = 0.5*(X(1)+X(N)) - XLE
      CHORDY = 0.5*(Y(1)+Y(N)) - YLE
      CHORDZ = CMPLX( CHORDX , CHORDY )
      ZLEOLD = CMPLX( XLE , YLE )
C
C       WRITE(*,1100) REAL(DZTE), IMAG(DZTE), AGTE*180.0
C  1100 FORMAT(/' Current TE gap  dx dy =', 2F7.4,
C      &        '    TE angle =', F7.3,' deg.' / )
C       WRITE(*,*) 'Initializing mapping coordinate ...'
C
C---- set approximate slope ds/dw at airfoil nose
      CVLE = CURV(SLE,X,XP,Y,YP,S,N) * S(N)
      CVABS = ABS(CVLE)
      DSDWLE = MAX( 1.0E-3, 0.5/CVABS )
C
      TOPS = SLE/S(N)
      BOTS = (S(N)-SLE)/S(N)
C
C---- set initial top surface s(w)
      WWT = 1.0 - 2.0*DSDWLE/TOPS
      DO 10 IC=1, (NC-1)/2+1
        SC(IC) = TOPS*(1.0 - COS(WWT*WC(IC)) )
     &               /(1.0 - COS(WWT*PI    ) )
   10 CONTINUE
C
C---- set initial bottom surface s(w)
      WWT = 1.0 - 2.0*DSDWLE/BOTS
      DO 15 IC=(NC-1)/2+2, NC
        SC(IC) = 1.0
     &         - BOTS*(1.0 - COS(WWT*(WC(NC)-WC(IC))) )
     &               /(1.0 - COS(WWT* PI            ) )
   15 CONTINUE
C
C---- iteration loop for s(w) array
      DO 500 IPASS=1, 30
C
C---- calculate imaginary part of harmonic function  P(w) + iQ(w)
      DO 20 IC=1, NC
C
        SIC = S(1) + (S(N)-S(1))*SC(IC)
        DXDS = DEVAL(SIC,X,XP,S,N)
        DYDS = DEVAL(SIC,Y,YP,S,N)
C
C------ set Q(w) - Qo   (Qo defined so that Q(w)-Qo = 0  at  w = 0 , 2 pi)
        QIM = ATAN2( DXDS , -DYDS )
     &      - 0.5*(WC(IC)-PI)*(1.0+AGTE)
     &      - QIM0
C
        PIQ(IC) = CMPLX( 0.0 , QIM )
C
   20 CONTINUE
C
C---- Fourier-decompose Q(w)
      CALL FTP
C
C---- zero out average real part and add on Qo we took out above
      CN(0) = CMPLX( 0.0 , IMAG(CN(0))+QIM0 )
C
C---- transform back to get entire  PIQ = P(w) + iQ(w)
      CALL PIQSUM
C
C---- save s(w) for monitoring of changes in s(w) by ZCCALC
      DO 30 IC=1, NC
        SCOLD(IC) = SC(IC)
   30 CONTINUE
C
C---- correct n=1 complex coefficient Cn for proper TE gap
      DO 40 ITGAP=1, 5
        CALL ZCCALC(1)
C
C------ set current LE,TE locations
        CALL ZLEFIND(ZLE,ZC,WC,NC,PIQ,AGTE)
        ZTE = 0.5*(ZC(1)+ZC(NC))
C
        DZWT = ABS(ZTE-ZLE)/ABS(CHORDZ)
        DCN = -(ZC(1)      - ZC(NC)     - DZWT*DZTE )
     &       / (ZC_CN(1,1) - ZC_CN(NC,1)            )
        CN(1) = CN(1) + DCN
C
        CALL PIQSUM
        IF(ABS(DCN) .LT. CEPS) GO TO 41
   40 CONTINUE
   41 CONTINUE
C
      DSCMAX = 0.
      DO 51 IC=1, NC
        DSCMAX = MAX( DSCMAX , ABS(SC(IC)-SCOLD(IC)) )
   51 CONTINUE
C
C      WRITE(*,*) IPASS, '     max(dw) =', DSCMAX
      IF(DSCMAX .LT. SEPS) GO TO 505
C
  500 CONTINUE
  505 CONTINUE
C
C---- normalize final geometry
      CALL ZCNORM(1)
C
C---- set final  s(w), x(w), y(w)  arrays for old airfoil
      DO 510 IC=1, NC
        SCOLD(IC) = SC(IC)
        XCOLD(IC) = REAL(ZC(IC))
        YCOLD(IC) = IMAG(ZC(IC))
  510 CONTINUE
C
      QIMOLD = IMAG(CN(0))
C
cC---- print out Fourier coefficients
c      write(*,*) ' '
c      do 700 m=0, mc
c        write(*,*) m, real(cn(m)), IMAG(cn(m))
c        write(1,*) m, real(cn(m)), IMAG(cn(m))
ccc 7000   format(1x,i3,2f10.6)
c  700 continue
C
      RETURN
      END SUBROUTINE SCINIT



      SUBROUTINE CNCALC(QC,LSYMM)
C----------------------------------------------------------
C     Calculates the complex Fourier coefficients Cn of
C     the real part of the harmonic function P(w) + iQ(w)
C     which is set from either the current surface speed
C     function
C                                                  e
C                   2 cos(w/2 - alpha) [2 sin(w/2)]
C       P(w) =  ln  -------------------------------
C                               q(w)
C
C
C     or the geometry function
C
C                                         e
C                       z'(w) [2 sin(w/2)]
C          P(w) =   ln  ------------------
C                           2 sin(w/2)
C
C     depending on whether the speed q(w) or the
C     geometry z(w) is specified for that particular
C     value of w.  
C     (z(w) option is currently implemented separately in SCINIT)
C
C     By Fourier-transforming P(w) into a sequence 
C     of Fourier coefficients Cn, its complex conjugate 
C     function Q(w) is automatically determined by an 
C     inverse transformation in PIQSUM.  The overall 
C     P(w) + iQ(w) then uniquely defines the overall 
C     airfoil geometry, which is calculated in ZCCALC.
C
C     If LSYMM=t, then the Real(Cn) change from current
C     Cn values is doubled, and Imag(Cn) is zeroed out.
C----------------------------------------------------------
      USE CMOD
      REAL QC(NC)
      LOGICAL LSYMM
C
      DIMENSION QCW(ICX)
C
      COMPLEX CNSAV
      COMMON /WORK/ CNSAV(0:IMX)
C
cc      REAL WCJ(2)
C
      IF(NC .GT. ICX) STOP 'CNCALC: Array overflow.'
C
ccC---- assume q(w) segment is entire airfoil
cc      WCJ(1) = WC(1)
cc      WCJ(2) = WC(NC)
ccC
cc      IF(LIQSET) THEN
ccC----- set w at q(w) segment endpoints
cc       WCJ(1) = WC(IQ1)
cc       WCJ(2) = WC(IQ2)
cc      ENDIF
C
C---- spline q(w)
      CALL SPLIND(QC,QCW,WC,NC,-999.0,-999.0)
C
C---- get approximate w value at stagnation point
      DO 10 IC=2, NC
        IF(QC(IC).LT.0.0) GO TO 11
   10 CONTINUE
   11 WCLE = WC(IC)
C
C---- set exact numerical w value at stagnation point from splined q(w)
      CALL SINVRT(WCLE,0.0,QC,QCW,WC,NC)
C
C---- set corresponding circle plane alpha
      ALFCIR = 0.5*(WCLE - PI)
C
C---- calculate real part of harmonic function  P(w) + iQ(w)
      DO 120 IC=2, NC-1
C
        COSW = 2.0*COS(0.5*WC(IC) - ALFCIR)
        SINW = 2.0*SIN(0.5*WC(IC))
        SINWE = SINW**AGTE
C
cc        IF(WC(IC).GE.WCJ(1) .AND. WC(IC).LE.WCJ(2)) THEN
C
C------- set P(w) from q(w)
         IF(ABS(COSW).LT.1.0E-4) THEN
C-------- use asymptotic form near stagnation point
          PFUN = ABS( SINWE/QCW(IC) )
         ELSE
C-------- use actual expression
          PFUN = ABS( COSW*SINWE/QC(IC) )
         ENDIF
C
cc        ELSE
ccC
ccC------- set P(w) from old geometry derivative z'(w)
cc         PFUN = ABS( ZCOLDW(IC)*SINWE/SINW )
ccC
cc        ENDIF
C
        PIQ(IC) = CMPLX( LOG(PFUN) , 0.0 )
C
  120 CONTINUE
C
C---- extrapolate P(w) to TE
      PIQ(1)  = 3.0*PIQ(2)    - 3.0*PIQ(3)    + PIQ(4)
      PIQ(NC) = 3.0*PIQ(NC-1) - 3.0*PIQ(NC-2) + PIQ(NC-3)
C
      DO 50 M=0, MC
        CNSAV(M) = CN(M)
 50   CONTINUE
C
C---- Fourier-transform P(w) to get new Cn coefficients
      CALL FTP
      CN(0) = CMPLX( 0.0 , QIMOLD )
C
      IF(LSYMM) THEN
        DO 60 M=1, MC
          CNR = 2.0*REAL(CN(M)) - REAL(CNSAV(M))
          CN(M) = CMPLX( CNR , 0.0 )
 60     CONTINUE
      ENDIF
C
      CALL PIQSUM
C
      RETURN
      END SUBROUTINE CNCALC


      SUBROUTINE CNSYMM
      USE CMOD
C
C---- eliminate imaginary (camber) parts of mapping coefficients
      DO 10 M=1, MC
        CN(M) = CMPLX( REAL(CN(M)) , 0.0 )
   10 CONTINUE
C
      CALL PIQSUM
      RETURN
      END SUBROUTINE CNSYMM


      SUBROUTINE PIQSUM
C---------------------------------------------
C     Inverse-transform to get back modified 
C     speed function and its conjugate.
C---------------------------------------------
      USE CMOD
      COMPLEX ZSUM
C
      DO 300 IC=1, NC
        ZSUM = (0.0,0.0)
        DO 310 M=0, MC
          ZSUM = ZSUM + CN(M)*CONJG(EIW(IC,M))
  310   CONTINUE
        PIQ(IC) = ZSUM
  300 CONTINUE
C
      RETURN
      END SUBROUTINE PIQSUM


      SUBROUTINE CNFILT(FFILT)
C-------------------------------------
C     Filters out upper harmonics 
C     with modified Hanning filter.
C-------------------------------------
      USE CMOD
C
      IF(FFILT.EQ.0.0) RETURN
C
      DO 10 M=0, MC
        FREQ = FLOAT(M)/FLOAT(MC)
        CWT = 0.5*(1.0 + COS(PI*FREQ))
        CWTX = CWT
        IF(FFILT.GT.0.0) CWTX = CWT**FFILT
        CN(M) = CN(M) * CWTX
   10 CONTINUE
C
      RETURN
      END SUBROUTINE CNFILT


      SUBROUTINE ZCCALC(MTEST)
C--------------------------------------------------------
C     Calculates the airfoil geometry z(w) from the
C     harmonic function P(w) + iQ(w).  Also normalizes
C     the coordinates to the old chord and calculates
C     the geometry sensitivities dz/dCn  (1 < n < MTEST)
C     for each point.
C--------------------------------------------------------
      USE CMOD
      COMPLEX DZDW1, DZDW2, DZ_PIQ1, DZ_PIQ2
C
C---- integrate upper airfoil surface coordinates from x,y = 4,0
      IC = 1
      ZC(IC) = (4.0,0.0)
      DO 10 M=1, MTEST
        ZC_CN(IC,M) = (0.0,0.0)
   10 CONTINUE
C
      SINW = 2.0*SIN(0.5*WC(IC))
      SINWE = 0.
      IF(SINW.GT.0.0) SINWE = SINW**(1.0-AGTE)
C
      HWC = 0.5*(WC(IC)-PI)*(1.0+AGTE) - 0.5*PI
      DZDW1 = SINWE * EXP( PIQ(IC) + CMPLX(0.0,HWC) )
      DO 20 IC=2, NC
C
        SINW = 2.0*SIN(0.5*WC(IC))
        SINWE = 0.
        IF(SINW.GT.0.0) SINWE = SINW**(1.0-AGTE)
C
        HWC = 0.5*(WC(IC)-PI)*(1.0+AGTE) - 0.5*PI
        DZDW2 = SINWE * EXP( PIQ(IC) + CMPLX(0.0,HWC) )
C
        ZC(IC)  = 0.5*(DZDW1+DZDW2)*DWC + ZC(IC-1)
        DZ_PIQ1 = 0.5* DZDW1       *DWC
        DZ_PIQ2 = 0.5*       DZDW2 *DWC
C
        DO 210 M=1, MTEST
          ZC_CN(IC,M) = DZ_PIQ1*CONJG(EIW(IC-1,M))
     &                + DZ_PIQ2*CONJG(EIW(IC  ,M))
     &                + ZC_CN(IC-1,M)
  210   CONTINUE
C
        DZDW1 = DZDW2
   20 CONTINUE
C
C---- set arc length array s(w)
      SC(1) = 0.
      DO 50 IC=2, NC
        SC(IC) = SC(IC-1) + ABS(ZC(IC)-ZC(IC-1))
   50 CONTINUE
C
C---- normalize arc length
      DO 60 IC=1, NC
        SC(IC) = SC(IC)/SC(NC)
   60 CONTINUE
C
      RETURN
      END SUBROUTINE ZCCALC


      SUBROUTINE ZCNORM(MTEST)
C-----------------------------------------------
C     Normalizes the complex airfoil z(w) to
C     the old chord and angle, and resets the
C     influence coefficients  dz/dCn .
C-----------------------------------------------
      USE CMOD
      COMPLEX ZCNEW, ZLE, ZTE, ZC_ZTE, ZTE_CN(IMX/4)
C
C---- find current LE location
      CALL ZLEFIND(ZLE,ZC,WC,NC,PIQ,AGTE)
C
C---- place leading edge at origin
      DO 60 IC=1, NC
        ZC(IC) = ZC(IC) - ZLE
   60 CONTINUE
C
C---- set normalizing quantities and sensitivities
      ZTE = 0.5*(ZC(1) + ZC(NC))
      DO 480 M=1, MTEST
        ZTE_CN(M) = 0.5*(ZC_CN(1,M) + ZC_CN(NC,M))
  480 CONTINUE
C
C---- normalize airfoil to proper chord, put LE at old position,
C-    and set sensitivities dz/dCn for the rescaled coordinates
      DO 500 IC=1, NC
        ZCNEW  = CHORDZ*ZC(IC)/ZTE
        ZC_ZTE = -ZCNEW/ZTE
        ZC(IC) = ZCNEW
        DO 510 M=1, MTEST
          ZC_CN(IC,M) = CHORDZ*ZC_CN(IC,M)/ZTE + ZC_ZTE*ZTE_CN(M)
  510   CONTINUE
  500 CONTINUE
C
C---- add on rotation to mapping coefficient so QCCALC gets the right alpha
      QIMOFF = -IMAG( LOG(CHORDZ/ZTE) )
      CN(0) = CN(0) - CMPLX( 0.0 , QIMOFF )
C
C---- shift airfoil to put LE at old location
      DO 600 IC=1, NC
        ZC(IC) = ZC(IC) + ZLEOLD
 600  CONTINUE
C
      RETURN
      END SUBROUTINE ZCNORM


      SUBROUTINE QCCALC(ISPEC,ALFA,CL,CM,MINF,QINF,
     &                  NCIR,XCIR,YCIR,SCIR,QCIR)
C---------------------------------------------------
C     Calculates the surface speed from the complex
C     speed function so that either a prescribed
C     ALFA or CL is achieved, depending on whether 
C     ISPEC=1 or 2.  The CL calculation uses the 
C     transformed Karman-Tsien Cp.
C---------------------------------------------------
      USE CMOD
      COMPLEX DZ, ZA, EIA, CMT,CFT,CFT_A
      DIMENSION XCIR(NC),YCIR(NC),SCIR(NC),QCIR(NC)
      DIMENSION QC_A(ICX)
      REAL MINF
      DATA AEPS / 5.0E-7 /
C
C---- Karman-Tsien quantities
      BETA = SQRT(1.0 - MINF**2)
      BFAC = 0.5*MINF**2 / (1.0 + BETA)
C
      NCIR = NC
C
C---- Newton iteration loop (executed only once if alpha specified)
      DO 1 IPASS=1, 10
C
C------ set alpha in the circle plane
        ALFCIR = ALFA - IMAG(CN(0))
C
        CMT   = (0.0,0.0)
        CFT   = (0.0,0.0)
        CFT_A = (0.0,0.0)
C
C------ set surface speed for current circle plane alpha
        DO 10 IC=1, NC
          PPP = REAL(PIQ(IC))
          EPPP = EXP(-PPP)
          SINW = 2.0*SIN(0.5*WC(IC))
C
          IF(AGTE.EQ.0.0) THEN
           SINWE = 1.0
          ELSE IF(SINW.GT.0.0) THEN
           SINWE = SINW**AGTE
          ELSE
           SINWE = 0.0
          ENDIF
C
          QCIR(IC) = 2.0*COS(0.5*WC(IC) - ALFCIR)*SINWE * EPPP
          QC_A(IC) = 2.0*SIN(0.5*WC(IC) - ALFCIR)*SINWE * EPPP
C
          XCIR(IC) = REAL(ZC(IC))
          YCIR(IC) = IMAG(ZC(IC))
          SCIR(IC) = SC(IC)
   10   CONTINUE
C
C------ integrate compressible  Cp dz  to get complex force  CL + iCD
        IC = 1
        CPINC1 = 1.0 - (QCIR(IC)/QINF)**2
        CPI_Q1 =   -2.0*QCIR(IC)/QINF**2
        CPCOM1 = CPINC1 / (BETA + BFAC*CPINC1)
        CPC_Q1 = (1.0 - BFAC*CPCOM1)/(BETA + BFAC*CPINC1) * CPI_Q1
        CPC_A1 = CPC_Q1*QC_A(IC)
        DO 20 IC=1, NC
          ICP = IC+1
          IF(IC.EQ.NC) ICP = 1
C
          CPINC2 = 1.0 - (QCIR(ICP)/QINF)**2
          CPI_Q2 =   -2.0*QCIR(ICP)/QINF**2
          CPCOM2 = CPINC2 / (BETA + BFAC*CPINC2)
          CPC_Q2 = (1.0 - BFAC*CPCOM2)/(BETA + BFAC*CPINC2) * CPI_Q2
          CPC_A2 = CPC_Q2*QC_A(ICP)
C
          ZA = (ZC(ICP) + ZC(IC))*0.5 - (0.25,0.0)
          DZ =  ZC(ICP) - ZC(IC)
C
          CMT   = CMT   - 0.5*(CPCOM1 + CPCOM2)*DZ*CONJG(ZA)
     &                  +     (CPCOM1 - CPCOM2)*DZ*CONJG(DZ)/12.0
          CFT   = CFT   + 0.5*(CPCOM1 + CPCOM2)*DZ
          CFT_A = CFT_A + 0.5*(CPC_A1 + CPC_A2)*DZ
C
          CPCOM1 = CPCOM2
          CPC_A1 = CPC_A2
   20   CONTINUE
C
C------ rotate force vector into freestream coordinates
        EIA = EXP(CMPLX(0.0,-ALFA))
        CFT   = CFT  *EIA
        CFT_A = CFT_A*EIA + CFT*(0.0,-1.0)
C
C------ lift is real part of complex force vector
        CLT   = REAL(CFT)
        CLT_A = REAL(CFT_A)
C
C------ moment is real part of complex moment
        CM = REAL(CMT)
C
        IF(ISPEC.EQ.1) THEN
C------- if alpha is prescribed, we're done
         CL = CLT
         RETURN
        ELSE
C------- adjust alpha with Newton-Raphson to get specified CL
         DALFA = (CL - CLT)/CLT_A
         ALFA = ALFA + DALFA
         IF(ABS(DALFA) .LT. AEPS) RETURN
        ENDIF
C
    1 CONTINUE
      WRITE(*,*) 'QCCALC: CL convergence failed.  dAlpha =', DALFA
C
      RETURN
      END SUBROUTINE QCCALC



      SUBROUTINE QSPINT(ALQSP,QSPEC,QINF,MINF,CLQSP,CMQSP)
C--------------------------------------------
C     Integrates circle-plane array surface 
C     pressures to get CL and CM
C--------------------------------------------
      USE CMOD
      DIMENSION QSPEC(NC)
      REAL MINF
C
      SA = SIN(ALQSP)
      CA = COS(ALQSP)
C
      BETA = SQRT(1.0 - MINF**2)
      BFAC = 0.5*MINF**2 / (1.0 + BETA)
C
      CLQSP = 0.0
      CMQSP = 0.0
C
      I = 1
      CQINC = 1.0 - (QSPEC(I)/QINF)**2
      CPQ1 = CQINC / (BETA + BFAC*CQINC)
C
      DO 10 I=1, NC
        IP = I+1
        IF(I.EQ.NC) IP = 1
C
        CQINC = 1.0 - (QSPEC(IP)/QINF)**2
        CPQ2 = CQINC / (BETA + BFAC*CQINC)
C
        DX = (XCOLD(IP) - XCOLD(I))*CA + (YCOLD(IP) - YCOLD(I))*SA
        DY = (YCOLD(IP) - YCOLD(I))*CA - (XCOLD(IP) - XCOLD(I))*SA
        DU = CPQ2 - CPQ1
C
        AX = 0.5*(XCOLD(IP)+XCOLD(I))*CA + 0.5*(YCOLD(IP)+YCOLD(I))*SA
        AY = 0.5*(YCOLD(IP)+YCOLD(I))*CA - 0.5*(XCOLD(IP)+XCOLD(I))*SA
        AQ = 0.5*(CPQ2 + CPQ1)
C
        CLQSP = CLQSP + DX* AQ
        CMQSP = CMQSP - DX*(AQ*(AX-0.25) + DU*DX/12.0)
     &                - DY*(AQ* AY       + DU*DY/12.0)
C
        CPQ1 = CPQ2
   10 CONTINUE
C
      RETURN
      END SUBROUTINE QSPINT


      SUBROUTINE FTP
C----------------------------------------------------------------
C     Slow-Fourier-Transform P(w) using Trapezoidal integration.
C----------------------------------------------------------------
      USE CMOD
      COMPLEX ZSUM
C
      DO 200 M=0, MC
        ZSUM = (0.0,0.0)
        DO 210 IC=2, NC-1
          ZSUM = ZSUM + PIQ(IC)*EIW(IC,M)
  210   CONTINUE
        CN(M) = (0.5*(PIQ(1)*EIW(1,M) + PIQ(NC)*EIW(NC,M))
     &           + ZSUM)*DWC / PI
  200 CONTINUE
      CN(0) = 0.5*CN(0)
C
      RETURN
      END SUBROUTINE FTP


      SUBROUTINE EIWSET(NC1)
C----------------------------------------------------
C     Calculates the uniformly-spaced circle-plane
C     coordinate array WC (omega), and the
C     corresponding complex unit numbers exp(inw)
C     for Slow Fourier Transform operations.
C----------------------------------------------------
      USE CMOD
C
      PI = 4.0*ATAN(1.0)
C
C---- set requested number of points in circle plane
      NC  = NC1
      MC  = NC1/4
      MCT = NC1/16
C
      IF(NC.GT.ICX) STOP 'EIWSET: Array overflow. Increase ICX.'
C
      DWC = 2.0*PI / FLOAT(NC-1)
C
      DO 10 IC=1, NC
        WC(IC) = DWC*FLOAT(IC-1)
   10 CONTINUE
C
C---- set  m = 0  numbers
      DO 20 IC=1, NC
        EIW(IC,0) = (1.0, 0.0)
   20 CONTINUE
C
C---- set  m = 1  numbers
      DO 30 IC=1, NC
        EIW(IC,1) = EXP( CMPLX( 0.0 , WC(IC) ) )
   30 CONTINUE
C
C---- set  m > 1  numbers by indexing appropriately from  m = 1  numbers
      DO 40 M=2, MC
        DO 410 IC=1, NC
          IC1 = M*(IC-1)
          IC1 = MOD( IC1 , (NC-1) ) + 1
          EIW(IC,M) = EIW(IC1,1)
  410   CONTINUE
   40 CONTINUE
C
      RETURN
      END SUBROUTINE EIWSET



      SUBROUTINE PERT(QSPEC)
C--------------------------------------------------------
C     Calculates the perturbed geometry resulting from
C     one Cn mapping coefficient being perturbed by user.
C--------------------------------------------------------
      USE CMOD
      DIMENSION QSPEC(ICX)
C
      COMPLEX QQ(IMX/4,IMX/4),DCN(IMX/4)
C
C---- calculate mapping coefficients for initial airfoil shape
      CALL CNCALC(QSPEC,.FALSE.)
C
C---- preset rotation offset of airfoil so that initial angle is close
C-    to the old airfoil's angle
      DX = XCOLD(2) - XCOLD(1)
      DY = YCOLD(2) - YCOLD(1)
      QIM0 = ATAN2( DX , -DY )  +  0.5*PI*(1.0+AGTE)
      QIMOFF = QIM0 - IMAG(CN(0))
      CN(0) = CN(0) + CMPLX( 0.0 , QIMOFF )
C
C       WRITE(*,*) 
C       WRITE(*,*) 'Current mapping coefficients...'
C       WRITE(*,*) '      n    Re(Cn)            Im(Cn)'
C       DO M = 1, NC
C c     DO M = 1, MIN(NC,32)
C         WRITE(*,1010) M, REAL(CN(M)), IMAG(CN(M))
C  1010   FORMAT(4X,I4, 2F18.12)
C       ENDDO
C C
C  10   WRITE(*,1050)
C  1050 FORMAT(/4X,'Enter  n, delta(Cnr), delta(Cni):  ', $)
C       READ(*,*,ERR=10) M, DCNR, DCNI
C       IF(M.LE.0) THEN
C        GO TO 10
C       ELSEIF(M.GT.NC) THEN
C        WRITE(*,*) 'Max number of modes is', NC
C        GO TO 10
C       ENDIF
C       CN(M) = CN(M) + CMPLX( DCNR , DCNI )
C
C---- inverse-transform and calculate geometry
      CALL PIQSUM
      CALL ZCCALC(MCT)
C
C---- normalize chord and set exact previous alpha
      CALL ZCNORM(MCT)
C
CCC---- put back rotation offset so speed routine QCCALC gets the right alpha
CCC      CN(0) = CN(0) - CMPLX( 0.0 , QIMOFF )

C---- enforce Lighthill's first constraint
      CN(0) = CMPLX( 0.0, IMAG(CN(0)) )

C---- number of free coefficients
      NCN = 1

C---- Newton iteration loop for modified Cn's
      DO 100 ITERCN=1, 10

C------ fix TE gap
        M = 1
        DCN(M) = ZC(1) - ZC(NC)  -  DZTE
        DO L=1, NCN
          QQ(M,L) = ZC_CN(1,L) - ZC_CN(NC,L)
        ENDDO
C
        CALL CGAUSS(IMX/4,NCN,QQ,DCN,1)
C
        DCNMAX = 0.
        DO M=1, NCN
          CN(M) = CN(M) - DCN(M)
          DCNMAX = MAX( ABS(DCN(M)) , DCNMAX )
        ENDDO
C
        CALL PIQSUM
C
        CALL ZCCALC(MCT)
        CALL ZCNORM(MCT)
C
        WRITE(*,*) ITERCN, DCNMAX
        IF(DCNMAX.LE.5.0E-5) GO TO 101
 100  CONTINUE
      WRITE(*,*) 'TE gap,chord did not converge'
 101  CONTINUE
      RETURN
      END SUBROUTINE PERT



      SUBROUTINE CNDUMP(LU)
C--------------------------------------------------------
C     Writes out the Fourier coefficients Cn
C--------------------------------------------------------
      USE CMOD
C
      do m=0, mc
        write(LU,7000) m, real(cn(m)), imag(cn(m))
     &                  , real(piq(m+1)), imag(piq(m+1))
      end do
C
      do m=mc+1, nc-1
        write(LU,7000) m, 0.0, 0.0
     &                  , real(piq(m+1)), imag(piq(m+1))
      end do
c
 7000 format(1x,i3,4f11.6)
c
      RETURN
      END SUBROUTINE CNDUMP


      SUBROUTINE GETVOV(KQSP,FNAME1)
C---- Read Qspec vs S from file
      USE XMOD
      CHARACTER*(*) FNAME1
CLED ENTIRE ROUTINE
C
      KK = 0
      DO I=1, IQX
        W1(I) = 0.
        W2(I) = 0.
        W3(I) = 0.
      END DO
C
      FNAME = FNAME1
C
      LU = 2
      OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=98)
C
C---- read the Qspec file
      DO 10 K=1, IQX
        READ(LU,*,END=11,ERR=99) W1(K), W2(K)
   10 CONTINUE
   11 KK = K-1
      CLOSE(LU)
C
C---- check for input data S,Q or X,Q
      IF(W1(1).EQ.0.0) THEN
C--- input is S vs Qspec (upper TE to LE to lower TE)
       WRITE(*,*) 'Input Qspec vs S file found'
      ELSE
C--- input is X vs Qspec (upper TE to LE to lower TE)
       WRITE(*,*) 'Input Qspec vs X file found'
C    find S(X) from current airfoil
       DO K = 1, KK
         XX = W1(K)
         SS = SLE - XX
         IF(W2(K).GT.0.0) THEN
          SS = SLE - XX
          ELSE
          SS = SLE + XX
         ENDIF
         CALL SINVRT(SS,XX,X,XP,S,N)
         SS = MIN(SS,S(N))
         W1(K) = SS
         write(*,*) 'k,x,s,q ',k,xx,w1(k),w2(k)
       END DO
      ENDIF
C
C---- nondimensionalize S distances
      SSPAN = W1(KK) - W1(1)
      SSTART = W1(1)
      DO 15 K=1, KK
        W1(K) = 1. - (W1(K) - SSTART) / SSPAN
        write(*,*) 'k,s,q ',k,w1(k),w2(k)
   15 CONTINUE
C
C---- sort input points then, removing identical pairs
      CALL SORT(KK,W1,W2)
C---- spline input points
      CALL SPLIND(W2,W3,W1,KK,-999.0,-999.0)
C
C---- set Qspec array
      DO 20 I=1, NSP
        SS = SSPEC(I)
CC------ evaluate spline at SSPEC positions
        QSNEW = SEVAL(SS,W2,W3,W1,KK)
CC------ set incompressible speed from new compressible speed
        QSPEC(I,KQSP) = QINCOM(QSNEW,QINF,TKLAM)
   20 CONTINUE
C
C---- spline new Qspec array
      CALL SPLQSP(KQSP)
      RETURN
C
   98 WRITE(*,*) 'GETVOV: File OPEN error.'
      RETURN
C
   99 WRITE(*,*) 'GETVOV: File READ error.'
      CLOSE(LU)
      RETURN
C
      END SUBROUTINE GETVOV


      SUBROUTINE PUTVOV(KQSP,FNAME1)
C---- write S,Qspec for flow condition KQSP to file
      USE XMOD
      CHARACTER*(*) FNAME1
C
      FNAME = FNAME1
C
      LU = 2
      OPEN(LU,FILE=FNAME,STATUS='UNKNOWN',ERR=98)
C
C---- write the Qspec file
C     S,Qspec ordered from lower TE (at SSPEC(1)=0 upper TE at SSPEC(N)=1
      DO 10 I=NSP, 1, -1
        WRITE(LU,*,ERR=99) 1.0-SSPEC(I), QSPEC(I,KQSP)
   10 CONTINUE
      CLOSE(LU)
C
      RETURN
C
   98 WRITE(*,*) 'PUTVOV: File OPEN error.'
      RETURN
C
   99 WRITE(*,*) 'PUTVOV: File WRITE error.'
      CLOSE(LU)
      RETURN
C
      END SUBROUTINE PUTVOV



      SUBROUTINE ZLEFIND(ZLE,ZC,WC,NC,PIQ,AGTE)
      COMPLEX ZLE, ZC(*), PIQ(*)
      DIMENSION WC(*)
C
      COMPLEX DZDW1, DZDW2, ZTE
C
C---- temporary work arrays for splining near leading edge
      PARAMETER (NTX=33)
      DIMENSION XC(NTX),YC(NTX), XCW(NTX),YCW(NTX)
C
      DATA  PI /3.1415926535897932384/
C
      ZTE = 0.5*(ZC(1)+ZC(NC))
C
C---- find point farthest from TE
      DMAX = 0.0
      DO 35 IC = 1, NC
        DIST = ABS( ZC(IC) - ZTE )
C
        IF(DIST.GT.DMAX) THEN
         DMAX = DIST
         ICLE = IC
        ENDIF
   35 CONTINUE
C
C---- set restricted spline limits around leading edge
      IC1 = MAX( ICLE - (NTX-1)/2 ,  1 )
      IC2 = MIN( ICLE + (NTX-1)/2 , NC )
C
C---- set up derivatives at spline endpoints
      SINW = 2.0*SIN(0.5*WC(IC1))
      SINWE = SINW**(1.0-AGTE)
      HWC = 0.5*(WC(IC1)-PI)*(1.0+AGTE) - 0.5*PI
      DZDW1 = SINWE * EXP( PIQ(IC1) + CMPLX(0.0,HWC) )
C
      SINW = 2.0*SIN(0.5*WC(IC2))
      SINWE = SINW**(1.0-AGTE)
      HWC = 0.5*(WC(IC2)-PI)*(1.0+AGTE) - 0.5*PI
      DZDW2 = SINWE * EXP( PIQ(IC2) + CMPLX(0.0,HWC) )
C
C---- fill temporary x,y coordinate arrays
      DO 45 IC=IC1, IC2
        I = IC-IC1+1
        XC(I) = REAL(ZC(IC))
        YC(I) = IMAG(ZC(IC))
   45 CONTINUE
C
C---- calculate spline near leading edge with derivative end conditions
      NIC = IC2 - IC1 + 1
      CALL SPLIND(XC,XCW,WC(IC1),NIC,REAL(DZDW1),REAL(DZDW2))
      CALL SPLIND(YC,YCW,WC(IC1),NIC,IMAG(DZDW1),IMAG(DZDW2))
C
      XCTE = 0.5*REAL(ZC(1) + ZC(NC))
      YCTE = 0.5*IMAG(ZC(1) + ZC(NC))
C
C---- initial guess for leading edge coordinate
      WCLE = WC(ICLE)
C
C---- Newton loop for improved leading edge coordinate
      DO 50 ITCLE=1, 10
        XCLE = SEVAL(WCLE,XC,XCW,WC(IC1),NIC)
        YCLE = SEVAL(WCLE,YC,YCW,WC(IC1),NIC)
        DXDW = DEVAL(WCLE,XC,XCW,WC(IC1),NIC)
        DYDW = DEVAL(WCLE,YC,YCW,WC(IC1),NIC)
        DXDD = D2VAL(WCLE,XC,XCW,WC(IC1),NIC)
        DYDD = D2VAL(WCLE,YC,YCW,WC(IC1),NIC)
C
        XCHORD = XCLE - XCTE
        YCHORD = YCLE - YCTE
C
C------ drive dot product between chord line and LE tangent to zero
        RES  = XCHORD*DXDW + YCHORD*DYDW
        RESW = DXDW  *DXDW + DYDW  *DYDW
     &       + XCHORD*DXDD + YCHORD*DYDD
C
        DWCLE = -RES/RESW
        WCLE = WCLE + DWCLE
C
        IF(ABS(DWCLE).LT.1.0E-5) GO TO 51
   50 CONTINUE
      WRITE(*,*) 'ZLEFIND: LE location failed.'
      WCLE = WC(ICLE)
   51 CONTINUE
C
C---- set final leading edge point complex coordinate
      XCLE = SEVAL(WCLE,XC,XCW,WC(IC1),NIC)
      YCLE = SEVAL(WCLE,YC,YCW,WC(IC1),NIC)
      ZLE = CMPLX(XCLE,YCLE)
C
      RETURN
      END SUBROUTINE ZLEFIND

