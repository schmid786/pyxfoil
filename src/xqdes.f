C***********************************************************************
C    Module:  xqdes.f
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
C       SUBROUTINE QDES
C C------------------------------------------------------
C C     Mixed-Inverse design routine. Based on the 
C C     same panel formulation as basic analysis method.
C C------------------------------------------------------
C       USE XMOD
C       CHARACTER*4 COMAND, COMOLD
C C
C       CHARACTER*128 COMARG, ARGOLD
C C
C       DIMENSION IINPUT(20)
C       DIMENSION RINPUT(20)
C       LOGICAL ERROR
C C
C       SAVE COMOLD, ARGOLD
C C
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
C C---- make sure a current solution exists
C       CALL SPECAL
C C
C C---- see if current Qspec, if any, didn't come from Full-Inverse
C       IF(NSP.NE.N) THEN
C         LQSPEC = .FALSE.
C         LIQSET = .FALSE.
C       ENDIF
C C
C C---- set alpha, etc corresponding to Q
C       ALGAM = ALFA
C       CLGAM = CL
C       CMGAM = CM
C C
C C---- set "old" speed distribution Q, arc length, and x/c,y/c arrays
C       NSP = N
C       DO I=1, NSP
C         QGAMM(I) = GAM(I)
C         SSPEC(I) = S(I)/S(N)
C       ENDDO
C C
C       WRITE(*,1150) ALGAM/DTOR, CLGAM
C  1150 FORMAT(/' Current Q operating condition:'
C      &       /' alpha = ', F8.3, ' deg.      CL = ', F8.4 / )
C C
C       IF(.NOT.LQSPEC) THEN
C C----- initialize Qspec to "old" solution and notify user
C        NQSP = 1
C        KQTARG = 1
C        CALL GAMQSP(1)
C        WRITE(*,1155)
C        LQSPEC = .TRUE.
C       ENDIF
C C
C C
C C====================================================
C C---- start of menu loop
C  500  CONTINUE
C       COMOLD = COMAND
C       ARGOLD = COMARG
C C
C  501  CALL ASKC('.QDES^',COMAND,COMARG)
C C
C C--------------------------------------------------------
C C---- process previous command ?
C       IF(COMAND(1:1).EQ.'!') THEN
C         IF(COMOLD.EQ.'****') THEN
C           WRITE(*,*) 'Previous .QDES command not valid'
C           GO TO 501
C         ELSE
C           COMAND = COMOLD
C           COMARG = ARGOLD
C         ENDIF
C       ENDIF
C C
C       IF(COMAND.EQ.'    ') THEN
C C----- just <return> was typed... exit QDES
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
C      &  /'   <cr>   Return to Top Level'
C      & //'   QSET   Reset Qspec <== Q'
C      &  /'   SMOO   Smooth Qspec inside target segment'
C      &  /'   SLOP   Toggle modified-Qspec slope matching flag'
C      & //'   eXec i Execute mixed-inverse calculation'
C      &  /'   REST   Restore geometry from buffer airfoil'
C      &  /'   CPXX   CPxx endpoint constraint toggle')
C C
C C--------------------------------------------------------
C C---- re-initialize Qspec to Q
C       ELSEIF(COMAND.EQ.'QSET') THEN
C        CALL GAMQSP(1)
C       GO TO 500
C C
C C--------------------------------------------------------
C C---- smooth Qspec within target segment, or entire Qspec if not marked off
C       ELSEIF(COMAND.EQ.'SMOO') THEN
C C
C        KQSP = 1
C        CALL SMOOQ(IQ1,IQ2,KQSP)
C        CALL SPLQSP(KQSP)
C C
C        CALL CLCALC(N,X,Y,QSPEC(1,KQSP),W1,ALFA,MINF,QINF, XCMREF,YCMREF, 
C      &             CLQSP(KQSP),CMQSP(KQSP),CDPQ, CLQ_ALF,CLQ_MSQ)
C         WRITE(*,1200) CL,CM,CLQSP(KQSP),CMQSP(KQSP)
C       GO TO 500
C C
C C--------------------------------------------------------
C C---- toggle Qspec endpoint slope matching
C       ELSEIF(COMAND.EQ.'SLOP') THEN
C        LQSLOP = .NOT.LQSLOP
C        IF(LQSLOP) THEN
C          WRITE(*,*)
C      &    'Modified Qspec piece will be made tangent at endpoints'
C        ELSE
C          WRITE(*,*)
C      &   'Modified Qspec piece will not be made tangent at endpoints'
C        ENDIF
C       GO TO 500
C C
C C--------------------------------------------------------
C C---- toggle CPxx preservation constraints
C       ELSEIF(COMAND.EQ.'CPXX') THEN
C        LCPXX = .NOT.LCPXX
C        IF(LCPXX) THEN
C         WRITE(*,*) 'CPxx will be constrained'
C        ELSE
C         WRITE(*,*) 'CPxx will not be constrained'
C        ENDIF
C       GO TO 500
C C
C C--------------------------------------------------------
C C---- set up for mixed-inverse calculation
C       ELSEIF(COMAND.EQ.'EXEC' .OR.
C      &       COMAND.EQ.'X   '      ) THEN
C        IF(.NOT.LIQSET) THEN
C         WRITE(*,*) '***  Must mark off target segment first  ***'
C         GO TO 500
C        ENDIF
C C
C C---- check if target segment includes stagnation point
C        IST = 0
C        DO I=IQ1, IQ2-1
C          IF(QGAMM(I).GE.0.0 .AND. QGAMM(I+1).LT.0.0) IST = I
C        ENDDO
C C
C        IF(IST.NE.0) THEN 
C         WRITE(*,*)
C         WRITE(*,*) 'Target segment cannot include ',
C      &             'stagnation point in mixed-inverse.'
C         GO TO 500
C        ENDIF
C C
C        KQSP = 1
C        CLSPEC = CLQSP(KQSP)
C CCC      CALL ASKR('Enter specified CL^',CLSPEC)
C C
C C----- save current coordinates for restoration if requested
C        DO I=1, N
C          XB(I) = X(I)
C          YB(I) = Y(I)
C          SB(I) = S(I)
C          XBP(I) = XP(I)
C          YBP(I) = YP(I)
C        ENDDO
C        NB = N
C        LGSAME = .TRUE.
C C
C        WRITE(*,*)
C        WRITE(*,*) 'Current airfoil saved in buffer airfoil'
C C
C C----- execute mixed-inverse calculation
C        IF(NINPUT.GE.1) THEN
C         NITERQ = IINPUT(1)
C        ELSE
C         CALL ASKI('Enter max number of iterations^',NITERQ)
C        ENDIF
C C
C        CALL MIXED(KQSP,NITERQ)
C        ADEG = ALFA/DTOR
C C
C C----- spline new airfoil shape
C        CALL SCALC(X,Y,S,N)
C        CALL SPLIND(X,XP,S,N,-999.0,-999.0)
C        CALL SPLIND(Y,YP,S,N,-999.0,-999.0)
C        CALL NCALC(X,Y,S,N,NX,NY)
C        CALL LEFIND(SLE,X,XP,Y,YP,S,N)
C        XLE = SEVAL(SLE,X,XP,S,N)
C        YLE = SEVAL(SLE,Y,YP,S,N)
C        CHORD  = SQRT( (0.5*(X(1)+X(N)) - XLE)**2
C      &              + (0.5*(Y(1)+Y(N)) - YLE)**2 )
C        CALL TECALC
C        CALL APCALC
C C
C        ALGAM = ALFA
C C
C        NSP = N
C        DO I=1, N
C          QGAMM(I) = GAM(I)
C          SSPEC(I) = S(I)/S(N)
C        ENDDO
C C
C C----- set inviscid surface speeds and calculate compressible Cp
C        DO I=1, N
C          QINV(I) = GAM(I)
C        ENDDO
C        CALL CPCALC(N,QINV,QINF,MINF,CPI)
C C
C C----- influence coefficients & other stuff is no longer valid for new airfoil
C        LGAMU = .FALSE.
C        LWAKE = .FALSE.
C        LQAIJ = .FALSE.
C        LADIJ = .FALSE.
C        LWDIJ = .FALSE.
C        LIPAN = .FALSE.
C        LVCONV = .FALSE.
C        LSCINI = .FALSE.
C CCC      LBLINI = .FALSE.
C        LGSAME = .FALSE.
C C
C cc       CALL NAMMOD(NAME,1,1)
C cc       CALL STRIP(NAME,NNAME)
C C
C C--------------------------------------------------------
C C---- restore and spline old airfoil
C       ELSEIF(COMAND.EQ.'REST') THEN
C        DO I=1, N
C          X(I) = XB(I)
C          Y(I) = YB(I)
C        ENDDO
C        CALL SCALC(X,Y,S,N)
C        CALL SPLIND(X,XP,S,N,-999.0,-999.0)
C        CALL SPLIND(Y,YP,S,N,-999.0,-999.0)
C        CALL NCALC(X,Y,S,N,NX,NY)
C        CALL LEFIND(SLE,X,XP,Y,YP,S,N)
C        XLE = SEVAL(SLE,X,XP,S,N)
C        YLE = SEVAL(SLE,Y,YP,S,N)
C        CHORD  = SQRT( (0.5*(X(1)+X(N)) - XLE)**2
C      &              + (0.5*(Y(1)+Y(N)) - YLE)**2 )
C        CALL TECALC
C        CALL APCALC
C        LGAMU = .FALSE.
C        LGSAME = .TRUE.
C C
C cc       CALL NAMMOD(NAME,-1,1)
C cc       CALL STRIP(NAME,NNAME)
C C
C C--------------------------------------------------------
C       ELSE
C        WRITE(*,1100) COMAND
C  1100  FORMAT(' Command ',A4,' not recognized.  Type a " ? " for list.')
C C
C        COMAND = '****'
C       ENDIF
C C
C       GO TO 500
C C
C C....................................................
C C
C  1155 FORMAT(/' Qspec initialized to current Q.'/ )
C  1200 FORMAT(/' Q    :   CL =',F11.6, '    CM =',F11.6
C      &       /' Qspec:   CL =',F11.6, '    CM =',F11.6 )
C       END QDES


      SUBROUTINE SPLQSP(KQSP)
C------------------------------------------------------
C     Splines Qspec(s).  The end intervals are treated
C     specially to avoid Gibbs-type problems from 
C     blindly splining to the stagnation point.
C------------------------------------------------------
      USE XMOD
C
C---- usual spline with natural end BCs
      CALL SPLIND(QSPEC(2,KQSP),QSPECP(2,KQSP),SSPEC(2),NSP-2,
     &            -999.0,-999.0)
C
ccC---- pseudo-monotonic spline with simple secant slope calculation
cc      CALL SPLINA(QSPEC(2,KQSP),QSPECP(2,KQSP),SSPEC(2),NSP-2)
C
C---- end intervals are splined separately with natural BCs at
C     the trailing edge and matching slopes at the interior points
C
      I = 1
      CALL SPLIND(QSPEC(I,KQSP),QSPECP(I,KQSP),SSPEC(I),2,
     &            -999.0,QSPECP(I+1,KQSP))
C
      I = NSP-1
      CALL SPLIND(QSPEC(I,KQSP),QSPECP(I,KQSP),SSPEC(I),2,
     &            QSPECP(I,KQSP),-999.0)
C
      RETURN
      END SUBROUTINE SPLQSP


      SUBROUTINE SMOOQ(KQ1,KQ2,KQSP)
C--------------------------------------------
C     Smooths Qspec(s) inside target segment
C--------------------------------------------
      USE XMOD
C------ mixed inverse: use arc length coordinate
        DO 15 I=1, NSP
          W8(I) = SSPEC(I)
   15   CONTINUE
C
ccc      ENDIF
C
C
      IF(KQ2-KQ1 .LT. 2) THEN
       WRITE(*,*) 'Segment is too short.  No smoothing possible.'
       RETURN
      ENDIF
C
C---- set smoothing length ( ~ distance over which data is smeared )
      SMOOL = 0.002*(W8(NSP) - W8(1))
CCC   CALL ASKR('Enter Qspec smoothing length^',SMOOL)
C
C---- set up tri-diagonal system for smoothed Qspec
      SMOOSQ = SMOOL**2
      DO 20 I=KQ1+1, KQ2-1
        DSM = W8(I  ) - W8(I-1)
        DSP = W8(I+1) - W8(I  )
        DSO = 0.5*(W8(I+1) - W8(I-1))
C
        W1(I) =  SMOOSQ * (         - 1.0/DSM) / DSO
        W2(I) =  SMOOSQ * ( 1.0/DSP + 1.0/DSM) / DSO  +  1.0
        W3(I) =  SMOOSQ * (-1.0/DSP          ) / DSO
   20 CONTINUE
C
C---- set fixed-Qspec end conditions
      W2(KQ1) = 1.0
      W3(KQ1) = 0.0
C
      W1(KQ2) = 0.0
      W2(KQ2) = 1.0
C
      IF(LQSLOP) THEN
C----- also enforce slope matching at endpoints
       I = KQ1 + 1
       DSM = W8(I  ) - W8(I-1)
       DSP = W8(I+1) - W8(I  )
       DS  = W8(I+1) - W8(I-1)
       W1(I) = -1.0/DSM - (DSM/DS)/DSM
       W2(I) =  1.0/DSM + (DSM/DS)/DSM + (DSM/DS)/DSP
       W3(I) =                         - (DSM/DS)/DSP
       QSPP1 = W1(I)*QSPEC(I-1,KQSP)
     &       + W2(I)*QSPEC(I  ,KQSP)
     &       + W3(I)*QSPEC(I+1,KQSP)
C
       I = KQ2 - 1
       DSM = W8(I  ) - W8(I-1)
       DSP = W8(I+1) - W8(I  )
       DS  = W8(I+1) - W8(I-1)
       W1(I) =                           (DSP/DS)/DSM
       W2(I) = -1.0/DSP - (DSP/DS)/DSP - (DSP/DS)/DSM
       W3(I) =  1.0/DSP + (DSP/DS)/DSP
       QSPP2 = W1(I)*QSPEC(I-1,KQSP)
     &       + W2(I)*QSPEC(I  ,KQSP)
     &       + W3(I)*QSPEC(I+1,KQSP)
C
       QSPEC(KQ1+1,KQSP) = QSPP1
       QSPEC(KQ2-1,KQSP) = QSPP2
      ENDIF
C
C
C---- solve for smoothed Qspec array
      CALL TRISOL(W2(KQ1),W1(KQ1),W3(KQ1),QSPEC(KQ1,KQSP),(KQ2-KQ1+1))
C
C
cc      IF(LQSYM) THEN
cc        DO 40 I=KQ1+1, KQ2-1
cc          QSPEC(NSP-I+1,KQSP) = -QSPEC(I,KQSP)
cc 40     CONTINUE
cc      ENDIF
C
      RETURN
      END SUBROUTINE SMOOQ
 

      FUNCTION QINCOM(QC,QINF,TKLAM)
C-------------------------------------
C     Sets incompressible speed from
C     Karman-Tsien compressible speed
C-------------------------------------
C
      IF(TKLAM.LT.1.0E-4 .OR. ABS(QC).LT.1.0E-4) THEN
C----- for nearly incompressible case or very small speed, use asymptotic
C      expansion of singular quadratic formula to avoid numerical problems
       QINCOM = QC/(1.0 - TKLAM)
      ELSE
C----- use quadratic formula for typical case
       TMP = 0.5*(1.0 - TKLAM)*QINF/(QC*TKLAM)
       QINCOM = QINF*TMP*(SQRT(1.0 + 1.0/(TKLAM*TMP**2)) - 1.0)
      ENDIF
      RETURN
      END FUNCTION QINCOM
 
 
      SUBROUTINE GAMQSP(KQSP)
C------------------------------------------------
C     Sets Qspec(s,k) from current speed Q(s).
C------------------------------------------------
      USE XMOD
C
      ALQSP(KQSP) = ALGAM
      CLQSP(KQSP) = CLGAM
      CMQSP(KQSP) = CMGAM
C
      DO 10 I=1, NSP
        QSPEC(I,KQSP) = QGAMM(I)
 10   CONTINUE
C
C---- zero out Qspec DOFs
      QDOF0 = 0.0
      QDOF1 = 0.0
      QDOF2 = 0.0
      QDOF3 = 0.0
C
      CALL SPLQSP(KQSP)
C
C---- reset target segment endpoints
      IF(.NOT.LIQSET) THEN
       IQ1 = 1
       IQ2 = NSP
      ENDIF
C
      RETURN
      END SUBROUTINE GAMQSP


      SUBROUTINE SYMQSP(KQSP)
C-----------------------------------------
C     Forces symmetry of Qspec(KQSP) array
C-----------------------------------------
      USE XMOD
C
      ALQSP(KQSP) = 0.
      CLQSP(KQSP) = 0.
      CMQSP(KQSP) = 0.
C
      SSPMID = 0.5*(SSPEC(NSP) - SSPEC(1))
      DO 11 I=1, (NSP+1)/2
        SSPEC(I) = SSPMID + 0.5*(SSPEC(I)      - SSPEC(NSP-I+1)  )
        QSPEC(I,KQSP) =     0.5*(QSPEC(I,KQSP) - QSPEC(NSP-I+1,KQSP))
 11   CONTINUE
C
      DO 15 I=(NSP+1)/2+1, NSP
        SSPEC(I)      = -SSPEC(NSP-I+1)      + 2.0*SSPMID
        QSPEC(I,KQSP) = -QSPEC(NSP-I+1,KQSP)
 15   CONTINUE
C
C---- zero out Qspec DOFs
      QDOF0 = 0.0
      QDOF1 = 0.0
      QDOF2 = 0.0
      QDOF3 = 0.0
C
      CALL SPLQSP(KQSP)
C
      WRITE(*,1001) KQSP
 1001 FORMAT(/' Qspec',I2,'  made symmetric')
C
      RETURN
      END SUBROUTINE SYMQSP



      SUBROUTINE MIXED(KQSP,NITERQ)
C-------------------------------------------------
C     Performs a mixed-inverse calculation using 
C     the specified surface speed array QSPEC.
C-------------------------------------------------
      USE XMOD
C
C---- distance of internal control point ahead of sharp TE
C-    (fraction of smaller panel length adjacent to TE)
      BWT = 0.1
C
      COSA = COS(ALFA)
      SINA = SIN(ALFA)
      CALL SCALC(X,Y,S,N)
C
C---- zero-out and set DOF shape functions
      DO 1 I=1, N
        QF0(I) = 0.0
        QF1(I) = 0.0
        QF2(I) = 0.0
        QF3(I) = 0.0
    1 CONTINUE
C
C---- set DOF shape functions and specified speed
      DO 2 I=IQ1, IQ2
        FS = (S(I)-S(IQ1)) / (S(IQ2)-S(IQ1))
CCC        QF0(I) = (1.0-FS)**2
CCC        QF1(I) = FS**2
        QF0(I) = 1.0 - FS
        QF1(I) = FS
        IF(LCPXX) THEN
         QF2(I) = EXP(-5.0*     FS )
         QF3(I) = EXP(-5.0*(1.0-FS))
        ELSE
         QF2(I) = 0.0
         QF3(I) = 0.0
        ENDIF
        GAM(I) = QSPEC(I,KQSP) + QDOF0*QF0(I) + QDOF1*QF1(I)
     &                         + QDOF2*QF2(I) + QDOF3*QF3(I)
    2 CONTINUE
C
C---- perform Newton iterations on the new geometry
      DO 1000 ITER=1, NITERQ
C
      DO 3 I=1, N+5
        DO 31 J=1, N+5
          Q(I,J) = 0.
   31   CONTINUE
    3 CONTINUE
C
C---- calculate normal direction vectors along which the nodes move
      CALL NCALC(X,Y,S,N,NX,NY)
C
C---- go over all nodes, setting up  Psi = Psi0  equations
      DO 20 I=1, N
        CALL PSILIN(I,X(I),Y(I),NX(I),NY(I),PSI,PSI_N,.TRUE.,.FALSE.)
C
        DZDN(I) = DZDN(I) + PSI_N
C
C------ fill columns for specified geometry location
        DO 201 J=1, IQ1-1
          Q(I,J) = Q(I,J) + DZDG(J)
  201   CONTINUE
C
C------ fill columns for specified surface speed location
        DO 202 J=IQ1, IQ2
          Q(I,J) = Q(I,J) + DZDN(J)
  202   CONTINUE
C
C------ fill columns for specified geometry location
        DO 203 J=IQ2+1, N
          Q(I,J) = Q(I,J) + DZDG(J)
  203   CONTINUE
C
C------ set residual
        DQ(I) = PSIO - PSI
C
C------ fill global unknown columns
        Q(I,N+1) = Q(I,N+1) - 1.0
        Q(I,N+2) = Q(I,N+2) + Z_QDOF0
        Q(I,N+3) = Q(I,N+3) + Z_QDOF1
        Q(I,N+4) = Q(I,N+4) + Z_QDOF2
        Q(I,N+5) = Q(I,N+5) + Z_QDOF3
   20 CONTINUE
C
C---- set up Kutta condition
      DQ(N+1) = -( GAM(1) + GAM(N) )
      CALL GAMLIN(N+1,1,1.0)
      CALL GAMLIN(N+1,N,1.0)
C
      IF(SHARP) THEN
C----- set zero internal velocity in TE corner 
C
C----- set TE bisector angle
       AG1 = ATAN2(-YP(1),-XP(1)    )
       AG2 = ATANC( YP(N), XP(N),AG1)
       ABIS = 0.5*(AG1+AG2)
       CBIS = COS(ABIS)
       SBIS = SIN(ABIS)
C
C----- minimum panel length adjacent to TE
       DS1 = SQRT( (X(1)-X(2)  )**2 + (Y(1)-Y(2)  )**2 )
       DS2 = SQRT( (X(N)-X(N-1))**2 + (Y(N)-Y(N-1))**2 )
       DSMIN = MIN( DS1 , DS2 )
C
C----- control point on bisector just ahead of TE point
       XBIS = XTE - BWT*DSMIN*CBIS
       YBIS = YTE - BWT*DSMIN*SBIS
ccc       write(*,*) xbis, ybis
C
C----- set velocity component along bisector line
       CALL PSILIN(0,XBIS,YBIS,-SBIS,CBIS,PSI,QBIS,.FALSE.,.TRUE.)
C
CCC--- RES = DQDGj*Gamj + DQDMj*Massj + QINF*(COSA*CBIS + SINA*SBIS)
       RES = QBIS
C
       DO J=1, N+5
         Q(N,J) = 0.
       ENDDO
C
C----- dRes/dgamj
       DO J=1, N
         CALL GAMLIN(N,J, DQDG(J) )
         Q(N,J) = DQDG(J)
       ENDDO
C
C----- dRes/dPsio
       Q(N,N+1) = 0.
C
C----- -dRes/dUinf
       DQ(N) = -RES
      ENDIF
C
C---- pinned IQ1 point condition
      Q(N+2,IQ1) = 1.0
      DQ(N+2) = 0.0
C
C---- pinned IQ2 point condition
      Q(N+3,IQ2) = 1.0
      DQ(N+3) = 0.0
C
      IF(IQ1.GT.1 .AND. LCPXX) THEN
C----- speed regularity IQ1 condition
       RES = GAM(IQ1-1)      - 2.0*  GAM(IQ1)      +   GAM(IQ1+1)
     &  - (QSPEC(IQ1-1,KQSP) - 2.0*QSPEC(IQ1,KQSP) + QSPEC(IQ1+1,KQSP) )
       CALL GAMLIN(N+4,IQ1-1, 1.0)
       CALL GAMLIN(N+4,IQ1  ,-2.0)
       CALL GAMLIN(N+4,IQ1+1, 1.0)
       DQ(N+4) = -RES
      ELSE
C----- zero DOF condition
       Q(N+4,N+4) = 1.0
       DQ(N+4) = -QDOF2
      ENDIF
C
      IF(IQ2.LT.N .AND. LCPXX) THEN
C----- speed regularity IQ2 condition
       RES = GAM(IQ2-1)      - 2.0*  GAM(IQ2)      +   GAM(IQ2+1)
     &  - (QSPEC(IQ2-1,KQSP) - 2.0*QSPEC(IQ2,KQSP) + QSPEC(IQ2+1,KQSP) )
       CALL GAMLIN(N+5,IQ2-1, 1.0)
       CALL GAMLIN(N+5,IQ2  ,-2.0)
       CALL GAMLIN(N+5,IQ2+1, 1.0)
       DQ(N+5) = -RES
      ELSE
C----- zero DOF condition
       Q(N+5,N+5) = 1.0
       DQ(N+5) = -QDOF3
      ENDIF
C
      CALL GAUSS(IQX,N+5,Q,DQ,1)
C
      INMAX = 0
      IGMAX = 0
      DNMAX = 0.0
      DGMAX = 0.0
C
C---- update surface speed GAM before target segment
      DO 100 I=1, IQ1-1
        GAM(I) = GAM(I) + DQ(I)
        IF(ABS(DQ(I)) .GT. ABS(DGMAX)) THEN
         DGMAX = DQ(I)
         IGMAX = I
        ENDIF
  100 CONTINUE
C
C---- update panel nodes inside target segment
      DO 110 I=IQ1, IQ2
        X(I) = X(I) + NX(I)*DQ(I)
        Y(I) = Y(I) + NY(I)*DQ(I)
        IF(ABS(DQ(I)) .GT. ABS(DNMAX)) THEN
         DNMAX = DQ(I)
         INMAX = I
        ENDIF
  110 CONTINUE
C
C---- update surface speed GAM after target segment
      DO 120 I=IQ2+1, N
        GAM(I) = GAM(I) + DQ(I)
        IF(ABS(DQ(I)) .GT. ABS(DGMAX)) THEN
         DGMAX = DQ(I)
         IGMAX = I
        ENDIF
  120 CONTINUE
C
C---- update gloabal variables
      PSIO  = PSIO  + DQ(N+1)
      QDOF0 = QDOF0 + DQ(N+2)
      QDOF1 = QDOF1 + DQ(N+3)
      QDOF2 = QDOF2 + DQ(N+4)
      QDOF3 = QDOF3 + DQ(N+5)
C
      COSA = COS(ALFA)
      SINA = SIN(ALFA)
      CALL SCALC(X,Y,S,N)
C
C---- set correct surface speed over target segment including DOF contributions
      DO 140 I=IQ1, IQ2
        GAM(I) = QSPEC(I,KQSP) + QDOF0*QF0(I) + QDOF1*QF1(I)
     &                         + QDOF2*QF2(I) + QDOF3*QF3(I)
  140 CONTINUE
C
C---- update everything else
      CALL TECALC
      CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &            CL,CM,CDP, CL_ALF,CL_MSQ)
      WRITE(*,2000) DNMAX,INMAX,DGMAX,IGMAX,CL
     &             ,DQ(N+2),DQ(N+3)
     &             ,DQ(N+4),DQ(N+5)
 2000 FORMAT(/' dNmax =',E10.3,I4,'   dQmax =',E10.3,I4,'    CL =',F7.4
     &       /' dQf1  =',E10.3,4X,'   dQf2  =',E10.3
     &       /' dQf3  =',E10.3,4X,'   dQf4  =',E10.3)
C
      IF(ABS(DNMAX).LT.5.0E-5 .AND. ABS(DGMAX).LT.5.0E-4) THEN
       WRITE(*,*)
       WRITE(*,*) 'New current airfoil generated'
       WRITE(*,*) 'Old buffer  airfoil unchanged'
       RETURN
      ENDIF
C
 1000 CONTINUE
      WRITE(*,*) 'Not quite converged.  Can EXEC again if necessary.'
      RETURN
C
      END SUBROUTINE MIXED

 
      SUBROUTINE GAMLIN(I,J,COEF)
C-------------------------------------------------------------------
C     Adds on Jacobian entry for point I due to node speed GAM at J.
C     GAM is either a local unknown if outside target segment,
C     or dependent on global Qspec DOF's if inside target segment.
C-------------------------------------------------------------------
      USE XMOD
C
      IF(J.GE.IQ1 .AND. J.LE.IQ2) THEN
C----- inside target segment
       Q(I,N+2) = Q(I,N+2) + COEF*QF0(J)
       Q(I,N+3) = Q(I,N+3) + COEF*QF1(J)
       Q(I,N+4) = Q(I,N+4) + COEF*QF2(J)
       Q(I,N+5) = Q(I,N+5) + COEF*QF3(J)
      ELSE
C----- outside target segment
       Q(I,J) = Q(I,J) + COEF
      ENDIF
      RETURN
      END SUBROUTINE GAMLIN
