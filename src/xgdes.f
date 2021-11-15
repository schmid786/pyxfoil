C***********************************************************************
C    Module:  xgdes.f
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
C       SUBROUTINE GDES
C       USE XMOD
C       CHARACTER*4 COMAND, COMOLD
C       DIMENSION XRF(2)
C C
C       CHARACTER*128 COMARG, ARGOLD
C C
C       REAL ATOL_TMP
C       DIMENSION ATOL_TMP(1)
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
C       ARERO = 0.0
C C
C       IF(NB.EQ.0) THEN
C        WRITE(*,*)
C        WRITE(*,*) '***  No airfoil available  ***'
C        RETURN
C       ENDIF
C C
C       WRITE(*,*)
C       WRITE(*,*) 'You are working with the buffer airfoil'
C C
C C====================================================
C C---- start of menu loop
C  500  CONTINUE
C       COMOLD = COMAND
C       ARGOLD = COMARG
C C
C  501  IF(LGSYM) THEN
C        CALL ASKC('.GDESs^',COMAND,COMARG)
C       ELSE
C        CALL ASKC('.GDES^',COMAND,COMARG)
C       ENDIF
C C
C C--------------------------------------------------------
C C---- process previous command ?
C       IF(COMAND(1:1).EQ.'!') THEN
C         IF(COMOLD.EQ.'****') THEN
C           WRITE(*,*) 'Previous .GDES command not valid'
C           GO TO 501
C         ELSE
C           COMAND = COMOLD
C           COMARG  = ARGOLD
C         ENDIF
C       ENDIF
C C
C       IF(COMAND.EQ.'    ') THEN
C C----- just <return> was typed... exit OPER
C        LGSYM = .FALSE.
C        IF(.NOT.LGSAME) THEN
C         WRITE(*,*)
C         WRITE(*,*) 'Buffer airfoil is not identical to current airfoil'
C        ENDIF
C        RETURN
C       ENDIF
C C
C C---- extract command line numeric arguments
C       DO I=1, 20
C         IINPUT(I) = 0
C         RINPUT(I) = 0.0
C       ENDDO
C       NINPUT = 20
C       CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
C       NINPUT = 20
C       CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C C
C C--------------------------------------------------------
C       IF(COMAND.EQ.'?   ') THEN
C        WRITE(*,1050)
C  1050  FORMAT(
C      & /'   <cr>     Return to Top Level'
C      & /'   !        Redo previous command'
C      &//'   GSET     Set buffer  airfoil <== current airfoil'
C      & /'   eXec     Set current airfoil <== buffer  airfoil'
C      & /'   SYMM     Toggle y-symmetry flag'
C      &//'   ADEG r   Rotate about origin (degrees)'
C      & /'   ARAD r   Rotate about origin (radians)'
C      & /'   Tran rr  Translate'
C      & /'   Scal r   Scale about origin'
C      & /'   LINS rr. Linearly-varying y scale'
C      & /'   DERO     Derotate (set chord line level)'
C      & /'   RERO     Rerotate (set chord line back from DERO)'
C      &//'   TGAP rr  Change trailing edge gap'
C      & /'   LERA rr  Change leading edge radius'
C      & /'   TFAC rr  Scale existing thickness and camber'
C      & /'   TSET rr  Set new thickness and camber'
C      & /'   HIGH rr  Move camber and thickness highpoints'
C      & /'  .CAMB     Modify camber shape directly or via loading'
C      &//'   Flap rrr Deflect trailing edge flap'
C      & /'   SLOP     Toggle modified-contour slope matching flag'
C      &//'   UNIT     Normalize buffer airfoil to unit chord'
C      & /'   CLIS     List curvatures'
C      & /'   CANG     List panel corner angles'
C      & /'   CADD ri. Add points at corners exceeding angle threshold'
C      &//'   NAME s   Specify new airfoil name'
C      & /'   NINC     Increment name version number')
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'GSET') THEN
C        NB = N
C        DO I=1, NB
C          XB(I) = X(I)
C          YB(I) = Y(I)
C        ENDDO
C        LGSAME = .TRUE.
C        CALL SCALC(XB,YB,SB,NB)
C        CALL SEGSPL(XB,XBP,SB,NB)
C        CALL SEGSPL(YB,YBP,SB,NB)
C C
C        CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
C      &             SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
C      &             EI11BA,EI22BA,APX1BA,APX2BA,
C      &             EI11BT,EI22BT,APX1BT,APX2BT,
C      &             THICKB,CAMBRB )
C C
C        IF(LGSYM) CALL ZERCAM
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'EXEC' .OR.
C      &       COMAND.EQ.'X   '      ) THEN
C        CALL ABCOPY
C cc       CALL NAMMOD(NAME,1,1)
C cc       CALL STRIP(NAME,NNAME)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SYMM') THEN
C        LGSYM = .NOT.LGSYM
C        IF(LGSYM) THEN
C         WRITE(*,*) 'y-symmetry forcing enabled.'
C         CALL ZERCAM
C        ELSE
C         WRITE(*,*) 'y-symmetry forcing disabled.'
C        ENDIF
C C
C C=================================================
C C---- rotate airfoil by degrees
C       ELSEIF(COMAND.EQ.'ADEG' .OR.
C      &       COMAND.EQ.'ARAD'     ) THEN
C        IF(COMAND.EQ.'ADEG') THEN
C          IF(NINPUT.GE.1) THEN
C           ADEG = RINPUT(1)
C          ELSE
C           ADEG = 0.0
C           CALL ASKR('Enter angle change (deg)^',ADEG)
C          ENDIF
C          ARAD = ADEG*PI/180.0
C        ELSE
C          IF(NINPUT.GE.1) THEN
C           ARAD = RINPUT(1)
C          ELSE
C           ARAD = 0.0
C           CALL ASKR('Enter angle change (rad)^',ARAD)
C          ENDIF
C        ENDIF
C C
C        CALL ROTATE(XB,YB,NB,ARAD)
C CCC      CALL SCALC(XB,YB,SB,NB)
C        CALL SEGSPL(XB,XBP,SB,NB)
C        CALL SEGSPL(YB,YBP,SB,NB)
C C
C        APX1BA = APX1BA - ARAD
C        APX2BA = APX2BA - ARAD
C        APX1BT = APX1BT - ARAD
C        APX2BT = APX2BT - ARAD
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'TRAN' .OR.
C      &       COMAND.EQ.'T   '      ) THEN
C        IF    (NINPUT.GE.2) THEN
C         DELX = RINPUT(1)
C         DELY = RINPUT(2)
C        ELSEIF(NINPUT.GE.1) THEN
C         DELX = RINPUT(1)
C         DELY = 0.0
C         CALL ASKR('Enter delta(y)^',DELY)
C        ELSE
C         DELX = 0.0
C         CALL ASKR('Enter delta(x)^',DELX)
C         DELY = 0.0
C         CALL ASKR('Enter delta(y)^',DELY)
C        ENDIF
C        DO I=1, NB
C          XB(I) = XB(I) + DELX
C          YB(I) = YB(I) + DELY
C        ENDDO
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SCAL' .OR.
C      &       COMAND.EQ.'S   '      ) THEN
C        IF(NINPUT.GE.1) THEN
C         FAC = RINPUT(1)
C         XXFAC = FAC
C         YYFAC = FAC
C        ELSE
C         FAC = 1.0
C         CALL ASKR('Enter scale factor (0 for separate x,y scales)^',FAC)
C         XXFAC = FAC
C         YYFAC = FAC
C        ENDIF
C C
C        IF(FAC .EQ. 0.0) THEN
C         IF(NINPUT.GE.3) THEN
C          XXFAC = RINPUT(2)
C          YYFAC = RINPUT(3)
C         ELSE
C          XXFAC = 1.0
C          CALL ASKR('Enter x scale factor^',XXFAC)
C          YYFAC = 1.0
C          CALL ASKR('Enter y scale factor^',YYFAC)
C         ENDIF
C        ENDIF
C C
C        DO I=1, NB
C          XB(I) = XB(I)*XXFAC
C          YB(I) = YB(I)*YYFAC
C        ENDDO
C C
C C----- re-order if necessary to maintain counterclockwise ordering 
C        IF(XXFAC*YYFAC .LT. 0.0) THEN
C          DO I=1, NB/2
C            XTMP = XB(I)
C            YTMP = YB(I)
C            XB(I) = XB(NB-I+1)
C            YB(I) = YB(NB-I+1)
C            XB(NB-I+1) = XTMP
C            YB(NB-I+1) = YTMP
C          ENDDO
C        ENDIF
C C
C C----- re-spline new geometry
C        CALL SCALC(XB,YB,SB,NB)
C        CALL SEGSPL(XB,XBP,SB,NB)
C        CALL SEGSPL(YB,YBP,SB,NB)
C C
C        CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
C      &             SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
C      &             EI11BA,EI22BA,APX1BA,APX2BA,
C      &             EI11BT,EI22BT,APX1BT,APX2BT,
C      &             THICKB,CAMBRB )
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'LINS') THEN
C  40    CONTINUE
C        IF(NINPUT.GE.4) THEN
C          XOC1  = RINPUT(1)
C          YFAC1 = RINPUT(2)
C          XOC2  = RINPUT(3)
C          YFAC2 = RINPUT(4)
C        ELSE
C  1001    FORMAT(/1X,A,$)
C  41      WRITE(*,1001) 'Location 1...  enter  x/c, y-scale :  '
C          READ(*,*,ERR=41) XOC1, YFAC1
C  42      WRITE(*,1001) 'Location 2...  enter  x/c, y-scale :  '
C          READ(*,*,ERR=42) XOC2, YFAC2
C        ENDIF
C C
C        IF(ABS(XOC1-XOC2) .LT. 1.0E-5) THEN
C         WRITE(*,*) 'x/c locations 1 and 2 must be different'
C         NINPUT = 0
C         GO TO 40
C        ENDIF
C C
C        CALL LEFIND(SBLE,XB,XBP,YB,YBP,SB,NB)
C        XLE = SEVAL(SBLE,XB,XBP,SB,NB)
C        YLE = SEVAL(SBLE,YB,YBP,SB,NB)
C        XTE = 0.5*(XB(1) + XB(NB))
C        YTE = 0.5*(YB(1) + YB(NB))
C        DO I=1, NB
C          XOC = (XB(I)-XLE) / (XTE-XLE)
C          FR1 = (XOC2-XOC )/(XOC2-XOC1)
C          FR2 = (XOC -XOC1)/(XOC2-XOC1)
C          YYFAC = FR1*YFAC1 + FR2*YFAC2
C          YB(I) = YB(I)*YYFAC
C        ENDDO
C C
C C----- re-spline new geometry
C        CALL SCALC(XB,YB,SB,NB)
C        CALL SEGSPL(XB,XBP,SB,NB)
C        CALL SEGSPL(YB,YBP,SB,NB)
C C
C        CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
C      &             SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
C      &             EI11BA,EI22BA,APX1BA,APX2BA,
C      &             EI11BT,EI22BT,APX1BT,APX2BT,
C      &             THICKB,CAMBRB )
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'DERO') THEN
C        CALL LEFIND(SBLE,XB,XBP,YB,YBP,SB,NB)
C        XLE = SEVAL(SBLE,XB,XBP,SB,NB)
C        YLE = SEVAL(SBLE,YB,YBP,SB,NB)
C        XTE = 0.5*(XB(1) + XB(NB))
C        YTE = 0.5*(YB(1) + YB(NB))
C C
C        ARAD = ATAN2(YTE-YLE,XTE-XLE)
C        CALL ROTATE(XB,YB,NB,ARAD)
C        WRITE(*,1080) ARAD / DTOR
C  1080  FORMAT(/'Rotating buffer airfoil by ',F8.3,' deg.')
C C---- save DERO derotation angle for RERO
C        ARERO = ARAD
C C
C        CALL SCALC(XB,YB,SB,NB)
C        CALL SEGSPL(XB,XBP,SB,NB)
C        CALL SEGSPL(YB,YBP,SB,NB)
C        CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
C      &             SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
C      &             EI11BA,EI22BA,APX1BA,APX2BA,
C      &             EI11BT,EI22BT,APX1BT,APX2BT,
C      &             THICKB,CAMBRB )
C C
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'RERO') THEN
C        IF(ARERO.NE.0.0) THEN
C         CALL ROTATE(XB,YB,NB,-ARERO)
C         WRITE(*,1080) -ARERO / DTOR
C C---- clear DERO derotation angle?
C C       ARERO = 0.0
C C
C         CALL SCALC(XB,YB,SB,NB)
C         CALL SEGSPL(XB,XBP,SB,NB)
C         CALL SEGSPL(YB,YBP,SB,NB)
C         CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
C      &              SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
C      &              EI11BA,EI22BA,APX1BA,APX2BA,
C      &              EI11BT,EI22BT,APX1BT,APX2BT,
C      &              THICKB,CAMBRB )
C        ENDIF
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'TGAP') THEN
C        CALL TGAP(RINPUT,NINPUT) !!! modified
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'LERA') THEN
C        CALL LERAD(RINPUT,NINPUT)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'TFAC') THEN
C        CALL TCSCAL(RINPUT,NINPUT)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'TSET') THEN
C        CALL TCSET(RINPUT,NINPUT)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'HIGH') THEN
C        CALL HIPNT(RINPUT,NINPUT)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CAMB') THEN
C        IF(LGSYM) THEN
C         WRITE(*,*) 'Disabling symmetry enforcement.'
C         LGSYM = .FALSE.
C        ENDIF
C        CALL CAMB
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CANG') THEN
C        CALL CANG(XB,YB,NB,2, IMAX,AMAX)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CADD') THEN
C        CALL CANG(XB,YB,NB,2, IMAX,AMAX)
C        WRITE(*,*)
C C
C        XBMIN = XB(1)
C        XBMAX = XB(1)
C        DO I=1, NB
C          XBMIN = MIN(XBMIN,XB(I))
C          XBMAX = MAX(XBMAX,XB(I))
C        ENDDO
C C
C C----- default inputs
C        ATOL = 0.5*AMAX
C        ISPL = 1
C        XRF(1) = XBMIN - 0.1*(XBMAX-XBMIN)
C        XRF(2) = XBMAX + 0.1*(XBMAX-XBMIN)
C C
C        IF    (NINPUT.LE.0) THEN
C         GO TO 70
C        ELSEIF(NINPUT.LE.1) THEN
C         ATOL = RINPUT(1)
C         GO TO 71
C        ELSEIF(NINPUT.LE.2) THEN
C         ATOL = RINPUT(1)
C         ISPL = IINPUT(2)
C         GO TO 72
C        ELSEIF(NINPUT.LE.4) THEN
C         ATOL = RINPUT(1)
C         ISPL = IINPUT(2)
C         XRF(1) = RINPUT(3)
C         XRF(2) = RINPUT(4)
C         GO TO 74
C        ENDIF
C C
C  70    WRITE(*,1090) ATOL
C  1090  FORMAT(1X,
C      &   'Enter corner angle criterion for refinement (deg):', F8.3)
       
C        CALL READR(1,ATOL_TMP,ERROR)
C        IF(ERROR) GO TO 70
C        ATOL = ATOL_TMP(1)
C C
C  71    WRITE(*,1091) ISPL
C  1091  FORMAT(1X,
C      &   'Enter type of spline parameter (1=uniform, 2=arclength):', I4)
C        CALL READI(1,ISPL,ERROR)
C        IF(ERROR) GO TO 71
C        IF(ISPL.LE.0) GO TO 500
C        IF(ISPL.GT.2) GO TO 71
C C
C  72    WRITE(*,1092) XRF(1), XRF(2)
C  1092  FORMAT(1X,
C      &   'Enter refinement x limits:', 2F10.5)
C        CALL READR(2,XRF,ERROR)
C        IF(ERROR) GO TO 72
C C
C  74    CONTINUE
C        IF(ISPL.EQ.1) THEN
C         SB(1) = 0.0
C         DO I = 2, NB
C           IF(XB(I).EQ.XB(I-1) .AND. YB(I).EQ.YB(I-1)) THEN
C            SB(I) = SB(I-1)
C           ELSE
C            SB(I) = SB(I-1) + 1.0
C           ENDIF
C         ENDDO
C         CALL SEGSPL(XB,XBP,SB,NB)
C         CALL SEGSPL(YB,YBP,SB,NB)
C        ENDIF
C C
C        CALL AREFINE(XB,YB,SB,XBP,YBP,NB, ATOL, 
C      &             IBX,NNEW,W1,W2,XRF(1),XRF(2))
C C
C        NBADD = NNEW - NB
C        WRITE(*,*) 'Number of points added: ', NBADD
C C
C        NB = NNEW
C        DO I = 1, NB
C          XB(I) = W1(I)
C          YB(I) = W2(I)
C        ENDDO
C C
C        CALL SCALC(XB,YB,SB,NB)
C        CALL SEGSPL(XB,XBP,SB,NB)
C        CALL SEGSPL(YB,YBP,SB,NB)
C C
C        CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
C      &             SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
C      &             EI11BA,EI22BA,APX1BA,APX2BA,
C      &             EI11BT,EI22BT,APX1BT,APX2BT,
C      &             THICKB,CAMBRB )
C C
C        CALL CANG(XB,YB,NB,1, IMAX,AMAX)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CLIS') THEN
C        CALL CLIS(XB,XBP,YB,YBP,SB,NB)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'FLAP' .OR.
C      &       COMAND.EQ.'F   '      ) THEN
C        CALL FLAP(RINPUT,NINPUT) !!! modified
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SLOP') THEN
C        LGSLOP = .NOT.LGSLOP
C        IF(LGSLOP) THEN
C         WRITE(*,*) 'Modified segment will be',
C      &             ' made tangent at endpoints'
C        ELSE
C         WRITE(*,*) 'Modified segment will not be',
C      &             ' made tangent at endpoints'
C        ENDIF
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'UNIT') THEN
C        CALL NORM(XB,XBP,YB,YBP,SB,NB)
C C
C C----- re-spline new geometry
C        CALL SCALC(XB,YB,SB,NB)
C        CALL SEGSPL(XB,XBP,SB,NB)
C        CALL SEGSPL(YB,YBP,SB,NB)
C C
C        CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
C      &             SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
C      &             EI11BA,EI22BA,APX1BA,APX2BA,
C      &             EI11BT,EI22BT,APX1BT,APX2BT,
C      &             THICKB,CAMBRB )
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'NAME') THEN
C        IF(COMARG.EQ.' ') THEN
C         CALL NAMMOD(NAME,0,-1)
C        ELSE
C         NAME = COMARG
C        ENDIF
C        CALL STRIP(NAME,NNAME)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'NINC') THEN
C        CALL NAMMOD(NAME,1,1)
C        CALL STRIP(NAME,NNAME)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'NDEC') THEN
C        CALL NAMMOD(NAME,-1,1)
C        CALL STRIP(NAME,NNAME)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SINT') THEN
C        CALL SPLNXY(XB,XBP,YB,YBP,SB,NB)
C C
C C--------------------------------------------------------
C       ELSE
C        WRITE(*,1100) COMAND
C  1100  FORMAT(' Command ',A4,' not recognized.  Type a " ? " for list.')
C        COMAND = '****'
C       ENDIF
C C
C       GO TO 500
C       END GDES


      SUBROUTINE ABCOPY
      USE XMOD
C
      IF(NB.LE.1) THEN
C        WRITE(*,*) 'ABCOPY: Buffer airfoil not available.'
       RETURN
      ELSEIF(NB.GT.IQX-5) THEN
C        WRITE(*,*) 'Maximum number of panel nodes  : ',IQX-5
C        WRITE(*,*) 'Number of buffer airfoil points: ',NB
C        WRITE(*,*) 'Current airfoil cannot be set.'
C        WRITE(*,*) 'Try executing PANE at Top Level instead.'
       RETURN
      ENDIF
      IF(N.NE.NB) LBLINI = .FALSE.
C
      N = NB
      DO 101 I=1, N
        X(I) = XB(I)
        Y(I) = YB(I)
  101 CONTINUE
      LGSAME = .TRUE.
C
      IF(LBFLAP) THEN
       XOF = XBF
       YOF = YBF
       LFLAP = .TRUE.
      ENDIF
C
C---- strip out doubled points
      I = 1
 102  CONTINUE
      I = I+1
      IF(X(I-1).EQ.X(I) .AND. Y(I-1).EQ.Y(I)) THEN
        DO 104 J=I, N-1
          X(J) = X(J+1)
          Y(J) = Y(J+1)
 104    CONTINUE
        N = N-1
      ENDIF
      IF(I.LT.N) GO TO 102
C
      CALL SCALC(X,Y,S,N)
      CALL SEGSPL(X,XP,S,N)
      CALL SEGSPL(Y,YP,S,N)
      CALL NCALC(X,Y,S,N,NX,NY)
      CALL LEFIND(SLE,X,XP,Y,YP,S,N)
      XLE = SEVAL(SLE,X,XP,S,N)
      YLE = SEVAL(SLE,Y,YP,S,N)
      XTE = 0.5*(X(1)+X(N))
      YTE = 0.5*(Y(1)+Y(N))
      CHORD  = SQRT( (XTE-XLE)**2 + (YTE-YLE)**2 )
      CALL TECALC
      CALL APCALC
C
      LGAMU = .FALSE.
      LWAKE = .FALSE.
      LQAIJ = .FALSE.
      LADIJ = .FALSE.
      LWDIJ = .FALSE.
      LIPAN = .FALSE.
      LVCONV = .FALSE.
      LSCINI = .FALSE.
CCC      LBLINI = .FALSE.
C
      RETURN
      END SUBROUTINE ABCOPY


      SUBROUTINE LERAD(RFAC,DOC)
C----------------------------
C     Changes buffer airfoil 
C     leading edge radius.
C----------------------------
      USE XMOD
C
      DOC = MAX( DOC , 0.001 )
C
      CALL LERSCL(XB,XBP,YB,YBP,SB,NB, DOC,RFAC, W1,W2)
C
      DO 40 I=1, NB
        XB(I) = W1(I)
        YB(I) = W2(I)
   40 CONTINUE
C
C---- spline new coordinates
      CALL SCALC(XB,YB,SB,NB)
      CALL SEGSPL(XB,XBP,SB,NB)
      CALL SEGSPL(YB,YBP,SB,NB)
C
      CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
     &            SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
     &            EI11BA,EI22BA,APX1BA,APX2BA,
     &            EI11BT,EI22BT,APX1BT,APX2BT,
     &            THICKB,CAMBRB )
C
C---- find max curvature
      CVMAX = 0.
      DO 6 I=NB/4, (3*NB)/4
        CV = CURV(SB(I),XB,XBP,YB,YBP,SB,NB)
        CVMAX = MAX( ABS(CV) , CVMAX )
    6 CONTINUE
C
      RADIUS = 1.0/CVMAX
C
      LGSAME = .FALSE.
C
      RETURN
      END SUBROUTINE LERAD



      SUBROUTINE FLAP(XF, YF, DDEF)
C----------------------------------------------------
C     Modifies buffer airfoil for a deflected flap.
C     Points may be added/subtracted in the flap
C     break vicinity to clean things up.
C----------------------------------------------------
      USE XMOD
      
      LOGICAL LCHANGE
C
      LOGICAL INSID
      LOGICAL LT1NEW,LT2NEW,LB1NEW,LB2NEW
C
      XBF = XF
      YBF = YF
C
      INSID = INSIDE(XB,YB,NB,XBF,YBF) .EQ. 1
C
      RDEF = DDEF*PI/180.0
      IF(RDEF .EQ. 0.0) RETURN
C
      IF(INSID) THEN
        ATOP = MAX( 0.0 , -RDEF )
        ABOT = MAX( 0.0 ,  RDEF )
      ELSE
        CHX = DEVAL(BOTS,XB,XBP,SB,NB) - DEVAL(TOPS,XB,XBP,SB,NB)
        CHY = DEVAL(BOTS,YB,YBP,SB,NB) - DEVAL(TOPS,YB,YBP,SB,NB)
        FVX = SEVAL(BOTS,XB,XBP,SB,NB) + SEVAL(TOPS,XB,XBP,SB,NB)
        FVY = SEVAL(BOTS,YB,YBP,SB,NB) + SEVAL(TOPS,YB,YBP,SB,NB)
        CRSP = CHX*(YBF-0.5*FVY) - CHY*(XBF-0.5*FVX)
        IF(CRSP .GT. 0.0) THEN
C-------- flap hinge is above airfoil
          ATOP = MAX( 0.0 ,  RDEF )
          ABOT = MAX( 0.0 ,  RDEF )
        ELSE
C-------- flap hinge is below airfoil
          ATOP = MAX( 0.0 , -RDEF )
          ABOT = MAX( 0.0 , -RDEF )
        ENDIF
      ENDIF
C
C---- find upper and lower surface break arc length values...
      CALL SSS(TOPS,ST1,ST2,ATOP,XBF,YBF,XB,XBP,YB,YBP,SB,NB,1)
      CALL SSS(BOTS,SB1,SB2,ABOT,XBF,YBF,XB,XBP,YB,YBP,SB,NB,2)
C
C---- ... and x,y coordinates
      XT1 = SEVAL(ST1,XB,XBP,SB,NB)
      YT1 = SEVAL(ST1,YB,YBP,SB,NB)
      XT2 = SEVAL(ST2,XB,XBP,SB,NB)
      YT2 = SEVAL(ST2,YB,YBP,SB,NB)
      XB1 = SEVAL(SB1,XB,XBP,SB,NB)
      YB1 = SEVAL(SB1,YB,YBP,SB,NB)
      XB2 = SEVAL(SB2,XB,XBP,SB,NB)
      YB2 = SEVAL(SB2,YB,YBP,SB,NB)
C
C
      WRITE(*,1100) XT1, YT1, XT2, YT2,
     &              XB1, YB1, XB2, YB2
 1100 FORMAT(/' Top breaks: x,y =  ', 2F9.5, 4X, 2F9.5
     &       /' Bot breaks: x,y =  ', 2F9.5, 4X, 2F9.5)
C
C---- find points adjacent to breaks
      DO 5 I=1, NB-1
        IF(SB(I).LE.ST1 .AND. SB(I+1).GT.ST1) IT1 = I+1
        IF(SB(I).LT.ST2 .AND. SB(I+1).GE.ST2) IT2 = I
        IF(SB(I).LE.SB1 .AND. SB(I+1).GT.SB1) IB1 = I
        IF(SB(I).LT.SB2 .AND. SB(I+1).GE.SB2) IB2 = I+1
    5 CONTINUE
C
      DSAVG = (SB(NB)-SB(1))/FLOAT(NB-1)
C
C---- smallest fraction of s increments i+1 and i+2 away from break point
      SFRAC = 0.33333
C
      IF(ATOP .NE. 0.0) THEN
        ST1P = ST1 + SFRAC*(SB(IT1  )-ST1)
        ST1Q = ST1 + SFRAC*(SB(IT1+1)-ST1)
        IF(SB(IT1) .LT. ST1Q) THEN
C-------- simply move adjacent point to ideal SFRAC location
          XT1NEW = SEVAL(ST1Q,XB,XBP,SB,NB)
          YT1NEW = SEVAL(ST1Q,YB,YBP,SB,NB)
          LT1NEW = .FALSE.
        ELSE
C-------- make new point at SFRAC location
          XT1NEW = SEVAL(ST1P,XB,XBP,SB,NB)
          YT1NEW = SEVAL(ST1P,YB,YBP,SB,NB)
          LT1NEW = .TRUE.
        ENDIF
C
        ST2P = ST2 + SFRAC*(SB(IT2 )-ST2)
        IT2Q = MAX(IT2-1,1)
        ST2Q = ST2 + SFRAC*(SB(IT2Q)-ST2)
        IF(SB(IT2) .GT. ST2Q) THEN
C-------- simply move adjacent point
          XT2NEW = SEVAL(ST2Q,XB,XBP,SB,NB)
          YT2NEW = SEVAL(ST2Q,YB,YBP,SB,NB)
          LT2NEW = .FALSE.
        ELSE
C-------- make new point
          XT2NEW = SEVAL(ST2P,XB,XBP,SB,NB)
          YT2NEW = SEVAL(ST2P,YB,YBP,SB,NB)
          LT2NEW = .TRUE.
        ENDIF
      ENDIF
C
      IF(ABOT .NE. 0.0) THEN
        SB1P = SB1 + SFRAC*(SB(IB1  )-SB1)
        SB1Q = SB1 + SFRAC*(SB(IB1-1)-SB1)
        IF(SB(IB1) .GT. SB1Q) THEN
C-------- simply move adjacent point
          XB1NEW = SEVAL(SB1Q,XB,XBP,SB,NB)
          YB1NEW = SEVAL(SB1Q,YB,YBP,SB,NB)
          LB1NEW = .FALSE.
        ELSE
C-------- make new point
          XB1NEW = SEVAL(SB1P,XB,XBP,SB,NB)
          YB1NEW = SEVAL(SB1P,YB,YBP,SB,NB)
          LB1NEW = .TRUE.
        ENDIF
C
        SB2P = SB2 + SFRAC*(SB(IB2 )-SB2)
        IB2Q = MIN(IB2+1,NB)
        SB2Q = SB2 + SFRAC*(SB(IB2Q)-SB2)
        IF(SB(IB2) .LT. SB2Q) THEN
C-------- simply move adjacent point
          XB2NEW = SEVAL(SB2Q,XB,XBP,SB,NB)
          YB2NEW = SEVAL(SB2Q,YB,YBP,SB,NB)
          LB2NEW = .FALSE.
        ELSE
C-------- make new point
          XB2NEW = SEVAL(SB2P,XB,XBP,SB,NB)
          YB2NEW = SEVAL(SB2P,YB,YBP,SB,NB)
          LB2NEW = .TRUE.
        ENDIF
      ENDIF
C
cc      DSTOP = ABS(SB(IT2)-SB(IT1))
cc      DSBOT = ABS(SB(IB2)-SB(IB1))
C
      SIND = SIN(RDEF)
      COSD = COS(RDEF)
C
C---- rotate flap points about the hinge point (XBF,YBF)
      DO 10 I=1, NB
        IF(I.GE.IT1 .AND. I.LE.IB1) GO TO 10
C
        XBAR = XB(I) - XBF
        YBAR = YB(I) - YBF
C
        XB(I) = XBF  +  XBAR*COSD  +  YBAR*SIND
        YB(I) = YBF  -  XBAR*SIND  +  YBAR*COSD
   10 CONTINUE
C
      IDIF = IT1-IT2-1
      IF(IDIF.GT.0) THEN
C----- delete points on upper airfoil surface which "disappeared".
       NB  = NB -IDIF
       IT1 = IT1-IDIF
       IB1 = IB1-IDIF
       IB2 = IB2-IDIF
       DO 21 I=IT2+1, NB
         SB(I) = SB(I+IDIF)
         XB(I) = XB(I+IDIF)
         YB(I) = YB(I+IDIF)
   21  CONTINUE
      ENDIF
C
      IDIF = IB2-IB1-1
      IF(IDIF.GT.0) THEN
C----- delete points on lower airfoil surface which "disappeared".
       NB  = NB -IDIF
       IB2 = IB2-IDIF
       DO 22 I=IB1+1, NB
         SB(I) = SB(I+IDIF)
         XB(I) = XB(I+IDIF)
         YB(I) = YB(I+IDIF)
   22  CONTINUE
      ENDIF
C
C
      IF(ATOP .EQ. 0.0) THEN
C
C------ arc length of newly created surface on top of airfoil
        DSNEW = ABS(RDEF)*SQRT((XT1-XBF)**2 + (YT1-YBF)**2)
C
C------ number of points to be added to define newly created surface
        NPADD = INT(1.5*DSNEW/DSAVG + 1.0)
ccc     NPADD = INT(1.5*DSNEW/DSTOP + 1.0)
C
C------ skip everything if no points are to be added
        IF(NPADD.EQ.0) GO TO 35
C
C------ increase coordinate array length to make room for the new point(s)
        NB  = NB +NPADD
        IT1 = IT1+NPADD
        IB1 = IB1+NPADD
        IB2 = IB2+NPADD
        DO 30 I=NB, IT1, -1
          XB(I) = XB(I-NPADD)
          YB(I) = YB(I-NPADD)
   30   CONTINUE
C
C------ add new points along the new surface circular arc segment
        DANG = RDEF / FLOAT(NPADD)
        XBAR = XT1 - XBF
        YBAR = YT1 - YBF
        DO 31 IP=1, NPADD
          ANG = DANG*(FLOAT(IP) - 0.5)
          CA = COS(ANG)
          SA = SIN(ANG)
C
          XB(IT1-IP) = XBF  +  XBAR*CA + YBAR*SA
          YB(IT1-IP) = YBF  -  XBAR*SA + YBAR*CA
   31   CONTINUE
C
      ELSE
C
C------ set point in the corner and possibly two adjacent points
        NPADD = 1
        IF(LT2NEW) NPADD = NPADD+1
        IF(LT1NEW) NPADD = NPADD+1
C
        NB  = NB +NPADD
        IT1 = IT1+NPADD
        IB1 = IB1+NPADD
        IB2 = IB2+NPADD
        DO 33 I=NB, IT1, -1
          XB(I) = XB(I-NPADD)
          YB(I) = YB(I-NPADD)
   33   CONTINUE
C
        IF(LT1NEW) THEN
         XB(IT1-1) = XT1NEW
         YB(IT1-1) = YT1NEW
         XB(IT1-2) = XT1
         YB(IT1-2) = YT1
        ELSE
         XB(IT1  ) = XT1NEW
         YB(IT1  ) = YT1NEW
         XB(IT1-1) = XT1
         YB(IT1-1) = YT1
        ENDIF
C
        XBAR = XT2NEW - XBF
        YBAR = YT2NEW - YBF
        IF(LT2NEW) THEN
          XB(IT2+1) = XBF  +  XBAR*COSD + YBAR*SIND
          YB(IT2+1) = YBF  -  XBAR*SIND + YBAR*COSD
        ELSE
          XB(IT2  ) = XBF  +  XBAR*COSD + YBAR*SIND
          YB(IT2  ) = YBF  -  XBAR*SIND + YBAR*COSD
        ENDIF
C
      ENDIF
   35 CONTINUE
C
C
      IF(ABOT .EQ. 0.0) THEN
C
C------ arc length of newly created surface on top of airfoil
        DSNEW = ABS(RDEF)*SQRT((XB1-XBF)**2 + (YB1-YBF)**2)
C
C------ number of points to be added to define newly created surface
        NPADD = INT(1.5*DSNEW/DSAVG + 1.0)
ccc     NPADD = INT(1.5*DSNEW/DSBOT + 1.0)
C
C------ skip everything if no points are to be added
        IF(NPADD.EQ.0) GO TO 45
C
C------ increase coordinate array length to make room for the new point(s)
        NB  = NB +NPADD
        IB2 = IB2+NPADD
        DO 40 I=NB, IB2, -1
          XB(I) = XB(I-NPADD)
          YB(I) = YB(I-NPADD)
   40   CONTINUE
C
C------ add new points along the new surface circular arc segment
        DANG = RDEF / FLOAT(NPADD)
        XBAR = XB1 - XBF
        YBAR = YB1 - YBF
        DO 41 IP=1, NPADD
          ANG = DANG*(FLOAT(IP) - 0.5)
          CA = COS(ANG)
          SA = SIN(ANG)
C
          XB(IB1+IP) = XBF  +  XBAR*CA + YBAR*SA
          YB(IB1+IP) = YBF  -  XBAR*SA + YBAR*CA
   41   CONTINUE
C
      ELSE

C------ set point in the corner and possibly two adjacent points
        NPADD = 1
        IF(LB2NEW) NPADD = NPADD+1
        IF(LB1NEW) NPADD = NPADD+1
C
        NB  = NB +NPADD
        IB2 = IB2+NPADD
        DO 43 I=NB, IB2, -1
          XB(I) = XB(I-NPADD)
          YB(I) = YB(I-NPADD)
   43   CONTINUE
C
        IF(LB1NEW) THEN
         XB(IB1+1) = XB1NEW
         YB(IB1+1) = YB1NEW
         XB(IB1+2) = XB1
         YB(IB1+2) = YB1
        ELSE
         XB(IB1  ) = XB1NEW
         YB(IB1  ) = YB1NEW
         XB(IB1+1) = XB1
         YB(IB1+1) = YB1
        ENDIF
C
        XBAR = XB2NEW - XBF
        YBAR = YB2NEW - YBF
        IF(LB2NEW) THEN
          XB(IB2-1) = XBF  +  XBAR*COSD + YBAR*SIND
          YB(IB2-1) = YBF  -  XBAR*SIND + YBAR*COSD
        ELSE
          XB(IB2  ) = XBF  +  XBAR*COSD + YBAR*SIND
          YB(IB2  ) = YBF  -  XBAR*SIND + YBAR*COSD
        ENDIF
C
      ENDIF
   45 CONTINUE
C
C
C---- check new geometry for splinter segments 
      STOL = 0.2
      CALL SCHECK(XB,YB,NB, STOL, LCHANGE)
C
C---- spline new geometry
      CALL SCALC(XB,YB,SB,NB)
      CALL SEGSPL(XB,XBP,SB,NB)
      CALL SEGSPL(YB,YBP,SB,NB)
C
      CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
     &            SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
     &            EI11BA,EI22BA,APX1BA,APX2BA,
     &            EI11BT,EI22BT,APX1BT,APX2BT,
     &            THICKB,CAMBRB )
C
      LBFLAP = .TRUE.
C
      IF(LGSYM) THEN
       WRITE(*,*)
       WRITE(*,*) 'Disabling symmetry enforcement'
       LGSYM = .FALSE.
      ENDIF
C
      RETURN
      END SUBROUTINE FLAP


      FUNCTION INSIDE(X,Y,N, XF,YF)
      DIMENSION X(N),Y(N)
C-------------------------------------
C     Returns .TRUE. if point XF,YF 
C     is inside contour X(i),Y(i).
C-------------------------------------
C
C---- integrate subtended angle around airfoil perimeter
      ANGLE = 0.0
      DO 10 I=1, N
        IP = I+1
        IF(I.EQ.N) IP = 1
        XB1 = X(I)  - XF
        YB1 = Y(I)  - YF
        XB2 = X(IP) - XF
        YB2 = Y(IP) - YF
        ANGLE = ANGLE + (XB1*YB2 - YB1*XB2)
     &                   / SQRT((XB1**2 + YB1**2)*(XB2**2 + YB2**2))
 10   CONTINUE
C
C---- angle = 0 if XF,YF is outside, angle = +/- 2 pi  if XF,YF is inside
      IF ( ABS(ANGLE) .GT. 1.0 ) THEN
        INSIDE = 1
      ELSE
        INSIDE = 0
      ENDIF
C
      RETURN
      END FUNCTION INSIDE


      SUBROUTINE TGAP(GAPNEW,DOC)
C----------------------------------
C     Used to set buffer airfoil 
C     trailing edge gap
C----------------------------------
      USE XMOD
C
      CALL LEFIND(SBLE,XB,XBP,YB,YBP,SB,NB)
      XBLE = SEVAL(SBLE,XB,XBP,SB,NB)
      YBLE = SEVAL(SBLE,YB,YBP,SB,NB)
      XBTE = 0.5*(XB(1)+XB(NB))
      YBTE = 0.5*(YB(1)+YB(NB))
      CHBSQ = (XBTE-XBLE)**2 + (YBTE-YBLE)**2
C
      DXN = XB(1) - XB(NB)
      DYN = YB(1) - YB(NB)
      GAP = SQRT(DXN**2 + DYN**2)
C
C---- components of unit vector parallel to TE gap
      IF(GAP.GT.0.0) THEN
       DXU = DXN / GAP
       DYU = DYN / GAP
      ELSE
       DXU = -.5*(YBP(NB) - YBP(1))
       DYU = 0.5*(XBP(NB) - XBP(1))
      ENDIF
C
      DOC = MIN( MAX( DOC , 0.0 ) , 1.0 )
C
      DGAP = GAPNEW - GAP
C
C---- go over each point, changing the y-thickness appropriately
      DO 30 I=1, NB
C
C------ chord-based x/c
        XOC = (  (XB(I)-XBLE)*(XBTE-XBLE)
     &         + (YB(I)-YBLE)*(YBTE-YBLE) ) / CHBSQ
C
C------ thickness factor tails off exponentially away from trailing edge
        IF(DOC .EQ. 0.0) THEN
          TFAC = 0.0
          IF(I.EQ.1 .OR. I.EQ.NB) TFAC = 1.0
        ELSE
          ARG = MIN( (1.0-XOC)*(1.0/DOC-1.0) , 15.0 )
          TFAC = EXP(-ARG)
        ENDIF
C
        IF(SB(I).LE.SBLE) THEN
         XB(I) = XB(I) + 0.5*DGAP*XOC*TFAC*DXU
         YB(I) = YB(I) + 0.5*DGAP*XOC*TFAC*DYU
        ELSE
         XB(I) = XB(I) - 0.5*DGAP*XOC*TFAC*DXU
         YB(I) = YB(I) - 0.5*DGAP*XOC*TFAC*DYU
        ENDIF
   30 CONTINUE
C
      CALL SCALC(XB,YB,SB,NB)
      CALL SEGSPL(XB,XBP,SB,NB)
      CALL SEGSPL(YB,YBP,SB,NB)
C
      CALL GEOPAR(XB,XBP,YB,YBP,SB,NB,W1,
     &            SBLE,CHORDB,AREAB,RADBLE,ANGBTE,
     &            EI11BA,EI22BA,APX1BA,APX2BA,
     &            EI11BT,EI22BT,APX1BT,APX2BT,
     &            THICKB,CAMBRB )
C
      LGSAME = .FALSE.
C
      RETURN
      END SUBROUTINE TGAP