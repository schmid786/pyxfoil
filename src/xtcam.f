***********************************************************************
C    Module:  xtcam.f
C 
C    Copyright (C) 2000 Harold Youngren, Mark Drela
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
      SUBROUTINE SORTOL(TOL,KK,S,W)
      DIMENSION S(KK), W(KK)
      LOGICAL DONE
C
C---- sort arrays
      DO IPASS=1, 1234
        DONE = .TRUE.
        DO N=1, KK-1
          NP = N+1
          IF(S(NP).LT.S(N)) THEN
           TEMP = S(NP)
           S(NP) = S(N)
           S(N) = TEMP
           TEMP = W(NP)
           W(NP) = W(N)
           W(N) = TEMP
           DONE = .FALSE.
          ENDIF
        END DO
        IF(DONE) GO TO 10
      END DO
      WRITE(*,*) 'Sort failed'
C
C---- search for near-duplicate pairs and eliminate extra points
C---- Modified 4/24/01 HHY to check list until ALL duplicates removed
C     This cures a bug for sharp LE foils where there were 3 LE points in
C     camber, thickness lists from GETCAM.
C
 10   KKS = KK
      DONE = .TRUE.
      DO 20 K=1, KKS
        IF(K.GE.KK) GO TO 20
        DSQ = (S(K)-S(K+1))**2 + (W(K)-W(K+1))**2
        IF(DSQ.GE.TOL*TOL) GO TO 20
C------- eliminate extra point pairs
ccc         write(*,*) 'extra on point ',k,kks
         KK = KK-1
         DO KT=K+1, KK
           S(KT) = S(KT+1)
           W(KT) = W(KT+1)
         END DO
         DONE = .FALSE.
   20 CONTINUE
      IF(.NOT.DONE) GO TO 10
C
      RETURN
      END SUBROUTINE SORTOL


      SUBROUTINE SORTDUP(KK,S,W)
C--- Sort arrays in S with no removal of duplicates
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
   11 CONTINUE
      RETURN
      END SUBROUTINE SORTDUP


      SUBROUTINE FIXDUP(KK,S,W)
C--- Check arrays in S by removing leading and ending duplicates
C    eliminate extra duplicates (more than one duplicate point) elsewhere
      DIMENSION S(KK), W(KK)
C
C---- Check first elements for dups
      IF(S(2).EQ.S(1)) THEN
        DO N=1, KK-1
          S(N) = S(N+1)
          W(N) = W(N+1)
        END DO
        KK = KK - 1
      ENDIF
C
C---- Check last elements for dups
      IF(S(KK).EQ.S(KK-1)) THEN
        S(KK-1) = S(KK)
        W(KK-1) = W(KK)
        KK = KK - 1
      ENDIF
C
C--- Eliminate more than 2 succeeding identical elements 
 10   DO N=1, KK-2
        IF(S(N).EQ.S(N+1) .AND. S(N).EQ.S(N+2)) THEN
          DO I = N, KK-1
           S(I) = S(I+1)
           W(I) = W(I+1)
          END DO
          KK = KK - 1
          GO TO 10
        ENDIF
      END DO
C
      RETURN
      END SUBROUTINE FIXDUP

C       SUBROUTINE CAMB
C C-------------------------------------------
C C     Camber modification routine.
C C-------------------------------------------
C       USE XMOD
C C
C       CHARACTER*72 LINE
C       CHARACTER*4 COMAND, COMOLD
C       CHARACTER*128 COMARG, ARGOLD
C       CHARACTER*1 ANS
C C
C       DIMENSION IINPUT(20)
C       DIMENSION RINPUT(20)
C       LOGICAL ERROR, LCLEAR, LGPARSAVE
C C
C  1000 FORMAT(A)
C C
C       LGPARSAVE = LGPARM
C       COMAND = '****'
C       COMARG = ' '
C       LCLEAR  = .TRUE.
C       LU = 8
C C
C       COMOLD = COMAND
C       ARGOLD = COMARG
C C
C C--- Check chordline direction (should be unrotated for camber routines
C C    to function correctly
C       XLE = SEVAL(SBLE,XB,XBP,SB,NB)
C       YLE = SEVAL(SBLE,YB,YBP,SB,NB)
C       XTE = 0.5*(XB(1)+XB(NB))
C       YTE = 0.5*(YB(1)+YB(NB))
C       AROT = ATAN2(YLE-YTE,XTE-XLE) / DTOR
C       IF(ABS(AROT).GT.1.0) THEN
C         WRITE(*,*) ' '
C         WRITE(*,*) 'Warning: CAMB does not work well on rotated foils'
C         WRITE(*,*) 'Current chordline angle: ',AROT
C         WRITE(*,*) 'Proceeding anyway...'
C       ENDIF
C C
C       LDCPLOT = .FALSE.
C       LGPARM = .NOT.LDCPLOT
C C
C       WRITE(*,1200)
C C
C C--------------------------------------------------------------
C C---- pick up here to initialize camber and loading
C  100  CONTINUE
C C
C C---- find leftmost point
C cc    CALL LEFIND(SBL,XB,XBP,YB,YBP,SB,NB)
C       CALL XLFIND(SBL,XB,XBP,SB,NB)
C C
C       XBL = SEVAL(SBL, XB,XBP,SB,NB)
C       XBR = 0.5*(XB(1)+XB(NB))
C C
C C---- set "chordline" axis vector for camber,thickness definitions
C       XBCH = XBR - XBL
C cc    YBCH = YBR - YBL
C       YBCH = 0.
C       SBCH = SQRT(XBCH**2 + YBCH**2)
C C
C C---- find the current buffer airfoil camber and thickness
C       CALL GETCAM(XCM,YCM,NCM,XTK,YTK,NTK,
C      &            XB,XBP,YB,YBP,SB,NB )
C C
C       NCAM = MIN( 201 , NCM )
C C---- initialize arrays for added camber and added loading 
C       DO K=1, NCAM
C         XCAM(K) = XCM(1) + (XCM(NCM)-XCM(1))*FLOAT(K-1)/FLOAT(NCAM-1)
C       ENDDO
C       IF(LCLEAR) THEN
C C---- initialize added camber to zero
C        NCADD = 2
C        XCADD(1) = XCAM(1)
C        XCADD(2) = XCAM(NCAM)
C        YCADD(1) = 0.0
C        YCADD(2) = 0.0
C C---- initialize added loading to zero
C        NPADD = 2
C        XPADD(1) = XCAM(1)
C        XPADD(2) = XCAM(NCAM)
C        YPADD(1) = 0.0
C        YPADD(2) = 0.0
C C---- spline added camber line y(x) and added loading dCp(x)
C        CALL SEGSPL(YCADD,YCADDP,XCADD,NCADD)
C        CALL SEGSPL(YPADD,YPADDP,XPADD,NPADD)
C C----- interpolate to dense plotting array
C        DO K=1, NCAM
C          YCAM(K)  = SEVAL(XCAM(K),YCADD,YCADDP,XCADD,NCADD)
C          YCAMP(K) = DEVAL(XCAM(K),YCADD,YCADDP,XCADD,NCADD)
C          PCAM(K)  = SEVAL(XCAM(K),YPADD,YPADDP,XPADD,NPADD)
C          PCAMP(K) = DEVAL(XCAM(K),YPADD,YPADDP,XPADD,NPADD)
C        ENDDO
C        LCLEAR = .FALSE.
C       ENDIF
C C
C C--------------------------------------------------------------
C C---- pick up here to find and display current camber and added camber line properties
C  200  CONTINUE
C C
C       WRITE(*,*) 
C       WRITE(*,*) 'Buffer airfoil thickness and camber:'
C       CALL TCBUF
C C
C       XMX = 0.0
C       YMX = 0.0
C       DO K=1, NCAM
C         IF(ABS(YCAM(K)) .GT. YMX) THEN
C          XMX = XCAM(K)
C          YMX = YCAM(K)
C         ENDIF
C       END DO
C       CHRD = XCAM(NCAM) - XCAM(1)
C       ALE = ATAN( DEVAL(XCAM(1)   ,YCAM,YCAMP,XCAM,NCAM) ) / DTOR
C       ATE = ATAN( DEVAL(XCAM(NCAM),YCAM,YCAMP,XCAM,NCAM) ) / DTOR
C       WRITE(*,1100) ALE, ATE, YMX/CHRD, XMX/CHRD
C  1100 FORMAT(/' Added camber line incidence at LE = ', F6.2, '  deg.',
C      &       /' Added camber line incidence at TE = ', F6.2, '  deg.',
C      &       /' Max added camber y/c = ', F8.4, '  at x/c = ', F7.3  )
C C
C C--------------------------------------------------------------
C       LGPARM = .NOT.LDCPLOT
C C
C C==================================================
C C---- top of menu loop 
C   500 CALL ASKC('..CAMB^',COMAND,COMARG)
C C
C C---- process previous command ?
C       IF(COMAND(1:1).EQ.'!') THEN
C         IF(COMOLD.EQ.'****') THEN
C           WRITE(*,*) 'Previous ..CAMB command not valid'
C           GO TO 500
C         ELSE
C           COMAND = COMOLD
C           COMARG = ARGOLD
C         ENDIF
C       ENDIF
C C
C       IF(COMAND.EQ.'    ') THEN
C C----- just <return> was typed... clean up plotting and exit CAMP
C        LGPARM = LGPARSAVE
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
C       IF(COMAND.EQ.'    ') THEN
C        RETURN
C C
C       ELSEIF(COMAND.EQ.'?   ') THEN
C        WRITE(*,1200)
C  1200  FORMAT(
C      &  /'   <cr>    Return to GDES'
C      &  /'   TFAC rr Scale existing thickness and camber'
C      &  /'   TSET rr Set new thickness and camber'
C      &  /'   HIGH rr Move camber and thickness highpoints'
C      &  /'   WRTC    Write airfoil camber and thickness to file'
C      & //'   RDAC    Read   added camber  x/c,y/c from file'
C      &  /'   SETC    Set    added camber  x/c,y/c from camberline'
C      &  /'   INPC    Input  added camber  x/c,y/c from keyboard'
C      & //'   RDAP    Read   added loading x/c,Dcp from file'
C      &  /'   INPP    Input  added loading x/c,DCp from keyboard'
C      &  /'   SLOP    Toggle modified-camber,dCp slope matching flag'
C      &  /'   SCAL r  Scale the added camber'
C      &  /'   CLR     Clear the added camber'
C      &  /'   ADD     Add added camber to the existing camberline')
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'TFAC') THEN
CCC      replace with THKCAM !!!
C        CALL TCSCAL(RINPUT,NINPUT)
C        GO TO 100
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'TSET') THEN
C        CALL TCSET(RINPUT,NINPUT)
C        GO TO 100
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'HIGH') THEN
C        CALL HIPNT(RINPUT,NINPUT)
C        GO TO 100
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'WRTC') THEN
C        CALL ASKS('Enter output camber filename^',FNAME)
C C
C        OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=12)
C        WRITE(*,*)
C        WRITE(*,*) 'Output file exists.  Overwrite?  Y'
C        READ(*,1000) ANS
C        IF(INDEX('Nn',ANS).EQ.0) GO TO 13
C C
C        CLOSE(LU)
C        WRITE(*,*) 'Current camber not saved.'
C        GO TO 500
C C
C  12    OPEN(LU,FILE=FNAME,STATUS='NEW',ERR=15)
C  13    REWIND(LU)
C C
C C--- Write out normalized camber coordinates (x/c in range 0->1, y/c)
C C    and normalized thickness (x/c,y/c)
C        WRITE(LU,1000) 'Camber: '//NAME
C        DO K = 1, NCM
C          WRITE(LU,14) (XCM(K)-XCM(1))/XBCH,(YCM(K)-YCM(1))/XBCH,
C      &                (XTK(K)-XTK(1))/XBCH,(YTK(K)-YTK(1))/XBCH
C        END DO
C        CLOSE(LU)
C        GO TO 500
C C
C  14    FORMAT(4(1X,F12.6))
C C
C  15    WRITE(*,*) 'Error opening camber/thickness save file'
C        GO TO 500
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'RDAC ') THEN
C        CALL ASKS('Enter added camber filename^',FNAME)
C        OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=19)
C        READ(LU,1000,ERR=18,END=18) LINE
C        NCADD = 0
C        DO K = 1, NTX
C          READ(LU,*,ERR=18,END=18) XX,YY
C          NCADD = NCADD + 1
C          XCADD(NCADD) = XX
C          YCADD(NCADD) = YY
C        END DO
C  18    CLOSE(LU)
C        IF(NCADD.LE.1 .OR.  (XCADD(NCADD)-XCADD(1)).EQ.0.0) THEN
C          NCADD = 2
C          XCADD(1) = XCAM(1)
C          XCADD(2) = XCAM(NCAM)
C          YCADD(1) = 0.0
C          YCADD(2) = 0.0
C          WRITE(*,*) 'No added camber points found'
C          GO TO 100
C        ENDIF
C C----- normalize input camber to x/c range 0->1
C        XCORG = XCADD(1)
C        XCSCL = XCADD(NCADD) - XCORG
C        DO K=1, NCADD
C          XCADD(K) = (XCADD(K)-XCORG) / XCSCL
C          YCADD(K) =  YCADD(K)        / XCSCL
C        ENDDO
C C----- reorigin and scale added camber to camber line coordinates
C        DO K=1, NCADD
C          XCADD(K) = XCAM(1) + XCADD(K)*XBCH - YCADD(K)*YBCH
C          YCADD(K) =           XCADD(K)*YBCH + YCADD(K)*XBCH
C        ENDDO
C C----- spline camber line y(x)
C        CALL SEGSPL(YCADD,YCADDP,XCADD,NCADD)
C C----- interpolate to dense plotting array
C        DO K=1, NCAM
C          YCAM(K)  = SEVAL(XCAM(K),YCADD,YCADDP,XCADD,NCADD)
C          YCAMP(K) = DEVAL(XCAM(K),YCADD,YCADDP,XCADD,NCADD)
C        ENDDO
C        LDCPLOT = .FALSE.
C        GO TO 200

C  19    WRITE(*,*)
C        WRITE(*,*) 'Error opening added camber file'
C        GO TO 500
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SETC') THEN
C C----- Set added camber from camberline
C        NCADD = NCM
C        DO K=1, NCM
C          XCADD(K) = XCM(K)
C          YCADD(K) = YCM(K)
C        END DO 
C C----- spline added camber line y(x)
C        CALL SEGSPL(YCADD,YCADDP,XCADD,NCADD)
C C
C C----- interpolate to dense plotting array
C        DO K=1, NCAM
C          YCAM(K)  = SEVAL(XCAM(K),YCADD,YCADDP,XCADD,NCADD)
C          YCAMP(K) = DEVAL(XCAM(K),YCADD,YCADDP,XCADD,NCADD)
C        ENDDO
C        LDCPLOT = .FALSE.
C        GO TO 200
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'INPC') THEN
C C----- Manual input of camber points
C        WRITE(*,2000)
C  2000  FORMAT(/' Manual input of camber x/c,y/c:',
C      &       //' Input x/c, y/c pairs from  x/c = 0  to  x/c = 1',
C      &        /' <cr> ends input')
C C
C C--- Points of x/c, y/c are added to existing definition of added camber line
C        NCADD = 0
C        DO 25 I=1, 2*IQX
C  23      READ(*,1000,ERR=24) LINE
C          IF(LINE.EQ.' ') GO TO 26
C          READ(LINE,*,ERR=24) XX,YY
C          IF(XX.LE.0.0) THEN
C           XX = 0.0
C          ELSEIF(XX.GE.1.0) THEN
C           XX = 1.0
C          ENDIF
C          NCADD = NCADD + 1
C          XCADD(NCADD) = XCAM(1) + XX*XBCH - YY*YBCH
C          YCADD(NCADD) =           XX*YBCH + YY*XBCH
C          GO TO 25
C  24      WRITE(*,*) 'try again'
C          GO TO 23
C  25    CONTINUE
C C----- Sort points allowing duplicates for slope breaks 
C  26    CALL SORTDUP(NCADD,XCADD,YCADD)
C        CALL FIXDUP (NCADD,XCADD,YCADD)
C C----- spline camber line y(x)
C        CALL SEGSPL(YCADD,YCADDP,XCADD,NCADD)
C C
C C----- interpolate to dense plotting array
C        DO K=1, NCAM
C          YCAM(K)  = SEVAL(XCAM(K),YCADD,YCADDP,XCADD,NCADD)
C          YCAMP(K) = DEVAL(XCAM(K),YCADD,YCADDP,XCADD,NCADD)
C        ENDDO
C        LDCPLOT = .FALSE.
C        GO TO 200
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'RDAP ') THEN
C        CALL ASKS('Enter added loading filename^',FNAME)
C        OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=29)
C        READ(LU,1000,ERR=27,END=27) LINE
C        NPADD = 0
C        DO K = 1, NTX
C          READ(LU,*,ERR=27,END=27) XX,YY
C          write(*,*) 'added loading x, dcp ',xx,yy
C          NPADD = NPADD + 1
C          XPADD(NPADD) = XX*XBCH
C          YPADD(NPADD) = YY
C        END DO
C  27    CLOSE(LU)
C        IF(NPADD.LE.1 .OR.  (XPADD(NPADD)-XPADD(1)).EQ.0.0) THEN
C          NPADD = 2
C          XPADD(1) = XCAM(1)
C          XPADD(2) = XCAM(NCAM)
C          YPADD(1) = 0.0
C          YPADD(2) = 0.0
C          WRITE(*,*) 'No added loading points found'
C          GO TO 100
C        ENDIF
C C
C C----- Sort points allowing duplicates for slope breaks 
C        CALL SORTDUP(NPADD,XPADD,YPADD)
C        CALL FIXDUP (NPADD,XPADD,YPADD)
C c       write(*,*) 'postdupcheck'
C c       do k=1,npadd
C c         write(*,*) xpadd(k),ypadd(k)
C c       end do
C C----- spline loading DCp(x)
C        CALL SEGSPL(YPADD,YPADDP,XPADD,NPADD)
C C
C C----- interpolate to dense plotting array
C        DO K=1, NCAM
C          PCAM(K)  = SEVAL(XCAM(K),YPADD,YPADDP,XPADD,NPADD)
C          PCAMP(K) = DEVAL(XCAM(K),YPADD,YPADDP,XPADD,NPADD)
C          write(14,*) xcam(k),pcam(k),pcamp(k)
C        ENDDO
C C
C C----- calculate camber line corresponding to specified loading
C        CALL CPCAM(NCAM,XCAM,YCAM,YCAMP,PCAM,PCAMP)
C C
C C----- calculate added lift and moment from added loading
C        CLX = 0.0
C        CMX = 0.0
C        DO K=1, NCAM-1
C         DX =      XCAM(K+1) - XCAM(K)
C         XA = 0.5*(XCAM(K+1) + XCAM(K))
C         PA = 0.5*(PCAM(K+1) + PCAM(K))
C         CLX = CLX + PA*DX
C         CMX = CMX + PA*DX*(XCMREF-XA)
C        END DO  
C        WRITE(*,1110) CLX, CMX
C C
C        LDCPLOT = .TRUE.
C        GO TO 200
C C
C  29    WRITE(*,*)
C        WRITE(*,*) 'Error opening added loading file'
C        GO TO 500
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'INPP') THEN
C C----- Manual input of loading delta Cp
C        WRITE(*,3000)
C  3000  FORMAT(/' Manual input of loading x/c, DCp:',
C      &       //' Input x/c, DCp pairs from  x/c = 0  to  x/c = 1',
C      &        /' <cr> ends input')
C C
C C--- Points of x/c, dCp are added to existing definition of loading line
C        NPADD = 0
C        DO 35 I=1, 2*IQX
C  33      READ(*,1000,ERR=34) LINE
C          IF(LINE.EQ.' ') GO TO 36
C          READ(LINE,*,ERR=34) XX,YY
C          IF(XX.LE.0.0) THEN
C            XX = 0.0
C           ELSEIF(XX.GE.1.0) THEN
C            XX = 1.0
C          ENDIF
C          NPADD = NPADD + 1
C          XPADD(NPADD) = XCAM(1) + XX*XBCH
C          YPADD(NPADD) = YY
C          GO TO 35
C  34      WRITE(*,*) 'try again'
C          GO TO 33
C  35    CONTINUE
C C----- Sort points allowing duplicates for slope breaks 
C  36    CALL SORTDUP(NPADD,XPADD,YPADD)
C        CALL FIXDUP (NPADD,XPADD,YPADD)
C c       do i = 1, npadd
C c         write(*,*) 'x,y addp ',xpadd(i),ypadd(i)
C c       end do
C C----- spline loading DCp(x)
C        CALL SEGSPL(YPADD,YPADDP,XPADD,NPADD)
C C
C C----- interpolate to dense plotting array
C        DO K=1, NCAM
C          PCAM(K)  = SEVAL(XCAM(K),YPADD,YPADDP,XPADD,NPADD)
C          PCAMP(K) = DEVAL(XCAM(K),YPADD,YPADDP,XPADD,NPADD)
C        ENDDO
C C
C C----- calculate camber line corresponding to specified loading
C        CALL CPCAM(NCAM,XCAM,YCAM,YCAMP,PCAM,PCAMP)
C C
C C----- calculate added lift and moment from added loading
C        CLX = 0.0
C        CMX = 0.0
C        DO K=1, NCAM-1
C         DX =      XCAM(K+1) - XCAM(K)
C         XA = 0.5*(XCAM(K+1) + XCAM(K))
C         PA = 0.5*(PCAM(K+1) + PCAM(K))
C         CLX = CLX + PA*DX
C         CMX = CMX + PA*DX*(XCMREF-XA)
C        END DO  
C        WRITE(*,1110) CLX, CMX
C C
C        LDCPLOT = .TRUE.
C        GO TO 200
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CLR ') THEN
C C----- Clear the added camber
C        LCLEAR  = .TRUE.
C        LDCPLOT = .FALSE.
C        GO TO 100
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SCAL') THEN
C C----- Scale camber
C        IF(NINPUT.GE.1) THEN
C         SCAL = RINPUT(1)
C        ELSE
C         SCAL = 1.0
C         CALL ASKR('Enter camber scaling factor^',SCAL)
C        ENDIF
C C
C C--- Scale added camber user arrays
C        DO I = 1, NCADD
C          YCADD(I)  = YCADD(I) *SCAL
C          YPADD(I)  = YPADD(I) *SCAL
C          YCADDP(I) = YCADDP(I)*SCAL
C          YPADDP(I) = YPADDP(I)*SCAL
C        END DO
C C
C C--- Scale added camber arrays
C        DO I = 1, NCAM
C          YCAM(I)   = YCAM(I) *SCAL
C          YCAMP(I)  = YCAMP(I)*SCAL
C          PCAM(I)   = PCAM(I) *SCAL
C          PCAMP(I)  = PCAMP(I)*SCAL
C        END DO
C C
C C----- calculate added lift and moment from added loading
C        CLX = 0.0
C        CMX = 0.0
C        DO K=1, NCAM-1
C         DX =      XCAM(K+1) - XCAM(K)
C         XA = 0.5*(XCAM(K+1) + XCAM(K))
C         PA = 0.5*(PCAM(K+1) + PCAM(K))
C         CLX = CLX + PA*DX
C         CMX = CMX + PA*DX*(XCMREF-XA)
C        END DO  
C        IF(CLX.NE.0.0 .AND. CMX.NE.0.0) WRITE(*,1110) CLX, CMX
C C
C        COMOLD = COMAND
C        ARGOLD = COMARG
C C
C C
C C----- go replot new shape and resume menu loop
C        GO TO 200
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'ADD ') THEN
C C----- Add camber to camberline
C        CALL SEGSPL(YCAM,YCAMP,XCAM,NCAM)
C C
C C----- go over each point, changing the camber line appropriately
C        DO I=1, NB
C C------- coordinates of point on the opposite side with the same chord x value
C          CALL SOPPS(SBOPP, SB(I),XB,XBP,YB,YBP,SB,NB,SBL)
C          XBOPP = SEVAL(SBOPP,XB,XBP,SB,NB)
C          YBOPP = SEVAL(SBOPP,YB,YBP,SB,NB)
C C
C C------- set present camber height
C          OLDCAM = 0.5*(YB(I)+YBOPP)*XBCH/SBCH
C      &          - 0.5*(XB(I)+XBOPP)*YBCH/SBCH
C C
C C------- add on new camber
C          CAM = OLDCAM
C      &       + SEVAL(XB(I),YCAM,YCAMP,XCAM,NCAM)
C C
C C------- set new y coordinate by changing camber & thickness appropriately
C          W1(I) = CAM  +  0.5*(YB(I)-YBOPP)
C        END DO
C C
C        DO I=1, NB
C          YB(I) = W1(I)
C        END DO
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
C        LDCPLOT = .FALSE.
C C---- reinitialize added camber to zero
C        LCLEAR  = .TRUE.
C        GO TO 100
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SLOP') THEN
C        LCSLOP = .NOT.LCSLOP
C        IF(LCSLOP) THEN
C         WRITE(*,*) 'Modified segment will be',
C      &             ' made tangent at endpoints'
C        ELSE
C         WRITE(*,*) 'Modified segment will not be',
C      &             ' made tangent at endpoints'
C        ENDIF
C C
C C--------------------------------------------------------
C       ELSE
C        WRITE(*,8000) COMAND
C  8000  FORMAT(1X,A4,' command not recognized.  Type a "?" for list')
C C
C       ENDIF
C       GO TO 500
C C
C  1110 FORMAT(/' Delta Cp loading gives delta CL = ',F7.3,
C      &       /'                        delta CM = ',F7.3)
C C
C       END CAMB


      SUBROUTINE ZERCAM
C-----------------------------------------
C     Zeros out camber of buffer airfoil
C-----------------------------------------
      USE XMOD
C
C      WRITE(*,*) 'Setting current camber to zero.'
      TFAC = 1.0
      CFAC = 0.0
      CALL THKCAM(TFAC,CFAC)
C
C---- make points exact mirror images
      CALL YSYM(XB,XBP,YB,YBP,SB,2*IQX,NB,1,W1,W2)
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
      END SUBROUTINE ZERCAM



C       SUBROUTINE TCBUF
C C------------------------------------------------------
C C     Reports buffer airfoil thickness and camber
C C------------------------------------------------------
C       USE XMOD
C C
C C--- find the current buffer airfoil camber and thickness
C       CALL GETCAM(XCM,YCM,NCM,XTK,YTK,NTK,
C      &            XB,XBP,YB,YBP,SB,NB )
C       CALL GETMAX(XCM,YCM,YCMP,NCM,CXMAX,CYMAX)
C       CALL GETMAX(XTK,YTK,YTKP,NTK,TXMAX,TYMAX)
C C
C       WRITE(*,1000) 2.0*TYMAX,TXMAX, CYMAX,CXMAX
C  1000 FORMAT( ' Max thickness = ',F8.4,'  at x = ',F7.3,
C      &       /' Max camber    = ',F8.4,'  at x = ',F7.3)
C C
C       RETURN
C       END SUBROUTINE TCBUF


      SUBROUTINE TCSET(TNEW, CNEW)
      USE XMOD
C------------------------------------------------------
C     Finds buffer airfoil thickness and/or camber,
C     plots thickness, camber and airfoil, 
C     and scales t and/or c by user input factors
C------------------------------------------------------
C
C--- find the current buffer airfoil camber and thickness
      CALL GETCAM(XCM,YCM,NCM,XTK,YTK,NTK,
     &            XB,XBP,YB,YBP,SB,NB )
      CALL GETMAX(XCM,YCM,YCMP,NCM,CXMAX,CYMAX)
      CALL GETMAX(XTK,YTK,YTKP,NTK,TXMAX,TYMAX)
C
      CFAC = 1.0
      TFAC = 1.0
      IF(CYMAX.NE.0.0 .AND. CNEW.NE.999.0) CFAC = CNEW /      CYMAX
      IF(TYMAX.NE.0.0 .AND. TNEW.NE.999.0) TFAC = TNEW / (2.0*TYMAX)
C
      CALL THKCAM(TFAC,CFAC)
C
      RETURN
      END SUBROUTINE TCSET



      SUBROUTINE THKCAM(TFAC,CFAC)
C---------------------------------------------------
C     Changes buffer airfoil thickness and camber
C---------------------------------------------------
      USE XMOD
C
      CALL LEFIND(SBLE,XB,XBP,YB,YBP,SB,NB)
C
C---This fails miserably with sharp LE foils, tsk,tsk,tsk HHY 4/24/01
C---- set baseline vector normal to surface at LE point
c      DXC = -DEVAL(SBLE,YB,YBP,SB,NB)
c      DYC =  DEVAL(SBLE,XB,XBP,SB,NB)
c      DSC = SQRT(DXC**2 + DYC**2)
c      DXC = DXC/DSC
c      DYC = DYC/DSC
C
C---Rational alternative 4/24/01 HHY
      XLE = SEVAL(SBLE,XB,XBP,SB,NB)
      YLE = SEVAL(SBLE,YB,YBP,SB,NB)
      XTE = 0.5*(XB(1)+XB(NB))
      YTE = 0.5*(YB(1)+YB(NB))
      CHORD = SQRT((XTE-XLE)**2 + (YTE-YLE)**2)
C---- set unit chord-line vector
      DXC = (XTE-XLE) / CHORD
      DYC = (YTE-YLE) / CHORD
C
C---- go over each point, changing the y-thickness appropriately
      DO I=1, NB
C------ coordinates of point on the opposite side with the same x value
        CALL SOPPS(SBOPP, SB(I),XB,XBP,YB,YBP,SB,NB,SBLE)
        XBOPP = SEVAL(SBOPP,XB,XBP,SB,NB)
        YBOPP = SEVAL(SBOPP,YB,YBP,SB,NB)
C
C------ set new y coordinate by changing camber & thickness appropriately
        XCAVG =        ( 0.5*(XB(I)+XBOPP)*DXC + 0.5*(YB(I)+YBOPP)*DYC )
        YCAVG = CFAC * ( 0.5*(YB(I)+YBOPP)*DXC - 0.5*(XB(I)+XBOPP)*DYC )

        XCDEL =        ( 0.5*(XB(I)-XBOPP)*DXC + 0.5*(YB(I)-YBOPP)*DYC )
        YCDEL = TFAC * ( 0.5*(YB(I)-YBOPP)*DXC - 0.5*(XB(I)-XBOPP)*DYC )
C
        W1(I) = (XCAVG+XCDEL)*DXC - (YCAVG+YCDEL)*DYC
        W2(I) = (YCAVG+YCDEL)*DXC + (XCAVG+XCDEL)*DYC
      ENDDO
C
      DO I=1, NB
        XB(I) = W1(I)
        YB(I) = W2(I)
      ENDDO
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
      RETURN
      END SUBROUTINE THKCAM



      SUBROUTINE HIPNT(THPNT,CHPNT)
      USE XMOD
C------------------------------------------------------
C     Changes buffer airfoil 
C     thickness and/or camber highpoint
C------------------------------------------------------
      REAL XFN(5), YFN(5), YFNP(5)
C
C---- find leftmost point location 
      CALL XLFIND(SBL,XB,XBP,SB,NB)
      YBL = SEVAL(SBL,YB,YBP,SB,NB)
C
C---- find the current buffer airfoil camber and thickness
      CALL GETCAM(XCM,YCM,NCM,XTK,YTK,NTK,
     &            XB,XBP,YB,YBP,SB,NB )
C
C---- find the max thickness and camber
      CALL GETMAX(XCM,YCM,YCMP,NCM,CXMAX,CYMAX)
      CALL GETMAX(XTK,YTK,YTKP,NTK,TXMAX,TYMAX)
C
      IF (THPNT.LE.XTK(1) .OR. THPNT.GE.XTK(NTK)) THPNT = TXMAX
      IF (CHPNT.LE.XCM(1) .OR. CHPNT.GE.XCM(NCM)) CHPNT = CXMAX
C
C--- a simple cubic mapping function is used to map x/c to move highpoints
C
C    the assumption is that a smooth function (cubic, given by the old and 
C    new highpoint locations) maps the range 0-1 for x/c
C    into the range 0-1 for altered x/c distribution for the same y/c
C    thickness or camber (ie. slide the points smoothly along the x axis)
C
C--- shift thickness highpoint
      IF (THPNT .NE. TXMAX) THEN
       XFN(1) = XTK(1)
       XFN(2) = TXMAX
       XFN(3) = XTK(NTK)
       YFN(1) = XTK(1)
       YFN(2) = THPNT
       YFN(3) = XTK(NTK)
       CALL SPLINA(YFN,YFNP,XFN,3)
       DO I = 1, NTK
         XTK(I) = SEVAL(XTK(I),YFN,YFNP,XFN,3)
       ENDDO
      ENDIF
C
C--- shift camber highpoint
      IF (CHPNT .NE. CXMAX) THEN
       XFN(1) = XCM(1)
       XFN(2) = CXMAX
       XFN(3) = XCM(NCM)
       YFN(1) = XCM(1)
       YFN(2) = CHPNT
       YFN(3) = XCM(NCM)
       CALL SPLINA(YFN,YFNP,XFN,3)
       DO I = 1, NCM
         XCM(I) = SEVAL(XCM(I),YFN,YFNP,XFN,3)
       ENDDO
      ENDIF
C
C---- Make new airfoil from thickness and camber
C     new airfoil points are spaced to match the original
C--- HHY 4/24/01 got rid of splining vs X,Y vs S (buggy), now spline Y(X)
      CALL SEGSPL(YTK,YTKP,XTK,NTK)
      CALL SEGSPL(YCM,YCMP,XCM,NCM)
C
C
C---- for each orig. airfoil point setup new YB from camber and thickness
      DO 40 I=1, NB
C
C------ spline camber and thickness at original xb points
        YCC = SEVAL(XB(I),YCM,YCMP,XCM,NCM)
        YTT = SEVAL(XB(I),YTK,YTKP,XTK,NTK)
C
C------ set new y coordinate from new camber & thickness
        IF (SB(I) .LE. SBL) THEN
          YB(I) = YCC + YTT
         ELSE
          YB(I) = YCC - YTT
        ENDIF
C---- Add Y-offset for original leftmost (LE) point to camber
        YB(I) = YB(I) + YBL
   40 CONTINUE
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
      RETURN
      END SUBROUTINE HIPNT



      SUBROUTINE GETCAM (XCM,YCM,NCM,XTK,YTK,NTK,
     &                   X,XP,Y,YP,S,N )
C------------------------------------------------------
C     Finds camber and thickness 
C     distribution for input airfoil 
C------------------------------------------------------
      REAL XCM(*), YCM(*)
      REAL XTK(*), YTK(*)
      REAL X(*),XP(*),Y(*),YP(*),S(*)
C
      CALL XLFIND(SL,X,XP,S,N)
      XL = SEVAL(SL,X,XP,S,N)
      YL = SEVAL(SL,Y,YP,S,N)
C
C---- go over each point, finding opposite points, getting camber and thickness
      DO 10 I=1, N
C------ coordinates of point on the opposite side with the same x value
        CALL SOPPS(SOPP, S(I), X,XP,Y,YP,S,N,SL)
        XOPP = SEVAL(SOPP,X,XP,S,N)
        YOPP = SEVAL(SOPP,Y,YP,S,N)
C
C------ get camber and thickness
        XCM(I) = 0.5*(X(I)+XOPP)
        YCM(I) = 0.5*(Y(I)+YOPP)
        XTK(I) = 0.5*(X(I)+XOPP)
        YTK(I) = 0.5*(Y(I)-YOPP)
        YTK(I) = ABS(YTK(I))
c        if (XOPP.gt.0.9) then
c         write(*,*) 'cm i,x,y ',i,xcm(i),ycm(i)
c         write(*,*) 'tk i,x,y ',i,xtk(i),ytk(i)
c        endif
   10 CONTINUE
C
C---- Tolerance for nominally identical points
      TOL = 1.0E-3 * (S(N)-S(1))
C
C---- Sort the camber points
      NCM = N+1
      XCM(N+1) = XL
      YCM(N+1) = YL
      CALL SORTOL(TOL,NCM,XCM,YCM)
C
C--- Reorigin camber from LE so camberlines start at Y=0  4/24/01 HHY 
C    policy now to generate camber independent of Y-offsets 
      YOF = YCM(1)
      DO I = 1, NCM
        YCM(I) = YCM(I) - YOF
      END DO
C
C---- Sort the thickness points
      NTK = N+1
      XTK(N+1) = XL
      YTK(N+1) = 0.0
      CALL SORTOL(TOL,NTK,XTK,YTK)
C
      RETURN
      END SUBROUTINE GETCAM


      SUBROUTINE GETMAX(X,Y,YP,N,XMAX,YMAX)
      REAL X(*), Y(*), YP(*)
C------------------------------------------------
C     Calculates camber or thickness highpoint 
C     and x position
C------------------------------------------------
C
      XLEN = X(N) - X(1)
      XTOL = XLEN * 1.0E-5
C
      CALL SEGSPL(Y,YP,X,N)
C
C---- get approx max point and rough interval size
      YMAX0 = Y(1)
      XMAX0 = X(1)
      DO 5 I = 2, N
        IF (ABS(Y(I)).GT.ABS(YMAX0)) THEN
          YMAX0 = Y(I)
          XMAX0 = 0.5*(X(I-1) + X(I))
          DDX = 0.5*ABS(X(I+1) - X(I-1))
        ENDIF
 5    CONTINUE
      XMAX = XMAX0
C
C---- do a Newton loop to refine estimate
      DO 10 ITER=1, 10
        YMAX  = SEVAL(XMAX,Y,YP,X,N)
        RES   = DEVAL(XMAX,Y,YP,X,N)
        RESP  = D2VAL(XMAX,Y,YP,X,N)
        IF (ABS(XLEN*RESP) .LT. 1.0E-6) GO TO 20
          DX = -RES/RESP
          DX = SIGN( MIN(0.5*DDX,ABS(DX)) , DX)
          XMAX = XMAX + DX
          IF(ABS(DX) .LT. XTOL) GO TO 20
   10 CONTINUE
      WRITE(*,*)
     &  'GETMAX: Newton iteration for max camber/thickness failed.'
      YMAX = YMAX0
      XMAX = XMAX0
C
 20   RETURN
      END SUBROUTINE GETMAX



      SUBROUTINE CPCAM(N,X,Y,DYDX,P,DPDX)
      REAL X(*), Y(*), DYDX(*), P(*), DPDX(*)
C------------------------------------------------------------------
C     Generates y(x) camberline from specified DCp(x) distribution.
C
C     Input:  N       number of points
C             X(.)    x array
C             P(.)    DCp array
C             DPDX(.) dDCp/dx array
C
C     Output: Y(.)    y(x) array
C             DYDX(.) dy/dx array
C------------------------------------------------------------------
C---- 1 / 4 pi
      DATA QOPI / 7.9577471545948E-02 /
C
C---- singular part of camber y(x) due to finite loadings P0,P1 at LE and TE
C-    dYSING/dX has logarithmic singularity at x=X0,X1
      YSING(XT) = QOPI*P1*((XT-X1)*LOG(MAX((X1-XT)/(X1-X0),1.E-6)) - XT)
     &          - QOPI*P0*((XT-X0)*LOG(MAX((XT-X0)/(X1-X0),1.E-6)) - XT)
C
      P0 = P(1)
      P1 = P(N)
C
      X0 = X(1)
      X1 = X(N)
C         
C---- calculate Cauchy integral for y'(x) with removed singularity
      DO I=1, N
        DYDX(I) = 0.0
        J = 1
        IF(I.EQ.J) THEN
         YP1 = DPDX(J)
        ELSE
         YP1 = (P(J) - P(I)) / (X(J) - X(I))
        ENDIF
        DO J=2, N
          IF(I.EQ.J) THEN
           YP2 = DPDX(J)
          ELSE
           YP2 = (P(J) - P(I)) / (X(J) - X(I))
          ENDIF
          DYDX(I) = DYDX(I) + 0.5*(YP1+YP2)*(X(J)-X(J-1))
          YP1 = YP2
        END DO
        DYDX(I) = QOPI*DYDX(I)
C
C------ add on removed part of Cauchy integral, further leaving out the
C-      possible infinities at LE and TE so that y(x) can be safely splined. 
C-      The infinities are analytically integrated, and added on to y(x)
C-      with the statement function YSING.
        IF(I.NE.1) THEN
         DYDX(I) = DYDX(I)
     &           - QOPI*(P(I) - P0)*LOG(X(I) - X0)
        ENDIF
        IF(I.NE.N) THEN
         DYDX(I) = DYDX(I)
     &           + QOPI*(P(I) - P1)*LOG(X1 - X(I))
        ENDIF
      END DO
C
C---- integrate regular part of y'(x) from LE
      Y(1) = 0.
      DO I=2, N
        Y(I) = Y(I-1)
     &       + 0.5*(DYDX(I) + DYDX(I-1))*(X(I) - X(I-1))
      END DO
C
C---- add on singular part
      DO I=1, N
        Y(I) = Y(I) + YSING(X(I))
      END DO
C
C---- add offset and angle of attack to get y(0) = y(1) = 0
      Y0 = Y(1)
      Y1 = Y(N)
      DO I=1, N
        Y(I) = Y(I)
     &       - Y0*(X1  -X(I))/(X1-X0)
     &       - Y1*(X(I)-X0  )/(X1-X0)
      END DO
C
      RETURN
      END SUBROUTINE CPCAM
