C***********************************************************************
C    Module:  xoper.f
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
C       SUBROUTINE OPER
C       USE XMOD
C       CHARACTER*4 COMAND, COMOLD
C       LOGICAL LRECALC
C C
C       CHARACTER*128 COMARG, ARGOLD
C C
C       DIMENSION IINPUT(20)
C       DIMENSION RINPUT(20)
C       LOGICAL ERROR


C       common /damp_com/ idamp

C C
C C---- retain last-command info if OPER is exited and then re-entered
C       SAVE COMOLD, ARGOLD
C C
C       COMAND = '****'
C       COMARG = ' '
C       LRECALC = .FALSE.
C C
C       IF(N.EQ.0) THEN
C        WRITE(*,*)
C        WRITE(*,*) '***  No airfoil available  ***'
C        RETURN
C       ENDIF
C C
C ccc 500  CONTINUE
C       COMOLD = COMAND
C       ARGOLD = COMARG
C C
C C====================================================
C C---- start of menu loop
C  500  CONTINUE
C C
C       IF(LVISC) THEN
C         CALL ASKC('.OPERv^',COMAND,COMARG)
C       ELSE
C         CALL ASKC('.OPERi^',COMAND,COMARG)
C       ENDIF
C C
C C---- process previous command ?
C       IF(COMAND(1:1).EQ.'!') THEN
C         IF(COMOLD.EQ.'****') THEN
C           WRITE(*,*) 'Previous .OPER command not valid'
C           GO TO 500
C         ELSE
C           COMAND = COMOLD
C           COMARG = ARGOLD
C           LRECALC = .TRUE.
C         ENDIF
C       ELSE
C         LRECALC = .FALSE.
C       ENDIF
C C
C       IF(COMAND.EQ.'    ') THEN
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
C      & /'   !        Redo last ALFA,CLI,CL,ASEQ,CSEQ,VELS'
C      &//'   Visc r   Toggle Inviscid/Viscous mode'
C      & /'  .VPAR     Change BL parameter(s)'
C      & /'   Re   r   Change Reynolds number'
C      & /'   Mach r   Change Mach number'
C      & /'   Type i   Change type of Mach,Re variation with CL'
C      & /'   ITER     Change viscous-solution iteration limit'
C      & /'   INIT     Toggle BL initialization flag'
C      &//'   Alfa r   Prescribe alpha'
C      & /'   CLI  r   Prescribe inviscid CL'
C      & /'   Cl   r   Prescribe CL'
C      &//'   FMOM     Calculate flap hinge moment and forces'
C      & /'   FNEW rr  Set new flap hinge point'
C      & /'   VELS rr  Calculate velocity components at a point'
C      & /'   DUMP f   Output Ue,Dstar,Theta,Cf vs s,x,y to file'
C      & /'   CPWR f   Output x vs Cp to file'
C      & /'   CPMN     Report minimum surface Cp'
C      & /'   NAME s   Specify new airfoil name'
C      & /'   NINC     Increment name version number')
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'WAK') THEN
C        IF(NINPUT.GE.1) THEN
C         WAKLEN = RINPUT(1)
C        ELSE
C         WRITE(*,*) 'Current WakLen =', WAKLEN
C         CALL ASKR('Enter new WakLen^',WAKLEN)
C        ENDIF
C
C--------------------------------------------------------
      SUBROUTINE O_VISC(REINF2)
C       Toggle Inviscid/Viscous mode
       USE XMOD
       REAL, INTENT(IN   ) :: REINF2
       
       LVISC = .NOT. LVISC
C
       IF(LVISC) THEN
         REINF1 = REINF2
       ENDIF
       LVCONV = .FALSE.
      END SUBROUTINE O_VISC
C
C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'VPAR') THEN
C        CALL VPAR
C
C--------------------------------------------------------
      SUBROUTINE O_RE(REINF2)
C       Change Reynolds number
       USE XMOD
       REAL, INTENT(IN   ) :: REINF2
C
       REINF1 = REINF2
C
       CALL MRCL(1.0,MINF_CL,REINF_CL)
       LVCONV = .FALSE.
      END SUBROUTINE O_RE
C
C--------------------------------------------------------
      SUBROUTINE O_MACH(MINF2)
C       Change Mach number
       USE XMOD
       REAL, INTENT(IN   ) :: MINF2
C
       MINF1 = MINF2
C
       IF(MINF1.GE.1.0) THEN
        WRITE(*,*) 'Supersonic freestream not allowed'
        NINPUT = 0
        RETURN
       ENDIF
       CALL MRCL(1.0,MINF_CL,REINF_CL)
       CALL COMSET
C
       CALL CPCALC(N,QINV,QINF,MINF,CPI)
       IF(LVISC) CALL CPCALC(N+NW,QVIS,QINF,MINF,CPV)
       CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &             CL,CM,CDP, CL_ALF,CL_MSQ)
       CALL CDCALC
       LVCONV = .FALSE.
      END SUBROUTINE O_MACH
C
C--------------------------------------------------------
      SUBROUTINE O_TYPE(ITYP)
C       Change type of Mach,Re variation with CL
       USE XMOD
       INTEGER, INTENT(IN   ) :: ITYP
C
       IF(ITYP.EQ.1) THEN
        MATYP = 1
        RETYP = 1
       ELSE IF(ITYP.EQ.2) THEN
        MATYP = 2
        RETYP = 2
       ELSE IF(ITYP.EQ.3) THEN
        MATYP = 1
        RETYP = 3
       ENDIF
C
       IF(ITYP.LT.1 .OR. ITYP.GT.3) THEN
        RETURN
       ENDIF
C
       CALL MRCL(1.0,MINF_CL,REINF_CL)
       CALL COMSET
       LVCONV = .FALSE.
      END SUBROUTINE O_TYPE
C
C--------------------------------------------------------
      SUBROUTINE O_ITER(ITMAX2)
C       Change viscous-solution iteration limit
       USE XMOD
       INTEGER, INTENT(IN   ) :: ITMAX2
C
       ITMAX = ITMAX2
C
       IF(ITMAX.LT.1) THEN
        ITMAX = 10
       ENDIF
      END SUBROUTINE O_ITER
C
C--------------------------------------------------------
      SUBROUTINE BL_INIT
C       Toggle BL initialization flag
       USE XMOD
C
       LBLINI = .NOT.LBLINI
C
       IF(.NOT.LBLINI) THEN
        LIPAN = .FALSE.
       ENDIF
      END SUBROUTINE BL_INIT
C
C--------------------------------------------------------
      SUBROUTINE O_ALFA(ADEG2)
C       Prescribe alpha
       USE XMOD
       REAL, INTENT(IN   ) :: ADEG2
C
       ADEG = ADEG2
C
       LALFA = .TRUE.
       ALFA = DTOR*ADEG
       QINF = 1.0
       CALL SPECAL
       IF(ABS(ALFA-AWAKE) .GT. 1.0E-5) LWAKE  = .FALSE.
       IF(ABS(ALFA-AVISC) .GT. 1.0E-5) LVCONV = .FALSE.
       IF(ABS(MINF-MVISC) .GT. 1.0E-5) LVCONV = .FALSE. 
C
       IF(LVISC) CALL VISCAL(ITMAX)
       CALL FCPMIN
C
       IF(LVISC .AND. .NOT.LVCONV) THEN
        WRITE(*,*) 'Calculation not converged'
       ENDIF
      END SUBROUTINE O_ALFA
C
C--------------------------------------------------------
      SUBROUTINE O_CLI(CLSPEC2)
C       Prescribe inviscid CL
       USE XMOD
       REAL, INTENT(IN   ) :: CLSPEC2
C
       CLSPEC = CLSPEC2
C
       LALFA = .TRUE.
       ALFA = 0.0
       QINF = 1.0
       CALL SPECCL
       ADEG = ALFA/DTOR
       IF(ABS(ALFA-AWAKE) .GT. 1.0E-5) LWAKE  = .FALSE.
       IF(ABS(ALFA-AVISC) .GT. 1.0E-5) LVCONV = .FALSE.
       IF(ABS(MINF-MVISC) .GT. 1.0E-5) LVCONV = .FALSE.
C
       IF(LVISC) CALL VISCAL(ITMAX)
       CALL FCPMIN
C
       IF(LVISC .AND. .NOT.LVCONV) THEN
        WRITE(*,*) 'Calculation not converged'
       ENDIF
      END SUBROUTINE O_CLI
C
C--------------------------------------------------------
      SUBROUTINE O_CL(CLSPEC2)
C       Prescribe CL
       USE XMOD
       REAL, INTENT(IN   ) :: CLSPEC2
C
       CLSPEC = CLSPEC2
C
       LALFA = .FALSE.
       ALFA = 0.0
       QINF = 1.0
       CALL SPECCL
       ADEG = ALFA/DTOR
       IF(ABS(ALFA-AWAKE) .GT. 1.0E-5) LWAKE  = .FALSE.
       IF(ABS(ALFA-AVISC) .GT. 1.0E-5) LVCONV = .FALSE.
       IF(ABS(MINF-MVISC) .GT. 1.0E-5) LVCONV = .FALSE.
C
       IF(LVISC) CALL VISCAL(ITMAX)
       CALL FCPMIN
C
       IF(LVISC .AND. .NOT.LVCONV) THEN
        WRITE(*,*) 'Calculation not converged'
       ENDIF
      END SUBROUTINE O_CL
C
C--------------------------------------------------------
      SUBROUTINE O_FMOM(XOF2, YOF2)
C       Calculate flap hinge moment and forces
       REAL, INTENT(IN   ) :: XOF2, YOF2
C
       CALL MHINGE(XOF2, YOF2)
      END SUBROUTINE O_FMOM
C        WRITE(*,1500) XOF,YOF,HMOM,HFX,HFY
C  1500  FORMAT(/' Flap hinge x,y :', 2F8.4/
C      &         '                                           2  2'/
C      &         ' Hinge moment/span = ',F8.6,'  x  1/2 rho V  c '/
C      &         '                                           2   '/
C      &         ' x-Force     /span = ',F8.6,'  x  1/2 rho V  c '/
C      &         '                                           2   '/
C      &         ' y-Force     /span = ',F8.6,'  x  1/2 rho V  c '/)
C
C--------------------------------------------------------
      SUBROUTINE O_FNEW(XOF2, YOF2)
C       Set new flap hinge point
       USE XMOD
       REAL, INTENT(IN   ) :: XOF2, YOF2
C
       XOF = XOF2
       YOF = YOF2
C
       LFLAP = .FALSE.
      END SUBROUTINE O_FNEW
C
C--------------------------------------------------------
      SUBROUTINE O_VELS(XXX, YYY, UUU, VVV, QQQ, CPP)
C       Calculate velocity components at a point
C       XXX - x-position
C       YYY - y-position
C       UUU - u/Uinf
C       VVV - v/Uinf
C       QQQ - q/Uinf
C       CPP - Cp
       REAL, INTENT(IN   ) :: XXX, YYY
       REAL, INTENT(  OUT) :: UUU, VVV, QQQ, CPP
C
       CALL PSILIN(0,XXX,YYY,-1.0,0.0,PSI,VVV,.FALSE.,.TRUE.)
       CALL PSILIN(0,XXX,YYY, 0.0,1.0,PSI,UUU,.FALSE.,.TRUE.)
C
       QQQ = SQRT(UUU**2 + VVV**2)
       CPP = 1.0 - (UUU**2 + VVV**2)
      END SUBROUTINE O_VELS
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'DUMP') THEN
C        CALL BLDUMP(COMARG) !!! modified
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CPWR') THEN
C        CALL CPDUMP(COMARG) !!! modified
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CPMN') THEN
C        IF(LVISC)THEN
C         WRITE(*,1769) CPMNI, XCPMNI, CPMNV, XCPMNV
C  1769   FORMAT('  Minimum Inviscid Cp =',F8.4,'   at x =',F8.4
C      &       / '  Minimum Viscous  Cp =',F8.4,'   at x =',F8.4 )
C        ELSE
C         WRITE(*,1779) CPMNI, XCPMNI
C  1779   FORMAT('  Minimum Inviscid Cp =',F8.4,'   at x =',F8.4)
C        ENDIF
C C
C C--------------------------------------------------------
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
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'NINC') THEN
C        CALL NAMMOD(NAME,1,1)
CCC        CALL STRIP(NAME,NNAME)
C        NAME = ADJUSTL(NAME)
C        NNAME = LEN_TRIM(NAME)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'NDEC') THEN
C        CALL NAMMOD(NAME,-1,1)
CCC        CALL STRIP(NAME,NNAME)
C        NAME = ADJUSTL(NAME)
C        NNAME = LEN_TRIM(NAME)
C C
C C--------------------------------------------------------
C       ELSEIF(COMAND.EQ.'DAMP') THEN
C        IF(IDAMP.EQ.0) THEN
C         IDAMP = 1
C         WRITE(*,*) 'Modified amplification used'
C        ELSE 
C         IDAMP = 0
C         WRITE(*,*) 'Original amplification used'
C        ENDIF
C C--------------------------------------------------------
C       ELSE
C        WRITE(*,8000) COMAND
C  8000  FORMAT(1X,A4,' command not recognized.  Type a "?" for list')

C       ENDIF
C C
C C---- go back to top of menu loop
C       GO TO 500
C       END OPER


      SUBROUTINE FCPMIN
C------------------------------------------------
C     Finds minimum Cp on dist for cavitation work
C------------------------------------------------
      USE XMOD
C
      XCPMNI = X(1)
      XCPMNV = X(1)
      CPMNI = CPI(1)
      CPMNV = CPV(1)
C
      DO I = 2, N + NW
        IF(CPI(I) .LT. CPMNI) THEN
         XCPMNI = X(I)
         CPMNI = CPI(I)
        ENDIF
        IF(CPV(I) .LT. CPMNV) THEN
         XCPMNV = X(I)
         CPMNV = CPV(I)
        ENDIF
      ENDDO
C

      IF(.NOT. LVISC)THEN
        CPMNV  = CPMNI
        XCPMNV = XCPMNI
      ENDIF
C
      RETURN
      END SUBROUTINE FCPMIN



      SUBROUTINE NAMMOD(NAME,KDEL,KMOD0)
      CHARACTER*(*) NAME
C-------------------------------------------
C     Requests new modified NAME with 
C     version number in brackets, e.g.
C            NACA 0012  [5]
C
C     If bracketed index exists in NAME,
C        it is incremented by KDEL.
C     If no bracketed index exists, it 
C        is added with initial value KMOD0,
C        unless KMOD0 is negative in which 
C        case nothing is added.
C-------------------------------------------
      CHARACTER*48 NAMDEF
C
CCC      CALL STRIP(NAME,NNAME)
      NAME = ADJUSTL(NAME)
      NNAME = LEN_TRIM(NAME)
      KBRACK1 = INDEX(NAME,'[')
      KBRACK2 = INDEX(NAME,']')
C
      NAMDEF = NAME(1:NNAME)
C
      IF(KBRACK1.NE.0 .AND. 
     &   KBRACK2.NE.0 .AND. KBRACK2-KBRACK1.GT.1) THEN
C----- brackets exist... get number, (go get user's input on READ error)
       READ(NAME(KBRACK1+1:KBRACK2-1),*,ERR=40) KMOD
       KMOD = IABS(KMOD)
       KMODP = MOD( KMOD+KDEL , 100 )
       IF(KBRACK1.GE.2) THEN
        NAME = NAME(1:KBRACK1-1)
       ELSE
        NAME = ' '
       ENDIF
CCC       CALL STRIP(NAME,NNAME)
       NAME = ADJUSTL(NAME)
       NNAME = LEN_TRIM(NAME)
      ELSEIF(KMOD0.GT.0) THEN
       KMODP = MOD( KMOD0 , 100 )
      ELSE
       KMODP = 0
      ENDIF
C
      IF    (KMODP.GE.10) THEN
       NAMDEF = NAME(1:NNAME) // ' [  ]'
       WRITE(NAMDEF(NNAME+3:NNAME+4),1020) KMODP
 1020  FORMAT(I2)
      ELSEIF(KMODP.GE. 1) THEN
       NAMDEF = NAME(1:NNAME) // ' [ ]'
       WRITE(NAMDEF(NNAME+3:NNAME+3),1025) KMODP
 1025  FORMAT(I1)
      ENDIF
C
C  40   WRITE(*,1040) NAMDEF
C  1040 FORMAT(/' Enter airfoil name or <return> for default:  ',A)
C       READ(*,1000) NAME
C  1000 FORMAT(A)
C       IF(NAME .EQ. ' ') NAME = NAMDEF
C
  40  RETURN
      END SUBROUTINE NAMMOD



      SUBROUTINE BLDUMP(FNAME1)
      USE XMOD
      CHARACTER*(*) FNAME1
C
 1000 FORMAT(A,A)
C
      FNAME = FNAME1
C
      LU = 19
      OPEN(LU,FILE=FNAME,STATUS='UNKNOWN')
      REWIND(LU)
C
      WRITE(LU,1000)
     & '#    s        x        y     Ue/Vinf    Dstar     Theta ',
     & '     Cf       H        CD       CT'
C
      CALL COMSET
C
      DO 10 I=1, N
        IS = 1
        IF(GAM(I) .LT. 0.0) IS = 2
C
        IF(LIPAN .AND. LVISC) THEN
          IF(IS.EQ.1) THEN
            IBL = IBLTE(IS) - I + 1
          ELSE
            IBL = IBLTE(IS) + I - N
          ENDIF
          DS = DSTR(IBL,IS)
          TH = THET(IBL,IS)
          CF =  TAU(IBL,IS)/(0.5*QINF**2)
          IF(TH.EQ.0.0) THEN
           H = 1.0
          ELSE
           H = DS/TH
          ENDIF
        ELSE
          DS = 0.
          TH = 0.
          CF = 0.
          H = 1.0
        ENDIF
        UE = (GAM(I)/QINF)*(1.0-TKLAM) / (1.0 - TKLAM*(GAM(I)/QINF)**2)
        HSTINV = GAMM1*(MINF/QINF)**2 / (1.0 + 0.5*GAMM1*MINF**2)
        AMSQ = UE*UE*HSTINV / (GAMM1*(1.0 - 0.5*UE*UE*HSTINV))
        CALL HKIN( H, AMSQ, HK, DUMMY, DUMMY)
C
        CDIS = DIS(IBL,IS)/QINF**3
        CT = CTAU(IBL,IS)
        WRITE(LU,8500) S(I), X(I), Y(I), UE, DS, TH, CF, HK, CDIS, CT
  10  CONTINUE
C
 8500   FORMAT(1X,4F9.5,3F10.6,F10.3,2F10.6)
C
      IF(LWAKE) THEN
        IS = 2
        DO 20 I=N+1, N+NW
          IBL = IBLTE(IS) + I - N
          DS = DSTR(IBL,IS)
          TH = THET(IBL,IS)
          H = DS/TH
          CF = 0.
          UI = UEDG(IBL,IS)
          UE = (UI/QINF)*(1.0-TKLAM) / (1.0 - TKLAM*(UI/QINF)**2)
          AMSQ = UE*UE*HSTINV / (GAMM1*(1.0 - 0.5*UE*UE*HSTINV))
          CALL HKIN( H, AMSQ, HK, DUMMY, DUMMY)
C
          CDIS = DIS(IBL,IS)/QINF**3
          CT = CTAU(IBL,IS)
          WRITE(LU,8500) S(I), X(I), Y(I), UE, DS, TH, CF, HK, CDIS, CT
 20     CONTINUE
      ENDIF
C
      CLOSE(LU)
      RETURN
      END SUBROUTINE BLDUMP



      SUBROUTINE CPDUMP(FNAME1)
      USE XMOD
      CHARACTER*(*) FNAME1
C
 1000 FORMAT(A)
C
      FNAME = FNAME1
C
      LU = 19
      OPEN(LU,FILE=FNAME,STATUS='UNKNOWN')
      REWIND(LU)
C
      WRITE(LU,1000) NAME
      WRITE(LU,1200) ADEG,REINF1,XBF,YBF
 1200 FORMAT(1X,'Alfa = ',F9.5,' Re = ',F12.3,' Xflap,Yflap = ',2F12.6)
C
      WRITE(LU,1000)
     & '#    x        y        Cp  '
C
      CALL COMSET
C
      BETA = SQRT(1.0 - MINF**2)
      BFAC = 0.5*MINF**2 / (1.0 + BETA)
C
      DO 10 I=1, N
        CPINC = 1.0 - (GAM(I)/QINF)**2
        DEN = BETA + BFAC*CPINC
        CPCOM = CPINC / DEN
C
        WRITE(LU,8500) X(I), Y(I), CPCOM
 8500   FORMAT(1X,3F9.5)
  10  CONTINUE
C
      CLOSE(LU)
      RETURN
      END SUBROUTINE CPDUMP



      SUBROUTINE MHINGE(XF,YF)
C----------------------------------------------------
C     Calculates the hinge moment of the flap about
C     (XOF,YOF) by integrating surface pressures.
C----------------------------------------------------
      USE XMOD
C
      IF(.NOT.LFLAP) THEN
        XOF = XF
        YOF = YF
        LFLAP = .TRUE.
      ENDIF
C
C---- find top and bottom y at hinge x location
      TOPS = XOF
      BOTS = S(N) - XOF
      CALL SINVRT(TOPS,XOF,X,XP,S,N)      
      CALL SINVRT(BOTS,XOF,X,XP,S,N)      
C
      TOPX = SEVAL(TOPS,X,XP,S,N)
      TOPY = SEVAL(TOPS,Y,YP,S,N)
      BOTX = SEVAL(BOTS,X,XP,S,N)
      BOTY = SEVAL(BOTS,Y,YP,S,N)
C
      HMOM = 0.
      HFX  = 0.
      HFY  = 0.
C
C---- integrate pressures on top and bottom sides of flap
      DO 20 I=2, N
        IF(S(I-1).GE.TOPS .AND. S(I).LE.BOTS) GO TO 20
C
         DX = X(I) - X(I-1)
         DY = Y(I) - Y(I-1)
         XMID = 0.5*(X(I)+X(I-1)) - XOF
         YMID = 0.5*(Y(I)+Y(I-1)) - YOF
         IF(LVISC) THEN
          PMID = 0.5*(CPV(I) + CPV(I-1))
         ELSE
          PMID = 0.5*(CPI(I) + CPI(I-1))
         ENDIF
         HMOM = HMOM + PMID*(XMID*DX + YMID*DY)
         HFX  = HFX  - PMID* DY
         HFY  = HFY  + PMID* DX
   20 CONTINUE
C
C---- find S(I)..S(I-1) interval containing s=TOPS
      DO I=2, N
        IF(S(I).GT.TOPS) GO TO 31
      ENDDO
C
   31 CONTINUE
C---- add on top surface chunk TOPS..S(I-1),  missed in the DO 20 loop.
      DX = TOPX - X(I-1)
      DY = TOPY - Y(I-1)
      XMID = 0.5*(TOPX+X(I-1)) - XOF
      YMID = 0.5*(TOPY+Y(I-1)) - YOF
      IF(S(I) .NE. S(I-1)) THEN
       FRAC = (TOPS-S(I-1))/(S(I)-S(I-1))
      ELSE
       FRAC = 0.
      ENDIF
      IF(LVISC) THEN
       TOPP = CPV(I)*FRAC + CPV(I-1)*(1.0-FRAC)
       PMID = 0.5*(TOPP+CPV(I-1))
      ELSE
       TOPP = CPI(I)*FRAC + CPI(I-1)*(1.0-FRAC)
       PMID = 0.5*(TOPP+CPI(I-1))
      ENDIF
      HMOM = HMOM + PMID*(XMID*DX + YMID*DY)
      HFX  = HFX  - PMID* DY
      HFY  = HFY  + PMID* DX
C
C---- add on inside flap surface contribution from hinge to top surface
      DX = XOF - TOPX
      DY = YOF - TOPY
      XMID = 0.5*(TOPX+XOF) - XOF
      YMID = 0.5*(TOPY+YOF) - YOF
      HMOM = HMOM + PMID*(XMID*DX + YMID*DY)
      HFX  = HFX  - PMID* DY
      HFY  = HFY  + PMID* DX
C
C---- find S(I)..S(I-1) interval containing s=BOTS
      DO I=N, 2, -1
        IF(S(I-1).LT.BOTS) GO TO 41
      ENDDO
C
   41 CONTINUE
C---- add on bottom surface chunk BOTS..S(I),  missed in the DO 20 loop.
      DX = X(I) - BOTX
      DY = Y(I) - BOTY
      XMID = 0.5*(BOTX+X(I)) - XOF
      YMID = 0.5*(BOTY+Y(I)) - YOF
      IF(S(I) .NE. S(I-1)) THEN
       FRAC = (BOTS-S(I-1))/(S(I)-S(I-1))
      ELSE
       FRAC = 0.
      ENDIF
      IF(LVISC) THEN
       BOTP = CPV(I)*FRAC + CPV(I-1)*(1.0-FRAC)
       PMID = 0.5*(BOTP+CPV(I))
      ELSE
       BOTP = CPI(I)*FRAC + CPI(I-1)*(1.0-FRAC)
       PMID = 0.5*(BOTP+CPI(I))
      ENDIF
      HMOM = HMOM + PMID*(XMID*DX + YMID*DY)
      HFX  = HFX  - PMID* DY
      HFY  = HFY  + PMID* DX
C
C---- add on inside flap surface contribution from hinge to bottom surface
      DX = BOTX - XOF
      DY = BOTY - YOF
      XMID = 0.5*(BOTX+XOF) - XOF
      YMID = 0.5*(BOTY+YOF) - YOF
      HMOM = HMOM + PMID*(XMID*DX + YMID*DY)
      HFX  = HFX  - PMID* DY
      HFY  = HFY  + PMID* DX
C
C---- add on TE base thickness contribution
      DX = X(1) - X(N)
      DY = Y(1) - Y(N)
      XMID = 0.5*(X(1)+X(N)) - XOF
      YMID = 0.5*(Y(1)+Y(N)) - YOF
      IF(LVISC) THEN
       PMID = 0.5*(CPV(1)+CPV(N))
      ELSE
       PMID = 0.5*(CPI(1)+CPI(N))
      ENDIF
      HMOM = HMOM + PMID*(XMID*DX + YMID*DY)
      HFX  = HFX  - PMID* DY
      HFY  = HFY  + PMID* DX
C
      RETURN
      END SUBROUTINE MHINGE


C       SUBROUTINE VPAR
C C---------------------------------------------
C C     Viscous parameter change menu routine.
C C---------------------------------------------
C       USE XMOD
C       USE BLPMOD
C       CHARACTER*4 COMAND
C       CHARACTER*128 COMARG
C C
C       DIMENSION IINPUT(20)
C       DIMENSION RINPUT(20)
C       LOGICAL ERROR
C C
C C
C  10   TURB = 100.0 * EXP( -(ACRIT + 8.43)/2.4 )
C       WRITE(*,1200) XSTRIP(1), XSTRIP(2), ACRIT, TURB, VACCEL,
C      &              SCCON, DUXCON, GACON, GBCON, CTCON, CTRCON, CTRCEX
C  1200 FORMAT(/' Xtr/c     =', F8.4, '    top    side'
C      &       /' Xtr/c     =', F8.4, '    bottom side'
C      &       /' Ncrit     =', F8.2, '   (', F6.3, ' % turb. level )'
C      &       /' Vacc      =', F8.4,
C      &      //' Klag  =', F8.4,'     Uxwt  =', F8.2
C      &       /' A     =', F8.4,'     B     =', F8.4,'       KCt =', F8.5
C      &       /' CtiniK=', F8.4,'     CtiniX=', F8.4 )
C C
C C======================================================================
C C---- start of user interaction loop
C   500 CONTINUE
C       CALL ASKC('..VPAR^',COMAND,COMARG)
C C
C       DO I=1, 20
C         IINPUT(I) = 0
C         RINPUT(I) = 0.0
C       ENDDO
C       NINPUT = 20
C       CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
C       NINPUT = 20
C       CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C C
C C--------------------------------------------------------------
C       IF(COMAND.EQ.'    ') THEN
C        RETURN
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'?   ') THEN
C        WRITE(*,1050)
C  1050  FORMAT(
C      &  /'   <cr>    Return to OPER menu'
C      &  /'   SHOW    Display viscous parameters'
C      &  /'   XTR  rr Change trip positions Xtr/c'
C      &  /'   N    r  Change critical amplification exponent Ncrit'
C      &  /'   VACC r  Change Newton solution acceleration parameter'
C      &  /'   INIT    BL initialization flag toggle'
C      & //'   LAG     change lag equation constants'
C      &  /'   GB      change G-beta constants'
C      &  /'   CTR     change initial transition-Ctau constants'
C      &  /'   REST    restore BL calibration to baseline')
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'SHOW') THEN
C        GO TO 10
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'XTR ') THEN
C        IF(NINPUT.GE.2) THEN
C         XSTRIP(1) = RINPUT(1)
C         XSTRIP(2) = RINPUT(2)
C        ELSE
C        WRITE(*,*) 'Trips are entered in x/c (with xtr>0)'
C        WRITE(*,*) '                  or s/c (with xtr<0)'
C         CALL ASKR('Enter top    side Xtrip/c^',XSTRIP(1))
C         CALL ASKR('Enter bottom side Xtrip/c^',XSTRIP(2))
C        ENDIF
C        LVCONV = .FALSE.
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'N   ') THEN
C        IF(NINPUT.GE.1) THEN
C         ACRIT = RINPUT(1)
C        ELSE
C         CALL ASKR('Enter critical amplification ratio^',ACRIT)
C        ENDIF
C        LVCONV = .FALSE.
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'VACC') THEN
C        IF(NINPUT.GE.1) THEN
C         VACCEL = RINPUT(1)
C        ELSE
C         CALL ASKR('Enter viscous acceleration parameter^',VACCEL)
C        ENDIF
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'INIT') THEN
C        LBLINI = .NOT.LBLINI
C        IF(.NOT.LBLINI) WRITE(*,*)'BLs will be initialized on next point'
C        IF(     LBLINI) WRITE(*,*)'BLs are assumed to be initialized'
C        IF(.NOT.LBLINI) LIPAN = .FALSE.
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'LAG ') THEN
C        IF(NINPUT.GE.2) THEN
C         SCCON  = RINPUT(1)
C         DUXCON = RINPUT(2)
C        ELSE
C         CALL ASKR('Enter shear lag constant^',SCCON)
C         CALL ASKR('Enter shear lag UxEQ weight^',DUXCON)
C        ENDIF
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'GB  ') THEN
C        IF(NINPUT.GE.2) THEN
C         GACON = RINPUT(1)
C         GBCON = RINPUT(2)
C        ELSE
C         CALL ASKR('Enter G-beta constant A^',GACON)
C         CALL ASKR('Enter G-beta constant B^',GBCON)
C        ENDIF
C        CTCON = 0.5/(GACON**2 * GBCON)
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'CTR ') THEN
C        IF(NINPUT.GE.2) THEN
C         CTRCON = RINPUT(1)
C         CTRCEX = RINPUT(2)
C        ELSE
C         CALL ASKR('Enter initial-Ctau constant^',CTRCON)
C         CALL ASKR('Enter initial-Ctau exponent^',CTRCEX)
C        ENDIF
C C
C C--------------------------------------------------------------
C       ELSEIF(COMAND.EQ.'REST') THEN
C        CALL BLPINI
C C
C C--------------------------------------------------------------
C       ELSE
C        WRITE(*,1000) COMAND
C  1000  FORMAT(1X,A4,' command not recognized.  Type a "?" for list')
C C
C       ENDIF
C C
C       GO TO 500
C       END VPAR




      SUBROUTINE SPECAL
C-----------------------------------
C     Converges to specified alpha.
C-----------------------------------
      USE XMOD
      REAL MINF_CLM, MSQ_CLM
C
C---- calculate surface vorticity distributions for alpha = 0, 90 degrees
      IF(.NOT.LGAMU .OR. .NOT.LQAIJ) CALL GGCALC
C
      COSA = COS(ALFA)
      SINA = SIN(ALFA)
C
C---- superimpose suitably weighted  alpha = 0, 90  distributions
      DO 50 I=1, N
        GAM(I)   =  COSA*GAMU(I,1) + SINA*GAMU(I,2)
        GAM_A(I) = -SINA*GAMU(I,1) + COSA*GAMU(I,2)
   50 CONTINUE
      PSIO = COSA*GAMU(N+1,1) + SINA*GAMU(N+1,2)
C
      CALL TECALC
      CALL QISET
C
C---- set initial guess for the Newton variable CLM
      CLM = 1.0
C
C---- set corresponding  M(CLM), Re(CLM)
      CALL MRCL(CLM,MINF_CLM,REINF_CLM)
      CALL COMSET
C
C---- set corresponding CL(M)
      CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &            CL,CM,CDP, CL_ALF,CL_MSQ)
C
C---- iterate on CLM
      DO 100 ITCL=1, 20
C
        MSQ_CLM = 2.0*MINF*MINF_CLM
        DCLM = (CL - CLM)/(1.0 - CL_MSQ*MSQ_CLM)
C
        CLM1 = CLM
        RLX = 1.0
C
C------ under-relaxation loop to avoid driving M(CL) above 1
        DO 90 IRLX=1, 12
C
          CLM = CLM1 + RLX*DCLM
C
C-------- set new freestream Mach M(CLM)
          CALL MRCL(CLM,MINF_CLM,REINF_CLM)
C
C-------- if Mach is OK, go do next Newton iteration
          IF(MATYP.EQ.1 .OR. MINF.EQ.0.0 .OR. MINF_CLM.NE.0.0) GO TO 91
C
          RLX = 0.5*RLX
   90   CONTINUE
   91   CONTINUE
C
C------ set new CL(M)
        CALL COMSET
        CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &              CL,CM,CDP,CL_ALF,CL_MSQ)
C
        IF(ABS(DCLM).LE.1.0E-6) GO TO 110
C
  100 CONTINUE
C      WRITE(*,*) 'SPECAL:  Minf convergence failed'
  110 CONTINUE
C
C---- set final Mach, CL, Cp distributions, and hinge moment
      CALL MRCL(CL,MINF_CL,REINF_CL)
      CALL COMSET
      CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &            CL,CM,CDP, CL_ALF,CL_MSQ)
      CALL CPCALC(N,QINV,QINF,MINF,CPI)
      IF(LVISC) THEN
       CALL CPCALC(N+NW,QVIS,QINF,MINF,CPV)
       CALL CPCALC(N+NW,QINV,QINF,MINF,CPI)
      ELSE
       CALL CPCALC(N,QINV,QINF,MINF,CPI)
      ENDIF
      IF(LFLAP) CALL MHINGE(0.0, 0.0)
C
      RETURN
      END SUBROUTINE SPECAL
 
 
      SUBROUTINE SPECCL
C-----------------------------------------
C     Converges to specified inviscid CL.
C-----------------------------------------
      USE XMOD
C
C---- calculate surface vorticity distributions for alpha = 0, 90 degrees
      IF(.NOT.LGAMU .OR. .NOT.LQAIJ) CALL GGCALC
C
C---- set freestream Mach from specified CL -- Mach will be held fixed
      CALL MRCL(CLSPEC,MINF_CL,REINF_CL)
      CALL COMSET
C
C---- current alpha is the initial guess for Newton variable ALFA
      COSA = COS(ALFA)
      SINA = SIN(ALFA)
      DO 10 I=1, N
        GAM(I)   =  COSA*GAMU(I,1) + SINA*GAMU(I,2)
        GAM_A(I) = -SINA*GAMU(I,1) + COSA*GAMU(I,2)
   10 CONTINUE
      PSIO = COSA*GAMU(N+1,1) + SINA*GAMU(N+1,2)
C
C---- get corresponding CL, CL_alpha, CL_Mach
      CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &            CL,CM,CDP, CL_ALF,CL_MSQ)
C
C---- Newton loop for alpha to get specified inviscid CL
      DO 100 ITAL=1, 20
C
        DALFA = (CLSPEC - CL) / CL_ALF
        RLX = 1.0
C
        ALFA = ALFA + RLX*DALFA
C
C------ set new surface speed distribution
        COSA = COS(ALFA)
        SINA = SIN(ALFA)
        DO 40 I=1, N
          GAM(I)   =  COSA*GAMU(I,1) + SINA*GAMU(I,2)
          GAM_A(I) = -SINA*GAMU(I,1) + COSA*GAMU(I,2)
   40   CONTINUE
        PSIO = COSA*GAMU(N+1,1) + SINA*GAMU(N+1,2)
C
C------ set new CL(alpha)
        CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &              CL,CM,CDP,CL_ALF,CL_MSQ)
C
        IF(ABS(DALFA).LE.1.0E-6) GO TO 110
  100 CONTINUE
C      WRITE(*,*) 'SPECCL:  CL convergence failed'
  110 CONTINUE
C
C---- set final surface speed and Cp distributions
      CALL TECALC
      CALL QISET
      IF(LVISC) THEN
       CALL CPCALC(N+NW,QVIS,QINF,MINF,CPV)
       CALL CPCALC(N+NW,QINV,QINF,MINF,CPI)
      ELSE
       CALL CPCALC(N,QINV,QINF,MINF,CPI)
      ENDIF
      IF(LFLAP) CALL MHINGE(0.0, 0.0)
C
      RETURN
      END SUBROUTINE SPECCL


      SUBROUTINE VISCAL(NITER1)
C----------------------------------------
C     Converges viscous operating point
C----------------------------------------
      USE XMOD
C
C---- convergence tolerance
      DATA EPS1 / 1.0E-4 /
C
      NITER = NITER1
C
C---- calculate wake trajectory from current inviscid solution if necessary
      IF(.NOT.LWAKE) THEN
       CALL XYWAKE
      ENDIF
C
C---- set velocities on wake from airfoil vorticity for alpha=0, 90
      CALL QWCALC
C
C---- set velocities on airfoil and wake for initial alpha
      CALL QISET
C
      IF(.NOT.LIPAN) THEN
C
       IF(LBLINI) CALL GAMQV
C
C----- locate stagnation point arc length position and panel index
       CALL STFIND
C
C----- set  BL position -> panel position  pointers
       CALL IBLPAN
C
C----- calculate surface arc length array for current stagnation point location
       CALL XICALC
C
C----- set  BL position -> system line  pointers
       CALL IBLSYS
C
      ENDIF
C
C---- set inviscid BL edge velocity UINV from QINV
      CALL UICALC
C
      IF(.NOT.LBLINI) THEN
C
C----- set initial Ue from inviscid Ue
       DO IBL=1, NBL(1)
         UEDG(IBL,1) = UINV(IBL,1)
       ENDDO
C
       DO IBL=1, NBL(2)
         UEDG(IBL,2) = UINV(IBL,2)
       ENDDO
C
      ENDIF
C
      IF(LVCONV) THEN
C----- set correct CL if converged point exists
       CALL QVFUE
       IF(LVISC) THEN
        CALL CPCALC(N+NW,QVIS,QINF,MINF,CPV)
        CALL CPCALC(N+NW,QINV,QINF,MINF,CPI)
       ELSE
        CALL CPCALC(N,QINV,QINF,MINF,CPI)
       ENDIF
       CALL GAMQV
       CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &             CL,CM,CDP, CL_ALF,CL_MSQ)
       CALL CDCALC
      ENDIF
C
C---- set up source influence matrix if it doesn't exist
      IF(.NOT.LWDIJ .OR. .NOT.LADIJ) CALL QDCALC
C
C---- Newton iteration for entire BL solution
C       WRITE(*,*)
C       WRITE(*,*) 'Solving BL system ...'
      DO 1000 ITER=1, NITER
C
C------ fill Newton system for BL variables
        CALL SETBL
C
C------ solve Newton system with custom solver
        CALL BLSOLV
C
C------ update BL variables
        CALL UPDATE
C
        IF(LALFA) THEN
C------- set new freestream Mach, Re from new CL
         CALL MRCL(CL,MINF_CL,REINF_CL)
         CALL COMSET
        ELSE
C------- set new inviscid speeds QINV and UINV for new alpha
         CALL QISET
         CALL UICALC
        ENDIF
C
C------ calculate edge velocities QVIS(.) from UEDG(..)
        CALL QVFUE
C
C------ set GAM distribution from QVIS
        CALL GAMQV
C
C------ relocate stagnation point
        CALL STMOVE
C
C------ set updated CL,CD
        CALL CLCALC(N,X,Y,GAM,GAM_A,ALFA,MINF,QINF, XCMREF,YCMREF,
     &              CL,CM,CDP,CL_ALF,CL_MSQ)
        CALL CDCALC
C
C------ display changes and test for convergence
C         IF(RLX.LT.1.0) 
C      &   WRITE(*,2000) ITER, RMSBL, RMXBL, VMXBL,IMXBL,ISMXBL,RLX
C         IF(RLX.EQ.1.0) 
C      &   WRITE(*,2010) ITER, RMSBL, RMXBL, VMXBL,IMXBL,ISMXBL
        CDP = CD - CDF
C        WRITE(*,2020) ALFA/DTOR, CL, CM, CD, CDF, CDP
C
        IF(RMSBL .LT. EPS1) THEN
         LVCONV = .TRUE.
         AVISC = ALFA
         MVISC = MINF
         GO TO 90
        ENDIF
C
 1000 CONTINUE
C      WRITE(*,*) 'VISCAL:  Convergence failed'
C
   90 CONTINUE
      CALL CPCALC(N+NW,QINV,QINF,MINF,CPI)
      CALL CPCALC(N+NW,QVIS,QINF,MINF,CPV)
      IF(LFLAP) CALL MHINGE(0.0, 0.0)
      RETURN
C....................................................................
C  2000   FORMAT
C      &   (/1X,I3,'   rms: ',E10.4,'   max: ',E10.4,3X,A1,' at ',I4,I3,
C      &     '   RLX:',F6.3)
C  2010   FORMAT
C      &   (/1X,I3,'   rms: ',E10.4,'   max: ',E10.4,3X,A1,' at ',I4,I3)
C  2020   FORMAT
C      &   ( 1X,3X,'   a =', F7.3,'      CL =',F8.4  /
C      &     1X,3X,'  Cm =', F8.4, '     CD =',F9.5,
C      &           '   =>   CDf =',F9.5,'    CDp =',F9.5)
      END SUBROUTINE VISCAL