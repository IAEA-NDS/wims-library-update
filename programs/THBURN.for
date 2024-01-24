C
C  PROGRAM THBURN
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 2002
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK PWR THORIUM PIN CELL BURNUP
C
C  1) READ REFERENCE VALUES OF PARAMETERS FROM THBURN.REF
C
C  2) READ WIMS-D RESULTS FROM THBURN.OUT
C
C  3) CALCUL RELATIONS AND WRITE RESULTS
C
C***********************************************************************
      DIMENSION BURNUP(20),AKAIN(20),AKACAL(20),ATDENS(20),RELAT(10)
      DIMENSION IDAYS(20),MATER(20),IMATC(3)
      DIMENSION DENATC(3),DENCAL(20,20),DIFK(20),DIFDEN(20,20)
      DIMENSION REL(10),DIFR(10)
C
      CHARACTER*6 SIMBOL(20)
      CHARACTER*11 TITREL(10),SIMB
      CHARACTER*80,TIT,TIT1,TIT2,TIT3
      CHARACTER*120 LINE
C
C***********************************************************************
C
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS
C
      OPEN(1,FILE='THBURN.REF')
      read(1,1)TIT
      read(1,1)REF
C
      read(1,1)TIT1
      DO I=1,11
       read(1,2)BURNUP(I),IDAYS(I),AKAIN(I)
      ENDDO
C
      read(1,1)TIT2
      DO I=1,16
       read(1,3)SIMBOL(I),MATER(I),ATDENS(I)
      ENDDO
C
      read(1,1)TIT3
      DO I=1,4
       read(1,4)TITREL(I),RELAT(I)
      ENDDO
C
      DO I=1,16
       DENCAL(I,1)=0.0
      END DO
      read(1,1)TIT3
      read(1,4)SIMB,DENCAL(1,1)
      read(1,4)SIMB,DENCAL(6,1)
      read(1,4)SIMB,DENCAL(7,1)
      read(1,4)SIMB,DENCAL(9,1)
C
      CLOSE(1)
C
C***********************************************************************
C  2) READ WIMS-D RESULTS
C
      OPEN(1,FILE='THBURN.OUT')
C
C       CHECK IF LIBRARY WIMS86
C
        read(1,'(A120)')LINE
        IW86=0
        DO WHILE(LINE(16:34).NE.'entry into chain  2')
         read(1,'(A120)')LINE
         IF(LINE(1:43).EQ.'    129    69    25    14    13    17    55')
     1    IW86=1
        END DO
        CLOSE(1)
C
      OPEN(1,FILE='THBURN.OUT')
C
  50  READ(1,5,END=300)LINE
      IF(LINE(26:35).NE.'k-infinity')GOTO 50
      BACKSPACE(1)
      READ(1,6)AKACAL(1)
C
      DO N=1,12
       DO I=1,16
        DENCAL(I,N)=0.0
       ENDDO
      ENDDO
C
  60  N=1
  70  IF(N.EQ.12)GOTO 300
  80  READ(1,5,END=300)LINE
      IF(LINE(2:4).NE.'t= ')GOTO 80
      BACKSPACE(1)
      READ(1,7)IDAYSC
      IF(IDAYSC.LT.IDAYS(N+1))GOTO 80
      N=N+1
  90  READ(1,5,END=300)LINE
      IF(LINE(1:9).NE.' material')GOTO 90
      M=0
  95  READ(1,8)(IMATC(J),DENATC(J),J=1,3)
      DO J=1,3
       DO I=1,16
        IF(IMATC(J).EQ.MATER(I))THEN
         DENCAL(I,N)=DENATC(J)
         M=M+1
        ENDIF
       ENDDO
      ENDDO
      IF(IW86.EQ.0.AND.M.LT.16)GOTO 95
      IF(IW86.EQ.1.AND.M.LT.14)GOTO 95
 100  READ(1,5,END=300)LINE
      IF(LINE(26:35).NE.'k-infinity')GOTO 100
      BACKSPACE(1)
      READ(1,6)AKACAL(N)
      GOTO 70
 300  continue
      CLOSE(1)
C
C***********************************************************************
C  3) CALCUL RATIOS AND WRITE RESULTS
C
C     K-INF
      DO I=1,11
       DIFK(I)=(AKACAL(I)-AKAIN(I))*100.0/AKAIN(I)
      ENDDO
C
C    ISOTOPE CONCENTRATIONS
      DO J=2,11
       DO I=1,16
        DIFDEN(I,J)=(DENCAL(I,J)-ATDENS(I))*100.0/ATDENS(I)
       ENDDO
      ENDDO
C
      REL(1)=DENCAL(2,10)+DENCAL(3,10)+DENCAL(5,10)+DENCAL(7,10)+
     1 DENCAL(10,10)+DENCAL(11,10)+DENCAL(13,10)+DENCAL(15,10)
      DIFR(1)=(REL(1)-RELAT(1))*100.0/RELAT(1)
C
      ACT0=0.0
      ACT10=0.0
      DO I=1,16
       ACT0=ACT0+DENCAL(I,1)
       ACT10=ACT10+DENCAL(I,10)
      ENDDO
      REL(2)=(ACT0-ACT10)/ACT0
      DIFR(2)=(REL(2)-RELAT(2))*100.0/RELAT(2)
C
      FERT1=DENCAL(1,1)+DENCAL(9,1)
      FERT10=DENCAL(1,10)+DENCAL(9,10)
      REL(3)=(FERT1-FERT10)/FERT1
      DIFR(3)=(REL(3)-RELAT(3))*100.0/RELAT(3)
C
      THDEP=(DENCAL(1,10)-DENCAL(1,1))/DENCAL(1,1)
      U8DEP=(DENCAL(9,10)-DENCAL(9,1))/DENCAL(9,1)
      REL(4)=THDEP/U8DEP
      DIFR(4)=(REL(4)-RELAT(4))*100.0/RELAT(4)
C ---------------------------------------------
      OPEN(3,FILE="THBURN.LST")
C
C*
       WRITE(3,1)TIT
       WRITE(3,693)' ================================================='
       WRITE(3,1)
       WRITE(3,16)' Processed Benchmark analysis reports :           '
       WRITE(3,16)'                              1 case  : thburn.lst'
       WRITE(3,1)
       WRITE(3,14)
       WRITE(3,693)' ================================================='
C
       DO I=1,11
        WRITE(3,9)BURNUP(I),AKAIN(I)
        WRITE(3,10)AKACAL(I),DIFK(I)
        WRITE(3,13)
       ENDDO
       WRITE(3,693)' ================================================='
       WRITE(3,719)' Average  '
       WRITE(3,719)' Dif/St.Dv'
       WRITE(3,718)' # '
C
       WRITE(3,1)TIT
       WRITE(3,693)' ================================================='
       WRITE(3,1)
       WRITE(3,16)' Processed Benchmark analysis reports :           '
       WRITE(3,16)'                              1 case  : thburn.lst'
       WRITE(3,1)
       WRITE(3,15)
       WRITE(3,693)' ================================================='
       DO I=1,16
        WRITE(3,11)SIMBOL(I),ATDENS(I)
        WRITE(3,10)DENCAL(I,10),DIFDEN(I,10)
        WRITE(3,13)
       ENDDO
       WRITE(3,693)' ================================================='
       WRITE(3,719)' Average  '
       WRITE(3,719)' Dif/St.Dv'
C
C      DO I=1,4
C       WRITE(3,12)TITREL(I),RELAT(I)
C       WRITE(3,10)REL(I),DIFR(I)
C       WRITE(3,13)
C      ENDDO
C
      CLOSE(3)
C
  1   FORMAT(A80)
  2   FORMAT(F11.3,I7,F11.5)
  3   FORMAT(A6,I5,E12.5)
  4   FORMAT(A11,E12.5)
  5   FORMAT(A120)
  6   FORMAT(36X,E13.6)
  7   FORMAT(4X,I5)
  8   FORMAT(3(I6,3X,E14.7))
  9   FORMAT(F11.3,1X,1PE12.5,' (   0.0 )')
 10   FORMAT(12X,1PE12.5,' (',0PF7.2,')')
 11   FORMAT(5X,A6,1X,1PE12.5,' (   0.0 )')
 12   FORMAT(1X,A11,1PE12.5,' (   0.0 )')
 13   FORMAT(' ')
 14   FORMAT(' B(MWd/kgHM) Kinfinity   diff(%)')
 15   FORMAT('    ISOTOPE  AtomDens.   diff(%)')
 16   FORMAT(A50)
 693  FORMAT(A50)
 718  FORMAT(A4)
 719  FORMAT(A10)
C
      STOP 'Completed'
      END
