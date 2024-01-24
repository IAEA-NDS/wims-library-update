C
C  PROGRAM MTRBURN
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 2001
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK MTR-BURN FOR ONE LIBRARY
C
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS FROM MTRBURN.REF;
C
C  2) READ WIMS-D RESULTS FROM MTRBURN.OUT:
C      READ percent of U235(2235) vs.Burnup;
C
C  3) WRITE RESULTS ON MTRBURN.LST
C
C***********************************************************************
      DIMENSION IDAYS(17),IDENT(17),IPOS(17),IDD(100)
      DIMENSION PU5E(17),PU5C(17),ATDEN(17),ATDENC(17),ADD(100)
C
      CHARACTER*5 AA
      CHARACTER*6 NAME(17)
      CHARACTER*16 BB
      CHARACTER*80,TIT,REF,TIT2
      CHARACTER*120 LINE
C
C***********************************************************************
C
      I25=2235
      TIT2=''
C
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS
C
      OPEN(1,FILE='MTRBURN.REF')
      read(1,1)TIT
      DO I=1,9
       read(1,1)REF
      ENDDO
C
      DO N=1,17
       READ(1,2)IDAYS(N),PU5E(N)
      ENDDO
C
      DO I=1,4
       read(1,1)REF
      ENDDO
C
      DO N=1,17
       READ(1,10)NAME(N),IDENT(N),ATDEN(N)
      ENDDO
      CLOSE(1)
C
C***********************************************************************
C  2) READ WIMS-D RESULTS
C
      OPEN(1,FILE='MTRBURN.OUT')
      OPEN(2,FILE='MTRBURN.LIS')
C
C  READ percent of U235(2235) vs.Burnup;
C
      N=0
 100  IF(N.EQ.17)GOTO 300
      READ(1,3,END=300)AA
      IF(AA.EQ.' days')THEN
       BACKSPACE(1)
       READ(1,4)IBUR
       IF(IDAYS(N+1).EQ.IBUR)THEN
        N=N+1
 200    READ(1,5,END=300)BB
        NIAN2=0
        IF(BB.EQ.' weight percents')THEN
         NIAN2=1
         READ(1,1)REF
         NIAN=0
 250     READ(1,6)IAA,ANN
         IF(IAA.EQ.I25)THEN
          PU5C(N)=ANN
          NIAN=1
         ENDIF
         IF(NIAN.EQ.0)GOTO 250
        ENDIF
        IF(NIAN2.EQ.0)GOTO 200
       ENDIF
      ENDIF
      GOTO 100
 300  CONTINUE
      N=0
      DO I=1,17
       IPOS(I)=0
      ENDDO
 301  BACKSPACE(1)
      BACKSPACE(1)
      READ(1,11)LINE
      IF(LINE(2:17).EQ.'number densities')THEN
       I1=1
       I2=10
       NLIN=0
 302   READ(1,11)LINE
       IF(LINE(1:3).EQ.'   ')THEN
        BACKSPACE(1)
        READ(1,12)(IDD(I),I=I1,I2)
        NLIN=NLIN+1
        DO II=I1,I2
         DO J=1,17
          IF(IDD(II).EQ.IDENT(J))THEN
           IPOS(J)=II
           N=N+1
          ENDIF
         ENDDO
        ENDDO
        I1=I2+1
        I2=I2+10
        GO TO 302
       ENDIF
      ENDIF
      IF(N.EQ.0)GOTO 301
      BACKSPACE(1)
      NLIN1=NLIN*10
      READ(1,13)(ADD(IJ),IJ=1,NLIN1)
      NNN=N
      DO IK=1,17
       IF(IPOS(IK).NE.0) THEN
         ATDENC(IK)=ADD(IPOS(IK))
       ELSE
         ATDENC(IK)=0.0
       ENDIF
      ENDDO
      CLOSE(1)
C
C***********************************************************************
C  3) WRITE RESULTS
C
      write(2,1)TIT
      WRITE(2,1)
      write(2,9)
C
      DO N=1,17
       WRITE(2,7)N,PU5E(N)
       DIFP=PU5C(N)-PU5E(N)
       WRITE(2,8)PU5C(N),DIFP
      ENDDO
C
      WRITE(2,1)
      WRITE(2,14)
      DIFN=0.0
      DO N=1,17
       WRITE(2,15)NAME(N),ATDEN(N),DIFN
       IF(IPOS(N).NE.0)THEN
        DIFE=ATDENC(N)-ATDEN(N)
       ELSE
        DIFE=0.0
       ENDIF
       WRITE(2,16)ATDENC(N),DIFE
      ENDDO
C
      CLOSE(2)
C
  1   FORMAT(A80)
  2   FORMAT(25X,I5,7X,F5.1)
  3   FORMAT(13X,A5)
  4   FORMAT(3X,I6)
  5   FORMAT(A16)
  6   FORMAT(I6,E12.5)
  7   FORMAT(' EXP  ',I4,F6.1)
  8   FORMAT(10X,2F6.1)
  9   FORMAT(5X,'Cycle %U235 DIFF.')
 10   FORMAT(A6,1X,I5,2X,E9.2)
 11   FORMAT(A120)
 12   FORMAT(I8,9I12)
 13   FORMAT(10E12.5)
 14   FORMAT('NAME       AT.DENS.(EOCycle17) CALC.-REF.')
 15   FORMAT(A6,5X,1PE9.2,2X,F9.2)
 16   FORMAT(11X,1PE9.2,2X,1PE9.2)
C
      STOP 'Mtrburn Completed'
      END
