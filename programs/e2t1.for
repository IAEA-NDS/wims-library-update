C
C  PROGRAM E2T1
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 1999
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK D2O EX.2)DCA Task 1 FOR ONE LIBRARY
C
C  1) READ WIMS-D RESULTS-IOW
C                          1 <-E2T1D22D.OUT;
C                          3 <-E2T1D25D.OUT;
C                          5 <-E2T1A22D.OUT;
C                          7 <-E2T1A25D.OUT;
C                          9 <-E2T1H22D.OUT;
C                         11 <-E2T1H22D.OUT;
C  2) READ Keff
C  3) CALCUL (Keff-1)*100000
C  4) WRITE RESULTS
C
C***********************************************************************
      DIMENSION AKAEFF(12),AKAM1(12)
      CHARACTER*3 DVOID,DDLOW,ANA(5,12)
      CHARACTER*12 OUTW(12)
      CHARACTER*30 REFER2
      CHARACTER*44 ADVICE
      CHARACTER*80 TIT,REF
      CHARACTER*80 LINE
C
C***********************************************************************
C  1) READ WIMS-D RESULTS
C
      OUTW(1)= "E2T1D22D.OUT"
      OUTW(3)= "E2T1A22D.OUT"
      OUTW(5)= "E2T1H22D.OUT"
      OUTW(7)= "E2T1D25D.OUT"
      OUTW(9)= "E2T1A25D.OUT"
      OUTW(11)="E2T1H25D.OUT"
C
      OPEN(3,FILE="E2T1.TEM")
      DO IOW=1,11,2
       OPEN(1,FILE=OUTW(IOW))
C
C  2) READ Keff
C
        read(1,49)LINE
        DO WHILE(LINE(53:63).NE.'k-effective')
         read(1,49)LINE
        END DO
        BACKSPACE(1)
        read(1,50)AKAEFF(IOW)
        write(3,51)AKAEFF(IOW)
C
       CLOSE(1)
      END DO
C***********************************************************************
C  3) CALCUL (Keff-1)*100000
C
      DO IOW=1,11,2
       AKAM1(IOW)=(AKAEFF(IOW)-1.0)*100000.0
      END DO
C***********************************************************************
C  4) WRITE RESULTS
C
  1   FORMAT(A80)
  2   FORMAT(5E13.5)
  3   FORMAT(E13.5)
  4   FORMAT(4E13.5)
  5   FORMAT(A44)
 25   FORMAT(A30)
 47   FORMAT('----------------------------------------------'/
     1       'TABLE 28'/
     2       'Effective Multiplication Factor'/
     3       '(Keff-1.00000)*100000; unit:pcm'//
     4       'PITCH(cm) COOLANT METHOD')
 48   FORMAT(' 22.5       D2O   DSN  ',F8.0,A3/
     2       '            AIR   DSN  ',F8.0,A3/
     4       '            H2O   DSN  ',F8.0,A3/
     6       ' 25.0       D2O   DSN  ',F8.0,A3/
     8       '            AIR   DSN  ',F8.0,A3/
     A       '            H2O   DSN  ',F8.0,A3//)
 49   FORMAT(A80)
 50   FORMAT(64X,E13.6)
 51   FORMAT("AKAEFF ",E13.6)
C
      TIT=' BENCHMARK EX.2-DCA - TASK 1'
      REF=' REF.= K.Shiba, Nucl.Sci.and Engn.65,492-507,1978'
C
      DVOID= "   "
      DDLOW= "(v)"
C
      OPEN(2,FILE='E2T1.LIS')
      write(2,1)TIT
      write(2,1)REF
      write(2,1)
      write(2,1)
C
C TABLE 28
C
      ANA(1,1)=DVOID
      ANA(1,3)=DVOID
      ANA(1,5)=DVOID
      ANA(1,7)=DVOID
      ANA(1,9)=DVOID
      ANA(1,11)=DVOID
      write(2,47)
      WRITE(2,48)(AKAM1(IOW),ANA(1,IOW),IOW=1,11,2)
C
      CLOSE(2)
      CLOSE(3)
C
      STOP 'E2T1 Completed'
      END
