C
C  PROGRAM E1T3
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 1999
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK D2O EX.1)ZED-2 Task # FOR 1 LIBRARY
C
C  1) READ WIMS-D RESULTS-IOW
C                          1 <-E1T3000D.OUT;
C  2) READ Keff
C  3) CALCUL (Keff-1)*100000
C  4) WRITE RESULTS
C
C***********************************************************************
      DIMENSION AKAEFF(4),AKAM1(4)
      CHARACTER*3 DVOID,DDLOW,ANA(5,4)
      CHARACTER*12 OUTW(5)
      CHARACTER*30 REFER2
      CHARACTER*44 ADVICE
      CHARACTER*80 TIT,REF
      CHARACTER*80 LINE
C
C***********************************************************************
C  1) READ WIMS-D RESULTS: 1)<-E1T3000D.OUT
C
      OUTW(1)="E1T3000D.OUT"
C
      OPEN(3,FILE="E1T3.TEM")
      IOW=1
      OPEN(1,FILE=OUTW(IOW))
C
C  2) READ Keff
C
        read(1,37)LINE
        DO WHILE(LINE(53:63).NE.'k-effective')
         read(1,37)LINE
        END DO
        BACKSPACE(1)
        read(1,38)AKAEFF(IOW)
        write(3,39)AKAEFF(IOW)
C
      CLOSE(1)
C***********************************************************************
C  3) CALCUL (Keff-1)*100000
C
       AKAM1(IOW)=(AKAEFF(IOW)-1.0)*100000.0
C***********************************************************************
C  4) WRITE RESULTS
C
  1   FORMAT(A80)
  2   FORMAT(5E13.5)
  3   FORMAT(E13.5)
  4   FORMAT(4E13.5)
  5   FORMAT(A44)
 25   FORMAT(A30)
 35   FORMAT('----------------------------------------------'/
     1       'TABLE 20'/
     2       'Effective Multiplication Factor'/
     3       '(Keff-1.00000)*100000; unit:pcm'//
     4       'METHOD')
 36   FORMAT(' DSN  ',F6.0,A3//)
 37   FORMAT(A80)
 38   FORMAT(64X,E13.6)
 39   FORMAT("AKAEFF ",E13.6)
C
      TIT=' BENCHMARK EX.1-ZED 2 - TASK 3'
      REF=' REF.= R.T.jones, AECL-5853,1977'
C
      DVOID= "   "
      DDLOW= "(v)"
C
      OPEN(2,FILE='E1T3.LIS')
      write(2,1)TIT
      write(2,1)REF
      write(2,1)
      write(2,1)
C
C TABLE 20
C
      ANA(1,1)=DVOID
      write(2,35)
      WRITE(2,36)AKAM1(IOW),ANA(1,IOW)
C
      CLOSE(2)
      CLOSE(3)
C
      STOP 'E1T3 Completed'
      END
