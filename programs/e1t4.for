C
C  PROGRAM E1T4
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 1999
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK D2O EX.1)ZED-2 Task 4 FOR 1 LIBRARY
C
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS
C     (FOR PITCH 22 CM,COOLANT=D2O,AIR AND PITCH 24 CM,COOLANT=D2O,AIR)
C     <- E1T4.REF
C
C  2) READ WIMS-D RESULTS-IOW
C                          1 <-E1T4D22D.OUT;
C                          3 <-E1T4A22D.OUT;
C                          5 <-E1T4D28D.OUT;
C                          7 <-E1T4A28D.OUT;
C  2.1) READ Nth2,Nu5
C  2.2) READ Keff
C  2.3) READ TOTAL REACTION RATES BY MATERIAL, OF:
C       Th-232(2232) AND U-235(2235)
C
C  3) CALCUL (Keff-1)*100000
C  4) CALCUL FAST FISSION RATIO FFR=FIS.Th2*Nth2/FIS.U5*Nu5  (MATER.6)
C  5) CALCUL REL.CONV.RATIO C=(ABS.-FIS.)Th2*Nth2/FIS.U5*Nu5     "
C  6) WRITE RESULTS
C
C***********************************************************************
      DIMENSION FFRE(5,8),CE(5,8),RU5FRE(5,4),RCAE(5,4),ALMRE(5,4)
      DIMENSION FFR(5,8),C(5,8),RU5FR(5,5),RCA(5,5),ALMR(5,5)
      DIMENSION FFRR(5,8),CR(5,8),RU5FRR(5,5),RCAR(5,5),ALMRR(5,5)
      DIMENSION AU5(8),AU8(8)
      DIMENSION AKAEFF(8),AKAM1(8)
      DIMENSION ABSO(5,5,8),FIS(5,5,8)
      CHARACTER*3 DVOID,DINEX,DDLOW,ANA(5,8)
      CHARACTER*12 OUTW(8)
      CHARACTER*24 REFER1
      CHARACTER*28 AISOT(5)
      CHARACTER*30 REFER2
      CHARACTER*44 ADVICE
      CHARACTER*80 TIT,REF,TIT2
      CHARACTER*80 LINE
C
C***********************************************************************
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS
C     (FOR PITCH 22 CM,COOLANT=D2O,AIR AND PITCH 24 CM,COOLANT=D2O,AIR)
C     <- E1T4.REF
C
      OPEN(1,FILE='E1T4.REF')
      read(1,1)TIT
      read(1,1)REF
       read(1,1)TIT2
       read(1,2)FFRE(1,1)
       read(1,3)SDFF
       read(1,2)CE(1,1)
       read(1,3)SDC
       read(1,1)TIT2
       read(1,2)FFRE(1,3)
       read(1,3)SDFF
       read(1,2)CE(1,3)
       read(1,3)SDC
       read(1,1)TIT2
       read(1,2)FFRE(1,5)
       read(1,3)SDFF
       read(1,2)CE(1,5)
       read(1,3)SDC
       read(1,1)TIT2
       read(1,2)FFRE(1,7)
       read(1,3)SDFF
       read(1,2)CE(1,7)
       read(1,3)SDC
      CLOSE(1)
C
C***********************************************************************
C  2) READ WIMS-D RESULTS-IOW
C                          1 <-E1T4D22D.OUT
C                          3 <-E1T4A22D.OUT
C                          5 <-E1T4D28D.OUT
C                          7 <-E1T4A28D.OUT
C
      OUTW(1)="E1T4D22D.OUT"
      OUTW(3)="E1T4A22D.OUT"
      OUTW(5)="E1T4D28D.OUT"
      OUTW(7)="E1T4A28D.OUT"
C
      AISOT(1)='element  2232 reaction rates'
      AISOT(2)='element  2235 reaction rates'
C
      OPEN(3,FILE="E1T4.TEM")
      DO IOW=1,7,2
       OPEN(1,FILE=OUTW(IOW))
C
C 2.1) READ Nth,Nu5
C
       read(1,47)LINE
       DO WHILE(LINE(1:7).NE.'  2232 ')
        read(1,47)LINE
       END DO
       BACKSPACE(1)
       read(1,48)AU5(IOW)
       read(1,47)LINE
       DO WHILE(LINE(1:7).NE.'  2235 ')
        read(1,47)LINE
       END DO
       BACKSPACE(1)
       read(1,48)AU8(IOW)
C
C  2.2) READ Keff
C
        read(1,47)LINE
        DO WHILE(LINE(53:63).NE.'k-effective')
         read(1,47)LINE
        END DO
        BACKSPACE(1)
        read(1,49)AKAEFF(IOW)
        write(3,50)AKAEFF(IOW)
C
C  2.3) READ TOTAL REACTION RATES BY MATERIAL, OF:
C       Th-232(2232) AND U-235(2235)
C
C -------------------------------------------------------------------
       DO ISOT=1,2
C     ISOT=1: ABS.AND FIS.R.Th232
C     ISOT=2: ABS.AND FIS.R.U235
C
        read(1,47)LINE
        DO WHILE(LINE(2:29).NE.AISOT(ISOT))
         read(1,47)LINE
        END DO
C
        read(1,47)LINE
        DO WHILE(LINE(2:13).NE.'group materl')
         read(1,47)LINE
        END DO
C
        read(1,47)LINE
        DO WHILE(LINE(2:8).NE.'  total')
         read(1,47)LINE
        END DO
         BACKSPACE(1)
         read(1,51)ABSO(ISOT,1,IOW)
C
        read(1,47)LINE
        DO WHILE(LINE(2:8).NE.'fission')
         read(1,47)LINE
        END DO
C
        read(1,47)LINE
        DO WHILE(LINE(2:8).NE.'  total')
         read(1,47)LINE
        END DO
        BACKSPACE(1)
        read(1,51)FIS(ISOT,1,IOW)
C
       END DO
C
       CLOSE(1)
      END DO
C***********************************************************************
C  3) CALCUL (Keff-1)*100000
C
      DO IOW=1,7,2
       AKAM1(IOW)=(AKAEFF(IOW)-1.0)*100000.0
      END DO
C***********************************************************************
C  4) CALCUL FAST FISSION RATIO FFR=FIS.Th2*Nth2/FIS.U5*Nu5  (MATER.6)
C  5) CALCUL REL.CONV.RATIO C=(ABS.-FIS.)Th2*Nth2/FIS.U5*Nu5     "
C
      DO IOW=1,7,2
C
       AU8DU5 = AU5(IOW)/AU8(IOW)
       write(3,52)AU5(IOW),AU8(IOW)
C
       write(3,53)IOW,(ABSO(K,1,IOW),K=1,2)
       write(3,54)IOW,(FIS(K,1,IOW),K=1,2)
C
       FFR(1,IOW)   = FIS(1,1,IOW)*AU8DU5/FIS(2,1,IOW)
       FFRR(1,IOW)  = 100.0*((FFR(1,IOW)/FFRE(1,IOW))-1.0)
C
       C(1,IOW)  = ((ABSO(1,1,IOW)-FIS(1,1,IOW))*AU8DU5/FIS(2,1,IOW))
       CR(1,IOW) = 100.0*((C(1,IOW)/CE(1,IOW))-1.0)
C
      END DO
C***********************************************************************
C  6) WRITE RESULTS
C
  1   FORMAT(A80)
  2   FORMAT(5E13.5)
  3   FORMAT(E13.5)
  4   FORMAT(4E13.5)
  5   FORMAT(A44)
 23   FORMAT(A24)
 25   FORMAT(A30)
 37   FORMAT('----------------------------------------------'/
     1       'TABLE 22'/
     2       'Effective Multiplication Factor'/
     3       '(Keff-1.00000)*100000; unit:pcm'//
     4       'PITCH(cm) COOLANT METHOD')
 38   FORMAT('  22      D2O     DSN   ',F6.0,A3/
     2       '          AIR     DSN   ',F6.0,A3/
     4       '  28      D2O     DSN   ',F6.0,A3/
     6       '          AIR     DSN   ',F6.0,A3//)
 39   FORMAT('----------------------------------------------'/
     1       'TABLE 23'/
     2       'Th232/U235 Fission Ratio'/
     3       'Reference Experimental Values'//
     4       'PITCH(cm) COOLANT    FA')
 40   FORMAT('  22        D2O  ',F9.5/
     1       '            AIR  ',F9.5/
     2       '  28        D2O  ',F9.5/
     3       '            AIR  ',F9.5//)
 41   FORMAT('----------------------------------------------'/
     1       'TABLE 24'/
     2       'Th232/U235 Fission Ratio'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F5.1,' %)'//
     4       'PITCH(cm) COOLANT METHOD')
 42   FORMAT('  22      D2O     DSN   ',F5.1,A3/
     2       '          AIR     DSN   ',F5.1,A3/
     4       '  28      D2O     DSN   ',F5.1,A3/
     6       '          AIR     DSN   ',F5.1,A3//)
 43   FORMAT('----------------------------------------------'/
     1       'TABLE 25'/
     2       'Th232-CAPT/U235-FISS.Ratio'/
     3       'Reference Experimental Values'//
     4       'PITCH(cm) COOLANT    FA')
 44   FORMAT('  22        D2O  ',F6.3/
     1       '            AIR  ',F6.3/
     2       '  28        D2O  ',F6.3/
     3       '            AIR  ',F6.3//)
 45   FORMAT('----------------------------------------------'/
     1       'TABLE 26'/
     2       'Th232-CAPT/U235-FISS.Ratio'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F3.1,' %)'//
     4       'PITCH(cm) COOLANT METHOD')
 46   FORMAT('  22      D2O     DSN   ',F7.2,A3/
     2       '          AIR     DSN   ',F7.2,A3/
     4       '  28      D2O     DSN   ',F7.2,A3/
     6       '          AIR     DSN   ',F7.2,A3//)
 47   FORMAT(A80)
 48   FORMAT(61X,E11.4)
 49   FORMAT(64X,E13.6)
 50   FORMAT("AKAEFF ",E13.6)
 51   FORMAT(77X,E13.5)
 52   FORMAT("ATh2,AU5",2E12.5)
 53   FORMAT("IOW,ABSO",I3,2E12.5)
 54   FORMAT("IOW,FIS ",I3,2E12.5)
C
      REFER1 ='(*):WITHIN EXPERIM.ERROR'
      DVOID= "   "
      DINEX= "(*)"
      DDLOW= "(v)"
C
      OPEN(2,FILE='E1T4.LIS')
      write(2,1)TIT(1:31)
      write(2,1)REF
      write(2,1)
      write(2,23)REFER1
      write(2,1)
C
C TABLE 22
C
      ANA(1,1)=DVOID
      ANA(1,3)=DVOID
      ANA(1,5)=DVOID
      ANA(1,7)=DVOID
      write(2,37)
      WRITE(2,38)(AKAM1(IOW),ANA(1,IOW),IOW=1,7,2)
C
C TABLE 23
C
      write(2,39)
      WRITE(2,40)FFRE(1,1),FFRE(1,3),FFRE(1,5),FFRE(1,7)
C
C TABLE 24
C
      DO J=1,7,2
       IF(ABS(FFRR(1,J)).LT.SDFF)ANA(1,J)=DINEX
      ENDDO
      write(2,41)SDFF
      WRITE(2,42)(FFRR(1,J),ANA(1,J),J=1,7,2)
C
C TABLE 25
C
      write(2,43)
      WRITE(2,44)CE(1,1),CE(1,3),CE(1,5),CE(1,7)
C
C TABLE 26
C
      DO J=1,7,2
       IF(ABS(CR(1,J)).LT.SDC)ANA(1,J)=DINEX
      ENDDO
      write(2,45)SDC
      WRITE(2,46)(CR(1,J),ANA(1,J),J=1,7,2)
C
      CLOSE(2)
      CLOSE(3)
C
      STOP 'E1T4 Completed'
      END
