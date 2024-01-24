C
C  PROGRAM E1T2
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 1999
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK D2O EX.1)ZED-2 Task 2 FOR 1 LIBRARY
C
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS (FOR 24 AND 40 CM PITCHS)
C     <- E1T2.REF
C
C  2) READ WIMS-D RESULTS-IOW
C                          1 <-E1T2P24D.OUT;
C                          3 <-E1T2P40D.OUT;
C  2.1) READ Nu5,Nu8
C  2.2) READ Keff
C  2.3) READ TOTAL REACTION RATES BY MATERIAL, OF:
C       U-235(2235) AND U-238(8238)
C
C  3) CALCUL (Keff-1)*100000
C  4) CALCUL FAST FISSION RATIO        FFR  =FIS.U8/FIS.U5 (MATER.6)
C  5) CALCUL RELATIVE CONVERSION RATIO C    =ABS.U8/FIS.U5     "
C  6) WRITE RESULTS
C
C***********************************************************************
      DIMENSION FFRE(5,4),CE(5,4),RU5FRE(5,4),RCAE(5,4),ALMRE(5,4)
      DIMENSION FFR(5,5),C(5,5),RU5FR(5,5),RCA(5,5),ALMR(5,5)
      DIMENSION FFRR(5,5),CR(5,5),RU5FRR(5,5),RCAR(5,5),ALMRR(5,5)
      DIMENSION AU5(5),AU8(5)
      DIMENSION AKAEFF(4),AKAM1(4)
      DIMENSION ABSO(5,5,5),FIS(5,5,5)
      DIMENSION ANR(4)
      CHARACTER*3 DVOID,DINEX,DDLOW,ANA(5,4)
      CHARACTER*12 OUTW(5)
      CHARACTER*24 REFER1
      CHARACTER*28 AISOT(5)
      CHARACTER*30 REFER2
      CHARACTER*44 ADVICE
      CHARACTER*80 TIT,REF,TIT2(2)
      CHARACTER*80 LINE
C
C***********************************************************************
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS (FOR 24 AND 40 CM PITCHS)
C
      OPEN(1,FILE='E1T2.REF')
      read(1,1)TIT
      read(1,1)REF
      DO IC=1,2
       read(1,1)TIT2(IC)
       read(1,2)FFRE(1,IC)
       read(1,3)SDFF
       read(1,2)CE(1,IC)
       read(1,3)SDC
      END DO
      CLOSE(1)
C
      IC=3
       FFRE(1,IC)=FFRE(1,IC-1)
       CE(1,IC)=CE(1,IC-1)
C***********************************************************************
C  2) READ WIMS-D RESULTS: 1)<-E1T2P24D.OUT
C                          3)<-E1T2P40D.OUT
C
      OUTW(1)="E1T2P24D.OUT"
      OUTW(3)="E1T2P40D.OUT"
C
      AISOT(1)='element  2235 reaction rates'
      AISOT(2)='element  8238 reaction rates'
C
      OPEN(3,FILE="E1T2.TEM")
      DO IOW=1,3,2
       OPEN(1,FILE=OUTW(IOW))
C
C 2.1) READ Nu5,Nu8
C
       read(1,35)LINE
       DO WHILE(LINE(1:7).NE.'  2235 ')
        read(1,35)LINE
       END DO
       BACKSPACE(1)
       read(1,36)AU5(IOW)
       read(1,35)LINE
       DO WHILE(LINE(1:7).NE.'  8238 ')
        read(1,35)LINE
       END DO
       BACKSPACE(1)
       read(1,36)AU8(IOW)
C
C  2.2) READ Keff
C
        read(1,35)LINE
        DO WHILE(LINE(53:63).NE.'k-effective')
         read(1,35)LINE
        END DO
        BACKSPACE(1)
        read(1,37)AKAEFF(IOW)
        write(3,38)AKAEFF(IOW)
C
C  2.3) READ TOTAL REACTION RATES BY MATERIAL, OF:
C       U-235(2235) AND U-238(8238)
C
C -------------------------------------------------------------------
       DO ISOT=1,2
C     ISOT=4: ABS.AND FIS.R.U235
C     ISOT=5: ABS.AND FIS.R.U238
C
        read(1,35)LINE
        DO WHILE(LINE(2:29).NE.AISOT(ISOT))
         read(1,35)LINE
        END DO
C
        read(1,35)LINE
        DO WHILE(LINE(2:13).NE.'group materl')
         read(1,35)LINE
        END DO
C
        read(1,35)LINE
        DO WHILE(LINE(2:8).NE.'  total')
         read(1,35)LINE
        END DO
        IF(IOW.LT.5)THEN
         BACKSPACE(1)
         read(1,39)ABSO(ISOT,1,IOW)
        ENDIF
C
        read(1,35)LINE
        DO WHILE(LINE(2:8).NE.'fission')
         read(1,35)LINE
        END DO
C
        read(1,35)LINE
        DO WHILE(LINE(2:8).NE.'  total')
         read(1,35)LINE
        END DO
         BACKSPACE(1)
         read(1,39)FIS(ISOT,1,IOW)
C
       END DO
C
       CLOSE(1)
      END DO
C***********************************************************************
C  3) CALCUL (Keff-1)*100000
C
      DO IOW=1,3,2
       AKAM1(IOW)=(AKAEFF(IOW)-1.0)*100000.0
      END DO
C***********************************************************************
C  3) CALCUL (Keff-1)*100000
C  4) CALCUL FAST FISSION RATIO        FFR  =FIS.U8/FIS.U5 (MATER.6)
C  5) CALCUL RELATIVE CONVERSION RATIO C    =ABS.U8/FIS.U5     "
C
      DO IOW=1,3,2
C
       AU8DU5 = AU8(IOW)/AU5(IOW)
       write(3,40)AU5(IOW),AU8(IOW)
C
       write(3,41)IOW,(ABSO(K,1,IOW),K=1,2)
       write(3,42)IOW,(FIS(K,1,IOW),K=1,2)
C
       FFR(1,IOW)   = FIS(2,1,IOW)*AU8DU5/FIS(1,1,IOW)
       FFRR(1,IOW)  = 100.0*((FFR(1,IOW)/FFRE(1,IOW))-1.0)
C
       C(1,IOW)  = ((ABSO(2,1,IOW)-FIS(2,1,IOW))*AU8DU5/ABSO(1,1,IOW))
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
 27   FORMAT('----------------------------------------------'/
     1       'TABLE 14'/
     2       'Effective Multiplication Factor'/
     3       '(Keff-1.00000)*100000; unit:pcm'//
     4       'PITCH(cm) METHOD')
 28   FORMAT('  24      DSN   ',F6.0,A3/
     2       '  40      DSN   ',F6.0,A3//)
 29   FORMAT('----------------------------------------------'/
     1       'TABLE 15'/
     2       'Fast Fission Ratio'/
     3       'Reference Experimental Values'//
     4       'PITCH(cm)    FA')
 30   FORMAT('  24     ',F8.4/
     1       '  40     ',F8.4//)
 31   FORMAT('----------------------------------------------'/
     1       'TABLE 16'/
     2       'Fast Fission Ratio'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F4.1,' %)'//
     4       'PITCH(cm) Method    FA')
 32   FORMAT('  24      DSN   ',F7.1,A3/
     2       '  40      DSN   ',F7.1,A3//)
 33   FORMAT('----------------------------------------------'/
     1       'TABLE 17'/
     2       'Initial Conversion Ratio'/
     3       'Reference Experimental Values'//
     4       'PITCH(cm)    FA')
 34   FORMAT('----------------------------------------------'/
     1       'TABLE 18'/
     2       'Initial Conversion Ratio'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F5.2,' %)'//
     4       'PITCH(cm) Method    FA')
 35   FORMAT(A80)
 36   FORMAT(61X,E11.4)
 37   FORMAT(64X,E13.6)
 38   FORMAT("AKAEFF ",E13.6)
 39   FORMAT(77X,E13.5)
 40   FORMAT("AU5,AU8",2E12.5)
 41   FORMAT("IOW,ABSO",I3,5E12.5)
 42   FORMAT("IOW,FIS ",I3,5E12.5)
C
      REFER1 ='(*):WITHIN EXPERIM.ERROR'
      DVOID= "   "
      DINEX= "(*)"
      DDLOW= "(v)"
C
      OPEN(2,FILE='E1T2.LIS')
      write(2,1)TIT(1:31)
      write(2,1)REF
      write(2,1)
      write(2,23)REFER1
      write(2,1)
C
C TABLE 14
C
      DO I=1,3,2
       ANA(1,I)=DVOID
      ENDDO
      write(2,27)
      WRITE(2,28)(AKAM1(IOW),ANA(1,IOW),IOW=1,3,2)
C
C TABLE 15
C
      write(2,29)
      WRITE(2,30)FFRE(1,1),FFRE(1,3)
C
C TABLE 16
C
      DO J=1,3,2
       ANA(1,J)=DVOID
      ENDDO
      DO J=1,3,2
       IF(ABS(FFRR(1,J)).LT.SDFF)ANA(1,J)=DINEX
      ENDDO
      write(2,31)SDFF
      WRITE(2,32)(FFRR(1,J),ANA(1,J),J=1,3,2)
C
C TABLE 17
C
      write(2,33)
      WRITE(2,30)CE(1,1),CE(1,3)
C
C TABLE 18
C
      DO J=1,3,2
        ANA(1,J)=DVOID
      ENDDO
      DO J=1,3,2
       IF(ABS(CR(1,J)).LT.SDC)ANA(1,J)=DINEX
      ENDDO
      write(2,34)SDC
      WRITE(2,32)(CR(1,J),ANA(1,J),J=1,3,2)
C
      CLOSE(2)
      CLOSE(3)
C
      STOP 'E1T2 Completed'
      END
