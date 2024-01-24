C
C  PROGRAM E1T1
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 1999
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK D2O EX.1)ZED-2 Task 1 FOR 1 LIBRARY
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS (FOR D2O AND AIR COOLANTS)
C     <- E1T1.REF
C
C  2) READ WIMS-D RESULTS-IOW
C                          1 <-E1T1D2OD.OUT;
C                          2 <-E1T1AIRD.OUT;
C                          3 <-E1T1THER.OUT
C  2.1) READ Nu5,Nu8
C  2.2) READ Keff     (FOR IOW=1-4)
C  2.3) READ TOTAL REACTION RATES AND REACTIONS, BY MATERIAL, OF:
C       MN-55(55),CU-63(3063),LU-176(176),U-235(2235) AND U-238(8238)
C
C  3) CALCUL (Keff-1)*100000
C  4) CALCUL FAST FISSION RATIO FFR  =FIS.U8/FIS.U5 (MATERS.6-9 + F.AV.)
C  5) CALCUL RELATIVE CONVERSION RATIO C=ABS.U8/FIS.U5          "
C  6) CALCUL RELATIVE U5 FISSION RATE  RU5FR=FIS.U5        (MATERS.6-9)
C  7) CALCUL RELATIVE CU ACTIVITY RCA  =ABS.R.CU63 (MATERS.6-9 + M.AV.)
C  8) CALCUL LUTETIUM-MANGANESE RATIOS ALMR  =ABS.R.LU176/ABS.R.MN55
C                                                   (MATERS.6-9 + F.AV.)
C  9) WRITE RESULTS
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
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS (FOR D2O AND AIR COOLANTS)
C
      OPEN(UNIT=1,FILE='E1T1.REF',FORM='FORMATTED',STATUS='OLD')
      read(1,1)TIT
      read(1,1)REF
      DO IC=1,2
       read(1,1)TIT2(IC)
       read(1,2)(FFRE(I,IC),I=1,5)
       read(1,3)SDFF
       read(1,2)(CE(I,IC),I=1,5)
       read(1,3)SDC
       read(1,4)(RU5FRE(I,IC),I=1,4)
       read(1,3)SDRU5
       read(1,2)(RCAE(I,IC),I=1,5)
       read(1,3)SDRCA
       read(1,2)(ALMRE(I,IC),I=1,5)
       read(1,3)SDAL
      END DO
      CLOSE(1)
      CLOSE(2)
C
      IC=3
      DO I=1,5
      FFRE(I,IC)=FFRE(I,IC-1)
      CE(I,IC)=CE(I,IC-1)
      RU5FRE(I,IC)=RU5FRE(I,IC-1)
      RCAE(I,IC)=RCAE(I,IC-1)
      ALMRE(I,IC)=ALMRE(I,IC-1)
      END DO
C***********************************************************************
C  2) READ WIMS-D RESULTS: 1)<-E1T1D2OD.OUT
C                          3)<-E1T1AIRD.OUT
C                          5)<-E1T1THER.OUT
C
      OUTW(1)="E1T1D2OD.OUT"
      OUTW(3)="E1T1AIRD.OUT"
      OUTW(5)="E1T1THER.OUT"
C
      AISOT(1)='element    55 reaction rates'
      AISOT(2)='element  3063 reaction rates'
      AISOT(3)='element   176 reaction rates'
      AISOT(4)='element  2235 reaction rates'
      AISOT(5)='element  8238 reaction rates'
C
      OPEN(3,FILE="E1T1.TEM")
      DO IOW=1,5,2
       OPEN(1,FILE=OUTW(IOW))
C
C 2.1) READ Nu5,Nu8
C
       read(1,30)LINE
       DO WHILE(LINE(1:7).NE.'  2235 ')
        read(1,30)LINE
       END DO
       BACKSPACE(1)
       read(1,31)AU5(IOW)
       read(1,30)LINE
       DO WHILE(LINE(1:7).NE.'  8238 ')
        read(1,30)LINE
       END DO
       BACKSPACE(1)
       read(1,31)AU8(IOW)
C
       IF(IOW.LT.5)THEN
C
C  2.2) READ Keff
C
        read(1,30)LINE
        DO WHILE(LINE(53:63).NE.'k-effective')
         read(1,30)LINE
        END DO
        BACKSPACE(1)
        read(1,32)AKAEFF(IOW)
        write(3,40)AKAEFF(IOW)
       ENDIF
C
C  2.3) READ TOTAL REACTION RATES AND REACTIONS, BY MATERIAL, OF:
C       MN-55(55),CU-63(3063),LU-176(176),U-235(2235) AND U-238(8238)
C
C -------------------------------------------------------------------
       DO ISOT=1,5
C     ISOT=1: ABS.R.MN55
C     ISOT=2: ABS.R.CU63
C     ISOT=3: ABS.R.LU176
C     ISOT=4: ABS.AND FIS.R.U235
C     ISOT=5: ABS.AND FIS.R.U238
C
        read(1,30)LINE
        DO WHILE(LINE(2:29).NE.AISOT(ISOT))
         read(1,30)LINE
        END DO
C
        read(1,30)LINE
        DO WHILE(LINE(2:13).NE.'group materl')
         read(1,30)LINE
        END DO
C
        read(1,30)LINE
        DO WHILE(LINE(2:8).NE.'  total')
         read(1,30)LINE
        END DO
        IF(IOW.LT.5)THEN
         BACKSPACE(1)
         IF(ISOT.EQ.2)THEN
          read(1,33)ABSO(ISOT,5,IOW),
     1                                   (ABSO(ISOT,I,IOW),I=1,3)
         ELSE
          read(1,34)(ABSO(ISOT,I,IOW),I=1,3)
         ENDIF
        ENDIF
C
        read(1,30)LINE
        DO WHILE(LINE(2:8).NE.'  total')
         read(1,30)LINE
        END DO
        BACKSPACE(1)
        IF(IOW.LT.5)THEN
         read(1,35)ABSO(ISOT,4,IOW)
        ELSE
         BACKSPACE(1)
         read(1,36)ABSO(ISOT,1,IOW)
        ENDIF
C
        IF(ISOT.GT.3)THEN
         read(1,30)LINE
         DO WHILE(LINE(2:8).NE.'fission')
          read(1,30)LINE
         END DO
C
         read(1,30)LINE
         DO WHILE(LINE(2:8).NE.'  total')
          read(1,30)LINE
         END DO
         IF(IOW.LT.5)THEN
          BACKSPACE(1)
          read(1,34)(FIS(ISOT,I,IOW),I=1,3)
         ENDIF
C
         read(1,30)LINE
         DO WHILE(LINE(2:8).NE.'  total')
          read(1,30)LINE
         END DO
         BACKSPACE(1)
         IF(IOW.LT.5)THEN
          read(1,35)FIS(ISOT,4,IOW)
         ELSE
          BACKSPACE(1)
          read(1,36)FIS(ISOT,1,IOW)
         ENDIF
        ENDIF
C
        REWIND(1)
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
C  4) CALCUL FAST FISSION RATIO FFR  =FIS.U8/FIS.U5 (MATERS.6-9 + F.AV.)
C  5) CALCUL RELATIVE CONVERSION RATIO C=ABS.U8/FIS.U5(MATERS.6-9+F.AV.)
C  6) CALCUL RELATIVE U5 FISSION RATE  RU5FR=FIS.U5        (MATERS.6-9)
C  7) CALCUL RELATIVE CU ACTIVITY RCA  =ABS.R.CU63 (MATERS.6-9 + M.AV.)
C  8) CALCUL LUTETIUM-MANGANESE RATIOS ALMR  =ABS.R.LU176/ABS.R.MN55
C                                                   (MATERS.6-9 + F.AV.)
C
      ANR(1)=1.0
      ANR(2)=6.0
      ANR(3)=12.0
      ANR(4)=18.0
      ANRT=0.0
      DO I=1,4
       ANRT=ANRT+ANR(I)
      ENDDO
C
C    THERMAL VALUES
C
      CP0   = ((ABSO(5,1,5)-FIS(5,1,5))/FIS(4,1,5))
      ALMRP = (ABSO(3,1,5)/ABSO(1,1,5))
      write(3,41)CP0,ALMRP
C
      DO IOW=1,3,2
C
       AU8DU5 = AU8(IOW)/AU5(IOW)
       write(3,42)AU5(IOW),AU8(IOW)
       FFR(5,IOW)=0.0
       C(5,IOW)=0.0
       RU5FR(5,IOW)=0.0
       RCA(5,IOW)=0.0
       ALMR(5,IOW)=0.0
C
       DO I=1,4
        write(3,43)IOW,I,(ABSO(K,I,IOW),K=1,5)
        write(3,44)IOW,I,(FIS(K,I,IOW),K=1,5)
C
        FFR(I,IOW)   = FIS(5,I,IOW)*AU8DU5/FIS(4,I,IOW)
        FFR(5,IOW)   = FFR(5,IOW)+FFR(I,IOW)*ANR(I)
        FFRR(I,IOW)  = 100.0*((FFR(I,IOW)/FFRE(I,IOW))-1.0)
C
        C(I,IOW)     = ((ABSO(5,I,IOW)-FIS(5,I,IOW))/FIS(4,I,IOW))/CP0
        C(5,IOW)     = C(5,IOW)+C(I,IOW)*ANR(I)
        CR(I,IOW)    = 100.0*((C(I,IOW)/CE(I,IOW))-1.0)
C
        RU5FR(I,IOW) = FIS(4,I,IOW)
        RU5FR(5,IOW) = RU5FR(5,IOW)+RU5FR(I,IOW)*ANR(I)
C
        RCA(I,IOW)   = ABSO(2,I,IOW)*0.692*4.5/(4.5*0.692+2.17*0.308)
        RCA(5,IOW)   = RCA(5,IOW)+RCA(I,IOW)*ANR(I)
C
        ALMR(I,IOW)  = (ABSO(3,I,IOW)/ABSO(1,I,IOW))/ALMRP
        ALMR(5,IOW)  = ALMR(5,IOW)+ALMR(I,IOW)*ANR(I)
        ALMRR(I,IOW) = 100.0*((ALMR(I,IOW)/ALMRE(I,IOW))-1.0)
C
       END DO
C
       FFR(5,IOW)   = FFR(5,IOW)/ANRT
       FFRR(5,IOW)  = 100.0*((FFR(5,IOW)/FFRE(5,IOW))-1.0)
       C(5,IOW)     = C(5,IOW)/ANRT
       CR(5,IOW)    = 100.0*((C(5,IOW)/CE(5,IOW))-1.0)
       RU5FR(5,IOW) = RU5FR(5,IOW)/ANRT
       RCA(5,IOW)   = RCA(5,IOW)/ANRT
       ALMR(5,IOW)  = ALMR(5,IOW)/ANRT
       ALMRR(5,IOW) = 100.0*((ALMR(5,IOW)/ALMRE(5,IOW))-1.0)
C
       DO I=1,4
        RU5FR(I,IOW)  = RU5FR(I,IOW)/RU5FR(5,IOW)
        RU5FRR(I,IOW) = 100.0*((RU5FR(I,IOW)/RU5FRE(I,IOW))-1.0)
        RCA(I,IOW)    = RCA(I,IOW)/RCA(5,IOW)
        RCAR(I,IOW)   = 100.0*((RCA(I,IOW)/RCAE(I,IOW))-1.0)
       END DO
C
C  MODERATOR AV.
C
       RCA5=RCA(5,IOW)
       RCA(5,IOW)  = ABSO(2,5,IOW)*0.692*4.5/(4.5*0.692+2.17*0.308)
       RCA(5,IOW)  = RCA(5,IOW)/RCA5
       RCAR(5,IOW) = 100.0*((RCA(5,IOW)/RCAE(5,IOW))-1.0)
C
      END DO
C***********************************************************************
C  9) WRITE RESULTS
C
C
      REFER1 ='(*):WITHIN EXPERIM.ERROR'
      DVOID= "   "
      DINEX= "(*)"
      DDLOW= "(v)"
C
      OPEN(2,FILE='E1T1.LIS')
      write(2,1)TIT(1:31)
      write(2,1)REF
      write(2,1)
      write(2,23)REFER1
      write(2,1)
C
C TABLE 2
C
      DO I=1,3,2
       ANA(1,I)=DVOID
      ENDDO
      write(2,6)
      WRITE(2,7)(AKAM1(IOW),ANA(1,IOW),IOW=1,3,2)
C
C TABLE 3
C
      write(2,8)
      WRITE(2,9)(FFRE(I1,1),I1=1,5),(FFRE(I3,3),I3=1,5)
C
C TABLE 4
C
      DO J=1,3,2
       DO I=1,5
        ANA(I,J)=DVOID
       ENDDO
      ENDDO
      DO J=1,3,2
       DO I=1,5
        IF(ABS(FFRR(I,J)).LT.SDFF)ANA(I,J)=DINEX
       ENDDO
      ENDDO
      write(2,10)SDFF
      WRITE(2,11)((FFRR(I,J),ANA(I,J),I=1,5),J=1,3,2)
C
C TABLE 5
C
      write(2,12)
      WRITE(2,9)(CE(I1,1),I1=1,5),(CE(I3,3),I3=1,5)
C
C TABLE 6
C
      DO J=1,3,2
       DO I=1,5
        ANA(I,J)=DVOID
       ENDDO
      ENDDO
      DO J=1,3,2
       DO I=1,5
        IF(ABS(CR(I,J)).LT.SDC)ANA(I,J)=DINEX
       ENDDO
      ENDDO
      write(2,14)SDC
      WRITE(2,15)((CR(I,J),ANA(I,J),I=1,5),J=1,3,2)
C
C TABLE 7
C
      write(2,16)
      WRITE(2,17)(RU5FRE(I1,1),I1=1,4),(RU5FRE(I3,3),I3=1,4)
C
C TABLE 8
C
      DO J=1,3,2
       DO I=1,5
        ANA(I,J)=DVOID
       ENDDO
      ENDDO
      DO J=1,3,2
       DO I=1,4
        IF(ABS(CR(I,J)).LT.SDRU5)ANA(I,J)=DINEX
       ENDDO
      ENDDO
      write(2,18)SDRU5
      WRITE(2,19)((RU5FRR(I,J),ANA(I,J),I=1,4),J=1,3,2)
C
C TABLE 9
C
      write(2,20)
      WRITE(2,21)(RCAE(I1,1),I1=1,5),(RCAE(I3,3),I3=1,5)
C
C TABLE 10
C
      DO J=1,3,2
       DO I=1,5
        ANA(I,J)=DVOID
       ENDDO
      ENDDO
      DO J=1,3,2
       DO I=1,5
        IF(ABS(RCAR(I,J)).LT.SDRCA)ANA(I,J)=DINEX
       ENDDO
      ENDDO
      write(2,22)SDRCA
      WRITE(2,15)((RCAR(I,J),ANA(I,J),I=1,5),J=1,3,2)
C
C TABLE 11
C
      write(2,24)
      WRITE(2,21)(ALMRE(I1,1),I1=1,5),(ALMRE(I3,3),I3=1,5)
C
C TABLE 12
C
      DO J=1,3,2
       DO I=1,5
        ANA(I,J)=DVOID
       ENDDO
      ENDDO
      DO J=1,3,2
       DO I=1,5
        IF(ABS(ALMRR(I,J)).LT.SDAL)ANA(I,J)=DINEX
       ENDDO
      ENDDO
      write(2,26)SDAL
      WRITE(2,15)((ALMRR(I,J),ANA(I,J),I=1,5),J=1,3,2)
C
      CLOSE(2)
      CLOSE(3)
C
  1   FORMAT(A80)
  2   FORMAT(5E13.5)
  3   FORMAT(E13.5)
  4   FORMAT(4E13.5)
  5   FORMAT(A44)
  6   FORMAT('----------------------------------------------'/
     1       'TABLE 2'/
     2       'Effective Multiplication Factor'/
     3       '(Keff-1.00000)*100000; unit:pcm'//
     4       'COOLANT METHOD')
  7   FORMAT('D2O     DSN   ',F6.0,A3/
     2       'AIR     DSN   ',F6.0,A3//)
  8   FORMAT('----------------------------------------------'/
     1       'TABLE 3'/
     2       'Fast Fission Ratio'/
     3       'Reference Experimental Values'//
     4       'Coolant    A       B       C       D       FA')
  9   FORMAT('D2O    ',5F8.4/
     1       'AIR    ',5F8.4//)
 10   FORMAT('----------------------------------------------'/
     1       'TABLE 4'/
     2       'Fast Fission Ratio'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F4.1,' %)'//
     4       'Coolant Method     A        B        C        D       FA')
 11   FORMAT('D2O     DSN   ',5(F6.1,A3)/
     2       'AIR     DSN   ',5(F6.1,A3)//)
 12   FORMAT('----------------------------------------------'/
     1       'TABLE 5'/
     2       'Initial Conversion Ratio'/
     3       'Reference Experimental Values'//
     4       'Coolant    A       B       C       D       FA')
 14   FORMAT('----------------------------------------------'/
     1       'TABLE 6'/
     2       'Initial Conversion Ratio'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F5.2,' %)'//
     4       'Coolant Method     A         B         C         D
     5 FA')
 15   FORMAT('D2O     DSN   ',5(F7.2,A3)/
     2       'AIR     DSN   ',5(F7.2,A3)//)
 16   FORMAT('----------------------------------------------'/
     1       'TABLE 7'/
     2       'U-235 Fission Rate'/
     3       'Reference Experimental Values'//
     4       'Coolant   A      B      C      D')
 17   FORMAT('D2O    ',4F7.3/
     1       'AIR    ',4F7.3//)
 18   FORMAT('----------------------------------------------'/
     1       'TABLE 8'/
     2       'U-235 Fission Rate'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F4.1,' %)'//
     4       'Coolant Method     A         B         C         D')
 19   FORMAT('D2O     DSN   ',4(F7.2,A3)/
     2       'AIR     DSN   ',4(F7.2,A3)//)
 20   FORMAT('----------------------------------------------'/
     1       'TABLE 9'/
     2       'Relative Copper Activity'/
     3       'Reference Experimental Values'//
     4       'Coolant   A      B      C      D      MA')
 21   FORMAT('D2O    ',5F7.3/
     1       'AIR    ',5F7.3//)
 22   FORMAT('----------------------------------------------'/
     1       'TABLE 10'/
     2       'Relative Copper Activity'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F4.1,' %)'//
     4       'Coolant Method    A        B        C        D        MA')
 23   FORMAT(A24)
 24   FORMAT('----------------------------------------------'/
     1       'TABLE 11'/
     2       'Lutetium-Manganese Activity Ratio'/
     3       'Reference Experimental Values'//
     4       'Coolant   A      B      C      D      FA')
 25   FORMAT(A30)
 26   FORMAT('----------------------------------------------'/
     1       'TABLE 12'/
     2       'Lutetium-Manganese Activity Ratio'/
     3       'D=(calc/exp-1)*100'/'(Dexp.=',F4.1,' %)'//
     4       'Coolant Method    A        B        C        D        FA')
 30   FORMAT(A80)
 31   FORMAT(61X,E11.4)
 32   FORMAT(64X,E13.6)
 33   FORMAT(25X,E13.5,39X,3E13.5)
 34   FORMAT(77X,3E13.5)
 35   FORMAT(12X,E13.5,26X)
 36   FORMAT(51X,E13.5)
 40   FORMAT('AKAEFF ',E13.6)
 41   FORMAT('CP0,ALMRP',2E12.5)
 42   FORMAT('AU5,AU8',2E12.5)
 43   FORMAT('IOW,I,ABSO',2I3,5E12.5)
 44   FORMAT('IOW,I,FIS',2I3,5E12.5)
C
      STOP 'Completed'
      END
