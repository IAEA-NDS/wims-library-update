C
C  PROGRAM E3
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 1999
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK D2O EX.3)NPD FOR ONE LIBRARY
C
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS
C
C  2) READ WIMS-D RESULTS-IOW
C                          1 <-E300D2OD.OUT;
C      READ Nu5,Nu6,Nu8,NPu9,NPu0,NPu1,NPu2 vs.Burnup
C
C  3) CALCUL ISOTOPIC CONCENTRATION RATIOS FOR THE FUEL PINS OF EACH
C     RING AND BUNDLE AVERAGES AND WRITE RESULTS
C
C***********************************************************************
      DIMENSION R25A28(8,4,2),AN25(8,4,2),AN28(8,4,2)
      DIMENSION R26A28(8,4,2),AN26(8,4,2),R49A28(8,4,2),AN49(8,4,2)
      DIMENSION R40A49(8,4,2),AN40(8,4,2),R41A49(8,4,2),AN41(8,4,2)
      DIMENSION R42A49(8,4,2),AN42(8,4,2),QQ(8)
      DIMENSION R25A28E(8,4)
      DIMENSION R26A28E(8,4),R49A28E(8,4)
      DIMENSION R40A49E(8,4),R41A49E(8,4)
      DIMENSION R42A49E(8,4),QQE(8)
      DIMENSION DR25A28(8,4,2)
      DIMENSION DR26A28(8,4,2),DR49A28(8,4,2)
      DIMENSION DR40A49(8,4,2),DR41A49(8,4,2)
      DIMENSION DR42A49(8,4,2),DQQ(8)
      DIMENSION xmid(6),dxmid(6)
      DIMENSION IQQ(8),IQQE(8)
      DIMENSION IAA(3)
      DIMENSION ANN(3)
C
      CHARACTER*6 AA
      CHARACTER*8 BB
      CHARACTER*12 OUTW(8)
      CHARACTER*44 ADVICE
      CHARACTER*80,TIT,REF,TIT2
      CHARACTER*120 LINE
C
C***********************************************************************
      QQ(1) =   980.000
      QQ(2) =  1500.000
      QQ(3) =  3250.000
      QQ(4) =  5000.000
      QQ(5) =  6500.000
      QQ(6) =  7900.000
      QQ(7) =  9100.000
      QQ(8) = 10800.000
C
      IQQ(1) =   980
      IQQ(2) =  1500
      IQQ(3) =  3250
      IQQ(4) =  5000
      IQQ(5) =  6500
      IQQ(6) =  7900
      IQQ(7) =  9100
      IQQ(8) = 10800
C
      I25=2235
      I26= 236
      I28=8238
      I49=6239
      I40=1240
      I41=1241
      I42=1242
C
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS
C
      OPEN(1,FILE='E3.REF')
      read(1,1)TIT
      read(1,1)REF
C
      read(1,1)TIT2
      READ(1,6)(QQE(N),N=1,8)
      DO M=1,4
       read(1,1)TIT2
       DO N=1,8
        READ(1,7)R25A28E(N,M),R26A28E(N,M),
     1   R49A28E(N,M),R40A49E(N,M),R41A49E(N,M),R42A49E(N,M)
       ENDDO
      ENDDO
      CLOSE(1)
C
      DO N=1,8
       DQQ(N)=((QQ(N)/QQE(N))-1.0)*100.0
       IQQE(N)=QQE(N)
      ENDDO
C***********************************************************************
C  2) READ WIMS-D RESULTS
C
      OUTW(1)="E300D2OD.OUT"
      DQ=0.01
      IOW=1
      OPEN(1,FILE=OUTW(IOW))
C
C  READ Nu5,Nu6,Nu8,NPu9,NPu0,NPu1,NPu2 vs.Burnup
C
  98  N=0
 100  IF(N.EQ.8)GOTO 300
      READ(1,8,END=300)AA
      IF(AA.EQ.'MWd/te')THEN
       BACKSPACE(1)
       READ(1,9)Q
       IF((QQ(N+1)-Q).LT.DQ)THEN
        N=N+1
        M=0
 200    IF(M.EQ.3)GOTO 100
        READ(1,10,END=300)BB
        IF(BB.EQ.'material')THEN
         M=M+1
         NIAN=0
 250     READ(1,12)(IAA(IAN),ANN(IAN),IAN=1,3)
         DO IAN=1,3
          IF(IAA(IAN).EQ.I25)THEN
           AN25(N,M,IOW)=ANN(IAN)
           NIAN=NIAN+1
          ENDIF
          IF(IAA(IAN).EQ.I26)THEN
           AN26(N,M,IOW)=ANN(IAN)
           NIAN=NIAN+1
          ENDIF
          IF(IAA(IAN).EQ.I28)THEN
           AN28(N,M,IOW)=ANN(IAN)
           NIAN=NIAN+1
          ENDIF
          IF(IAA(IAN).EQ.I49)THEN
           AN49(N,M,IOW)=ANN(IAN)
           NIAN=NIAN+1
          ENDIF
          IF(IAA(IAN).EQ.I40)THEN
           AN40(N,M,IOW)=ANN(IAN)
           NIAN=NIAN+1
          ENDIF
          IF(IAA(IAN).EQ.I41)THEN
           AN41(N,M,IOW)=ANN(IAN)
           NIAN=NIAN+1
          ENDIF
          IF(IAA(IAN).EQ.I42)THEN
           AN42(N,M,IOW)=ANN(IAN)
           NIAN=NIAN+1
          ENDIF
         ENDDO
         IF(NIAN.LT.7)GOTO 250
        ENDIF
        GOTO 200
       ENDIF
      ENDIF
      GOTO 100
 300  continue
      CLOSE(1)
C
C***********************************************************************
C  3) CALCUL ISOTOPIC CONCENTRATION RATIOS FOR THE FUEL PINS OF EACH
C     RING AND BUNDLE AVERAGES AND WRITE RESULTS
C
C
C     R025A28=AU5(IOW)/AU8(IOW)
C      write(3,17)IOW,AU5(IOW),AU8(IOW)
C
      OPEN(3,FILE="E3.TEM")
C
       DO M=1,3
        WRITE(3,18)M
        WRITE(3,19)(AN25(N,M,IOW),N=1,8)
        WRITE(3,20)(AN28(N,M,IOW),N=1,8)
        WRITE(3,21)(AN26(N,M,IOW),N=1,8)
        WRITE(3,22)(AN49(N,M,IOW),N=1,8)
        WRITE(3,24)(AN40(N,M,IOW),N=1,8)
        WRITE(3,25)(AN41(N,M,IOW),N=1,8)
        WRITE(3,26)(AN42(N,M,IOW),N=1,8)
       END DO
      CLOSE(3)
C
C
      OPEN(2,FILE='E3.LIS')
      write(2,2)TIT(1:31)
      write(2,1)REF
      write(2,1)
C
      DO M=1,4
       WRITE(2,27)
       IF(M.LT.4)THEN
        WRITE(2,28)M
       ELSE
        WRITE(2,29)
       ENDIF
       WRITE(2,30)
C
       DO N=1,8
        IA=1
C
         IF(M.LT.4)THEN
          R25A28(N,M,IA)=(AN25(N,M,IA)/AN28(N,M,IA))*100.0
          R26A28(N,M,IA)=(AN26(N,M,IA)/AN28(N,M,IA))*100.0
          R49A28(N,M,IA)=(AN49(N,M,IA)/AN28(N,M,IA))*100.0
          R40A49(N,M,IA)=(AN40(N,M,IA)/AN49(N,M,IA))*100.0
          R41A49(N,M,IA)=(AN41(N,M,IA)/AN49(N,M,IA))*100.0
          R42A49(N,M,IA)=(AN42(N,M,IA)/AN49(N,M,IA))*100.0
         ENDIF
C
         IF(M.EQ.1)THEN
          R25A28(N,4,IA)= R25A28(N,M,IA)
          R26A28(N,4,IA)= R26A28(N,M,IA)
          R49A28(N,4,IA)= R49A28(N,M,IA)
          R40A49(N,4,IA)= R40A49(N,M,IA)
          R41A49(N,4,IA)= R41A49(N,M,IA)
          R42A49(N,4,IA)= R42A49(N,M,IA)
         ENDIF
C
         IF(M.GT.1.AND.M.LT.4)THEN
          R25A28(N,4,IA)= R25A28(N,4,IA)+ R25A28(N,M,IA)*(M*M+M)
          R26A28(N,4,IA)= R26A28(N,4,IA)+ R26A28(N,M,IA)*(M*M+M)
          R49A28(N,4,IA)= R49A28(N,4,IA)+ R49A28(N,M,IA)*(M*M+M)
          R40A49(N,4,IA)= R40A49(N,4,IA)+ R40A49(N,M,IA)*(M*M+M)
          R41A49(N,4,IA)= R41A49(N,4,IA)+ R41A49(N,M,IA)*(M*M+M)
          R42A49(N,4,IA)= R42A49(N,4,IA)+ R42A49(N,M,IA)*(M*M+M)
         ENDIF
C
         IF(M.EQ.4)THEN
          R25A28(N,4,IA)= R25A28(N,4,IA)/19.0
          R26A28(N,4,IA)= R26A28(N,4,IA)/19.0
          R49A28(N,4,IA)= R49A28(N,4,IA)/19.0
          R40A49(N,4,IA)= R40A49(N,4,IA)/19.0
          R41A49(N,4,IA)= R41A49(N,4,IA)/19.0
          R42A49(N,4,IA)= R42A49(N,4,IA)/19.0
         ENDIF
C
         DR25A28(N,M,IA)=((R25A28(N,M,IA)/R25A28E(N,M))-1.0)*100
         DR26A28(N,M,IA)=((R26A28(N,M,IA)/R26A28E(N,M))-1.0)*100
         DR49A28(N,M,IA)=((R49A28(N,M,IA)/R49A28E(N,M))-1.0)*100
         DR40A49(N,M,IA)=((R40A49(N,M,IA)/R40A49E(N,M))-1.0)*100
         DR41A49(N,M,IA)=((R41A49(N,M,IA)/R41A49E(N,M))-1.0)*100
         DR42A49(N,M,IA)=((R42A49(N,M,IA)/R42A49E(N,M))-1.0)*100
C
        WRITE(2,31)IQQE(N),R25A28E(N,M),
     1   R26A28E(N,M),R49A28E(N,M),R40A49E(N,M),R41A49E(N,M),
     2   R42A49E(N,M)
        WRITE(2,32)IQQ(N),R25A28(N,M,1),
     1   R26A28(N,M,1),R49A28(N,M,1),R40A49(N,M,1),R41A49(N,M,1),
     2   R42A49(N,M,1)
        WRITE(2,33)DQQ(N),
     1   DR25A28(N,M,1),DR26A28(N,M,1),DR49A28(N,M,1),DR40A49(N,M,1),
     2   DR41A49(N,M,1),DR42A49(N,M,1)
        WRITE(2,34)
C
       ENDDO
C
C      Averages and standard deviations of the discrepancies
C
       WRITE(2,35)
       WRITE(2,36)
       WRITE(2,37)
C
       MET=1
C
        do NV=1,6
         xmid(NV)=0.0
        enddo
C
        do N=1,8
         xmid(1)= xmid(1)+DR25A28(N,M,MET)
         if(N.GT.1)xmid(2)= xmid(2)+DR26A28(N,M,MET)
         xmid(3)= xmid(3)+DR49A28(N,M,MET)
         if(N.GT.2)then
          xmid(4)= xmid(4)+DR40A49(N,M,MET)
          xmid(5)= xmid(5)+DR41A49(N,M,MET)
          xmid(6)= xmid(6)+DR42A49(N,M,MET)
         endif
        enddo
C
        do NV=1,6
         NTOTB=8
         if(NV.EQ.2)NTOTB=7
         if(NV.GT.3)NTOTB=6
         xmid(NV)= xmid(NV)/NTOTB
        enddo
C
        do NV=1,6
         dxmid(NV)=0.0
        enddo
C
        do N=1,8
         dxmid(1)=dxmid(1)+(DR25A28(N,M,MET)-xmid(1))**2.0
         if(N.GT.1)dxmid(2)=dxmid(2)+(DR26A28(N,M,MET)-xmid(2))**2.0
         dxmid(3)= dxmid(3)+(DR49A28(N,M,MET)-xmid(3))**2.0
         if(N.GT.2)then
          dxmid(4)=dxmid(4)+(DR40A49(N,M,MET)-xmid(4))**2.0
          dxmid(5)=dxmid(5)+(DR41A49(N,M,MET)-xmid(5))**2.0
          dxmid(6)=dxmid(6)+(DR42A49(N,M,MET)-xmid(6))**2.0
         endif
        enddo
C
        do NV=1,6
         NTOTB=8
         if(NV.EQ.2)NTOTB=7
         if(NV.GT.3)NTOTB=6
         dxmid(NV)=sqrt(dxmid(NV)/NTOTB)
        enddo
C
        WRITE(2,38)
        WRITE(2,39)xmid(1),dxmid(1)
        WRITE(2,40)xmid(2),dxmid(2)
        WRITE(2,41)xmid(3),dxmid(3)
        WRITE(2,42)xmid(4),dxmid(4)
        WRITE(2,43)xmid(5),dxmid(5)
        WRITE(2,44)xmid(6),dxmid(6)
C
      ENDDO
C
      CLOSE(2)
C
  1   FORMAT(A80)
  2   FORMAT(A31)
  3   FORMAT(E13.5)
  5   FORMAT(A44)
  6   FORMAT(F8.1)
  7   FORMAT(6F8.4)
  8   FORMAT(37X,A6)
  9   FORMAT(27X,F9.3)
 10   FORMAT(1X,A8)
 11   FORMAT(A6)
 12   FORMAT(3(I6,2X,E15.7))
 17   FORMAT("IOW,AU5,AU8 ",I3,2E12.5)
 18   FORMAT("PIN=",I4)
 19   FORMAT("N25=",8E10.3)
 20   FORMAT("N28=",8E10.3)
 21   FORMAT("N26=",8E10.3)
 22   FORMAT("N49=",8E10.3)
 23   FORMAT(A24)
 24   FORMAT("N40=",8E10.3)
 25   FORMAT("N41=",8E10.3)
 26   FORMAT("N42=",8E10.3)
 27   FORMAT("****************************************")
 28   FORMAT("RING",I5)
 29   FORMAT("AVERAGE VALUES")
 30   FORMAT("       BUP N25/N28 N26/N28 N49/N28 N40/N49 N41/N49 N42/N49
     1")
 31   FORMAT("EXP ",I6,6F8.4)
 32   FORMAT("DSN ",I6,6F8.4)
 33   FORMAT("DDSN",F6.2,6(2X,F6.2))
 34   FORMAT(" ")
 35   FORMAT("AVERAGES AND STANDARD DEVIATIONS in %")
 36   FORMAT(" N26/28: cases 2 to 8")
 37   FORMAT(" Ni/49 : cases 3 to 8")
 38   FORMAT("METHOD DSN")
 39   FORMAT(" N25/N28 = ",2F5.2)
 40   FORMAT(" N26/N28 = ",2F5.2)
 41   FORMAT(" N49/N28 = ",2F5.2)
 42   FORMAT(" N40/N49 = ",2F5.2)
 43   FORMAT(" N41/N49 = ",2F5.2)
 44   FORMAT(" N42/N49 = ",2F5.2)
C
      STOP 'Completed'
C Error trap
  900 STOP 'E3 ERROR - Unrecognozed library in WIMS-D output'
      END
