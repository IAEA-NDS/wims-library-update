C
C  PROGRAM E2T2
C
C  AUTHOR: FRANCISCO LESZCZYNSKI-INGENIERIA NUCLEAR- 1999
C                                CENTRO ATOMICO BARILOCHE
C                                ARGENTINA
C
C  PROCESS WIMS OUTPUT FOR BENCHMARK D2O EX.2)DCA Task 2 FOR ONE LIBRARY
C
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS
C
C  2) READ WIMS-D RESULTS-IOW
C                          1 <-E2T2H00D.OUT;
C                          2 <-E2T2A00D.OUT;
C                          3 <-E2T2H12D.OUT;
C                          4 <-E2T2D00D.OUT;
C                          5 <-E2T2D01D.OUT;
C                          6 <-E2T2D05D.OUT;
C                          7 <-E2T2D10D.OUT;
C       READ TOTAL REACTION RATES BY MATERIAL OF Dy-164(164)
C
C  3) CALCUL THERMAL NEUTRON FLUX DISTRIBUTION FOR FUEL A1,A2,A3
C     (MATS.5,9,10,22) AND FOR MODERATOR VS. RADIUS (MATS.2,11-21)
C     FOR CLUSTER WITHOUT Gd (COOLANT=H2O and AIR) AND
C     FOR CLUSTER WITH Gd (COOLANT=H2O)
C  4) CALCUL DETAILED AND MEAN VALUE OF FINE STRUCTURE OF
C     THERMAL NEUTRON FLUX DISTRIBUTION ON A PIN ROUNDED BY D2O,
C     WITHOUT Gd AND WITH 0.1, 0.5 AND 1.0 WT% Gd
C  5) WRITE RESULTS
C
C***********************************************************************
      DIMENSION FFRE(33,8),CE(5,4),RU5FRE(5,4),RCAE(5,4),ALMRE(5,4)
      DIMENSION FFR(33,8),C(5,8),RU5FR(5,5),RCA(5,5),ALMR(5,5)
      DIMENSION FFRR(33,8),CR(5,8),RU5FRR(5,5),RCAR(5,5),ALMRR(5,5)
      DIMENSION ABSO(5,33,8),FIS(5,5,8)
      DIMENSION RADIO(33,8)
      CHARACTER*3 DVOID,DINEX,DDLOW,ANA(33,8)
      CHARACTER*12 OUTW(8)
      CHARACTER*24 REFER1
      CHARACTER*28 AISOT(5)
      CHARACTER*30 REFER2
      CHARACTER*44 ADVICE
      CHARACTER*80,TIT,REF,TIT2
      CHARACTER*120 LINE
C
C***********************************************************************
C  1) READ EXPERIMENTAL VALUES OF PARAMETERS
C
      OPEN(1,FILE='E2T2.REF')
      read(1,1)TIT
      read(1,1)REF
C
      DO J=1,2
       read(1,1)TIT2
       DO I=1,19
        read(1,3)FFRE(I,J)
       END DO
      END DO
C
      read(1,1)TIT2
      DO I=1,18
       read(1,3)FFRE(I,3)
      END DO
C
      read(1,1)TIT2
      DO I=1,8
       read(1,3)FFRE(I,7)
      END DO
C
      CLOSE(1)
  1   FORMAT(A80)
  3   FORMAT(E13.5)
C***********************************************************************
C  2) READ WIMS-D RESULTS-IOW
C                          1 <-E2T2H00D.OUT;
C                          2 <-E2T2A00D.OUT;
C                          3 <-E2T2H12D.OUT;
C                          4 <-E2T2D00D.OUT;
C                          5 <-E2T2D01D.OUT;
C                          6 <-E2T2D05D.OUT;
C                          7 <-E2T2D10D.OUT;
C
      OUTW(1)="E2T2H00D.OUT"
      OUTW(2)="E2T2A00D.OUT"
      OUTW(3)="E2T2H12D.OUT"
      OUTW(4)="E2T2D00D.OUT"
      OUTW(5)="E2T2D01D.OUT"
      OUTW(6)="E2T2D05D.OUT"
      OUTW(7)="E2T2D10D.OUT"
C
      AISOT(1)='element   164 reaction rates'
C
      DO IOW=1,3
       OPEN(1,FILE=OUTW(IOW))
C
C       READ TOTAL REACTION RATES BY MATERIAL OF Dy-164(164)
C
C -------------------------------------------------------------------
       ISOT=1
C
C      ISOT=1: ABS.R Dy164
C
        read(1,'(A120)')LINE
        DO WHILE(LINE(2:11).NE.'k-eff edit')
         read(1,'(A120)')LINE
        END DO
        read(1,'(A120)')LINE
        DO WHILE(LINE(2:29).NE.AISOT(ISOT))
         read(1,'(A120)')LINE
        END DO
        read(1,'(A120)')LINE
        DO WHILE(LINE(2:13).NE.'group materl')
         read(1,'(A120)')LINE
        END DO
         read(1,'(A120)')LINE
         DO WHILE(LINE(2:8).NE.'  total')
          read(1,'(A120)')LINE
         END DO
         BACKSPACE(1)
C         BACKSPACE(1)
         read(1,'(12X,8E13.5)')(ABSO(ISOT,J,IOW),J=1,8)
         read(1,'(A120)')LINE
         DO WHILE(LINE(2:8).NE.'  total')
          read(1,'(A120)')LINE
         END DO
         BACKSPACE(1)
C         BACKSPACE(1)
         read(1,'(12X,8E13.5)')(ABSO(ISOT,J,IOW),J=9,16)
         read(1,'(A120)')LINE
         DO WHILE(LINE(2:8).NE.'  total')
          read(1,'(A120)')LINE
         END DO
         BACKSPACE(1)
C         BACKSPACE(1)
         read(1,'(12X,6E13.5)')(ABSO(ISOT,J,IOW),J=17,22)
C
       CLOSE(1)
      END DO
C -------------------------------------------------------------------
      DO IOW=4,7
       OPEN(1,FILE=OUTW(IOW))
C
C      READ TOTAL REACTION RATES BY MESH, OF:
C      Dy-164(164)
C
C      ISOT=1: ABS.R Dy164
C
       ISOT=1
       read(1,'(A120)')LINE
       DO WHILE(LINE(2:11).NE.'k-inf edit')
        read(1,'(A120)')LINE
       END DO
C
       read(1,'(A120)')LINE
       DO WHILE(LINE(2:29).NE.AISOT(ISOT))
        read(1,'(A120)')LINE
       END DO
       read(1,'(A120)')LINE
       DO WHILE(LINE(2:8).NE.'  total')
        read(1,'(A120)')LINE
       END DO
C       BACKSPACE(1)
       BACKSPACE(1)
       read(1,'(12X,8E13.5)')(ABSO(ISOT,J,IOW),J=1,8)
       read(1,'(A120)')LINE
       DO WHILE(LINE(2:8).NE.'  total')
        read(1,'(A120)')LINE
       END DO
C       BACKSPACE(1)
       BACKSPACE(1)
       read(1,'(12X,8E13.5)')(ABSO(ISOT,J,IOW),J=9,16)
       read(1,'(A120)')LINE
       DO WHILE(LINE(2:8).NE.'  total')
        read(1,'(A120)')LINE
       END DO
C       BACKSPACE(1)
       BACKSPACE(1)
       read(1,'(12X,8E13.5)')(ABSO(ISOT,J,IOW),J=17,24)
       read(1,'(A120)')LINE
       DO WHILE(LINE(2:8).NE.'  total')
        read(1,'(A120)')LINE
       END DO
C       BACKSPACE(1)
       BACKSPACE(1)
       read(1,'(12X,8E13.5)')(ABSO(ISOT,J,IOW),J=25,32)
C
       CLOSE(1)
      END DO
C***********************************************************************
C  3) CALCUL THERMAL NEUTRON FLUX DISTRIBUTION FOR FUEL A1,A2,A3
C     (MATS.5,9,10,22), FOR P.TUBE, GAP AND CAL.TUBE (MATS. 3,4,8),
C     AND FOR MODERATOR VS. RADIUS (MATS.2,11-21)
C     FOR CLUSTER WITHOUT Gd (COOLANT=H2O and AIR) AND
C     FOR CLUSTER WITH Gd (COOLANT=H2O)
C
      DO IOW=1,2
       ABSO2=ABSO(1,3,IOW)
       FFR(1,IOW)=ABSO(1,5,IOW)/ABSO2
       FFRR(1,IOW)=FFR(1,IOW)-FFRE(1,IOW)
       FFR(2,IOW)=ABSO(1,9,IOW)/ABSO2
       FFRR(2,IOW)=FFR(2,IOW)-FFRE(2,IOW)
       FFR(3,IOW)=ABSO(1,10,IOW)
       FFR(3,IOW)=(0.5*FFR(3,IOW)+0.5*ABSO(1,22,IOW))/ABSO2
       FFRR(3,IOW)=FFR(3,IOW)-FFRE(3,IOW)
       FFR(4,IOW)=1.0
       FFRR(4,IOW)=0.0
       FFR(5,IOW)=ABSO(1,4,IOW)/ABSO2
       FFRR(5,IOW)=FFR(5,IOW)-FFRE(5,IOW)
       FFR(6,IOW)=ABSO(1,8,IOW)/ABSO2
       FFRR(6,IOW)=FFR(6,IOW)-FFRE(6,IOW)
C
       FFR(7,IOW)=ABSO(1,2,IOW)/ABSO2
       FFRR(7,IOW)=FFR(7,IOW)-FFRE(7,IOW)
       DO I=8,18
        FFR(I,IOW)=ABSO(1,I+3,IOW)/ABSO2
        FFRR(I,IOW)=FFR(I,IOW)-FFRE(I,IOW)
       END DO
       DR=1.0
       RADIO(1,IOW)=3.825
       RADIO(2,IOW)=5.76
       RADIO(3,IOW)=7.68
       RADIO(4,IOW)=9.25
       RADIO(5,IOW)=9.75
       RADIO(6,IOW)=10.25
       RADIO(7,IOW)=11.0
       DO I=8,18
        RADIO(I,IOW)=RADIO(I-1,IOW)+DR
       END DO
      END DO
C
      ABSO2=ABSO(1,3,3)
      FFR(1,3)=ABSO(1,5,3)/ABSO2
      FFRR(1,3)=FFR(1,3)-FFRE(1,3)
      FFR(2,3)=ABSO(1,9,3)/ABSO2
      FFRR(2,3)=FFR(2,3)-FFRE(2,3)
      FFR(3,3)=ABSO(1,10,3)/ABSO2
      FFRR(3,3)=FFR(3,3)-FFRE(3,3)
      FFR(4,3)=ABSO(1,22,3)/ABSO2
      FFRR(4,3)=FFR(4,3)-FFRE(4,3)
      FFR(5,3)=1.0
      FFRR(5,3)=0.0
      FFR(6,3)=ABSO(1,4,3)/ABSO2
      FFRR(6,3)=FFR(6,3)-FFRE(6,3)
      FFR(7,3)=ABSO(1,8,3)/ABSO2
      FFRR(7,3)=FFR(7,3)-FFRE(7,3)
      FFR(8,3)=ABSO(1,2,3)/ABSO2
      FFRR(8,3)=FFR(8,3)-FFRE(8,3)
      DO I=9,17
       FFR(I,3)=ABSO(1,I+2,3)/ABSO2
       FFRR(I,3)=FFR(I,3)-FFRE(I,3)
      ENDDO
C
      DR=1.0
      RADIO(1,3)=3.825
      RADIO(2,3)=5.76
      RADIO(4,3)=7.68
      RADIO(5,3)=9.25
      RADIO(6,3)=9.75
      RADIO(7,3)=10.25
      RADIO(8,3)=11.0
      DO I=9,17
       RADIO(I,3)=RADIO(I-1,3)+DR
      END DO
C
C  4) CALCUL MEAN VALUE OF FINE STRUCTURE OF THERMAL NEUTRON FLUX DISTRIBUTION
C     ON A PIN WITHOUT Gd AND WITH 0.1,0.5 AND 1.0 WT% Gd
C
      DO IOW=4,7
       ABSO32=ABSO(1,32,IOW)
       FFR(33,IOW)=0.0
       DO I=1,32
        FFR(I,IOW)=ABSO(1,I,IOW)/ABSO32
        FFR(33,IOW)=FFR(33,IOW)+FFR(I,IOW)
       ENDDO
       FFR(33,IOW)=FFR(33,IOW)/32.0
       FFRR(33,IOW)=FFR(33,IOW)-FFRE(IOW-3,7)
      END DO
C***********************************************************************
C  6) WRITE RESULTS
C
      OPEN(2,FILE='E2T2.LIS')
C **********************************
C
C TABLE 1
C
      WRITE(2,400)' Thermal Neutron Flux Distribution'
      WRITE(2,400)' ================================='
      WRITE(2,401)' Case  COO  A1   DIFF.  A2   DIFF. A3SG  DIFF. A3CG
     1DIFF.  M11  DIFF.  M16  DIFF.  M22  DIFF. '
      WRITE(2,401)' ====================================================
     1=========================================='
      WRITE(2,500)(FFRE(I,1),FFRE(19,1),I=1,3),FFRE(7,1),FFRE(19,1),
     1 FFRE(12,1),FFRE(19,1),FFRE(18,1),FFRE(19,1)
      WRITE(2,503)(FFR(I,1),FFRR(I,1),I=1,3),FFR(7,1),FFRR(7,1),
     1 FFR(12,1),FFRR(12,1),FFR(18,1),FFRR(18,1)
      WRITE(2,402)'          '
C
      WRITE(2,501)(FFRE(I,2),FFRE(19,1),I=1,3),FFRE(7,2),FFRE(19,1),
     1 FFRE(12,2),FFRE(19,1),FFRE(18,2),FFRE(19,1)
      WRITE(2,503)(FFR(I,2),FFRR(I,2),I=1,3),FFR(7,2),FFRR(7,2),
     1 FFR(12,2),FFRR(12,2),FFR(18,2),FFRR(18,2)
      WRITE(2,402)'          '
C
      WRITE(2,502)(FFRE(I,3),FFRE(18,3),I=1,2),FFRE(4,3),FFRE(18,3),
     1 FFRE(8,3),FFRE(18,3),FFRE(13,3),FFRE(18,3),FFRE(17,3),FFRE(18,3)
      WRITE(2,504)(FFR(I,3),FFRR(I,3),I=1,2),FFR(4,3),FFRR(4,3),
     1 FFR(8,3),FFRR(8,3),FFR(13,3),FFRR(13,3),FFR(17,3),FFRR(17,3)
      WRITE(2,402)'          '
      WRITE(2,401)' ====================================================
     1=========================================='
C
 400  FORMAT(A34)
 401  FORMAT(A95)
 402  FORMAT(A10)
 500  FORMAT('Gd:NO  H2O ',3(F5.2,'(',F5.2,')'),12X,3(F5.2,'(',F5.2,')')
     1)
 501  FORMAT('Gd:NO  AIR ',3(F5.2,'(',F5.2,')'),12X,3(F5.2,'(',F5.2,')')
     1)
 502  FORMAT('Gd:YES H2O ',2(F5.2,'(',F5.2,')'),12X,4(F5.2,'(',F5.2,')')
     1)
 503  FORMAT('           ',3(F5.2,'(',F5.2,')'),12X,3(F5.2,'(',F5.2,')')
     1)
 504  FORMAT('           ',2(F5.2,'(',F5.2,')'),12X,4(F5.2,'(',F5.2,')')
     1)
C *****************************
C
C FIGURE 1A
C
      EXPERR=0.0
      WRITE(2,600)' Therm.Flux Distr.H2O coolant-cluster without poison'
      WRITE(2,600)' ==================================================='
      WRITE(2,601)' R(cm)     FLUX   DIFF. '
      WRITE(2,601)' ======================='
      DO I=1,18
       WRITE(2,700)RADIO(I,1),FFRE(I,1),EXPERR
       WRITE(2,701)FFR(I,1),FFRR(I,1)
       WRITE(2,402)'          '
      END DO
      WRITE(2,601)' ======================='
C
C FIGURE 1B
C
      WRITE(2,600)' Therm.Flux Distr.AIR coolant-cluster without poison'
      WRITE(2,600)' ==================================================='
      WRITE(2,601)' R(cm)     FLUX   DIFF. '
      WRITE(2,601)' ======================='
      DO I=1,18
       WRITE(2,700)RADIO(I,1),FFRE(I,2),EXPERR
       WRITE(2,701)FFR(I,2),FFRR(I,2)
       WRITE(2,402)'          '
      END DO
      WRITE(2,601)' ======================='
C
 600  FORMAT(A52)
 601  FORMAT(A24)
 700  FORMAT(F7.2,3X,F7.2,'(',F7.2,')')
 701  FORMAT(10X,F7.2,'(',F7.2,')')
C *****************************
C
C FIGURE 2
C
      WRITE(2,600)' Therm.Flux Distr.12 poisoned fuel pins on 3rd.layer'
      WRITE(2,600)' ==================================================='
      WRITE(2,601)' R(cm)     FLUX   DIFF. '
      WRITE(2,601)' ======================='
      DO I=1,2
       WRITE(2,700)RADIO(I,3),FFRE(I,3),EXPERR
       WRITE(2,701)FFR(I,3),FFRR(I,3)
       WRITE(2,402)'          '
      END DO
      DO I=4,17
       WRITE(2,700)RADIO(I,3),FFRE(I,3),EXPERR
       WRITE(2,701)FFR(I,3),FFRR(I,3)
       WRITE(2,402)'          '
      END DO
      WRITE(2,601)' ======================='
C *****************************
C
C TABLE 32A
C
      WRITE(2,600)' Therm.Flux Distr.-Average Dy-reaction rate on pins '
      WRITE(2,600)' ==================================================='
      WRITE(2,601)' Case      Dy-RR  DIFF. '
      WRITE(2,601)' ======================='
C
      WRITE(2,801)FFRE(1,7),FFRE(5,7)
      WRITE(2,805)FFR(33,4),FFRR(33,4)
      WRITE(2,402)'          '
C
      WRITE(2,802)FFRE(2,7),FFRE(6,7)
      WRITE(2,805)FFR(33,5),FFRR(33,5)
      WRITE(2,402)'          '
C
      WRITE(2,803)FFRE(3,7),FFRE(7,7)
      WRITE(2,805)FFR(33,6),FFRR(33,6)
      WRITE(2,402)'          '
C
      WRITE(2,804)FFRE(4,7),FFRE(8,7)
      WRITE(2,805)FFR(33,7),FFRR(33,7)
      WRITE(2,402)'          '
C
      WRITE(2,601)' ======================='
C
 801  FORMAT('Gd:NO     ',F7.3,'(',F7.3,')')
 802  FORMAT('Gd:0.1wt% ',F7.3,'(',F7.3,')')
 803  FORMAT('Gd:0.5wt% ',F7.3,'(',F7.3,')')
 804  FORMAT('Gd:1.0wt% ',F7.3,'(',F7.3,')')
 805  FORMAT('          ',F7.3,'(',F7.3,')')
C
      CLOSE(2)
C
      STOP 'Completed'
      END
