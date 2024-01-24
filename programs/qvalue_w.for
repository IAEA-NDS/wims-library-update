      PROGRAM QVALUE_W
C-Title  : Program QVALUE_W
C-Purpose: Analyse WLUP benchmarks by slowing-down density q(E)
C-Author : F.Leszczynski, Ingenieria Nuclear
C-A                       Centro Atomico Bariloche, ARGENTINA
C-A        A.Trkov,       International Atomic Energy Agency
C-A                       Wagramerstrasse 5, Vienna, AUSTRIA
C-Version: August 2001, first release, (F.Leszczynski)
C-V  01/11 A.Trkov
C-V        - Sort results by ascending q(E)
C-V        - Add printout in PLOTTAB format
C-V  05/07 D.L. Aldama, IAEA/NDS Consultant
C-V        - Correction to skip " Average  " lines
C-M
C-M  QVALUE Program Users Guide
C-M  ==========================
C-M
C-M  The purpose of the program is
C-M  1. To calculate the q-value for each WLUP-benchmark lattice
C-M     by processing WIMS-D outputs for one library, and
C-M  2. To write tables for plotting Keff vs. q.
C-M     The Keff values are extracted from the SMRLIB output file.
C-M
C-M  The "q" is defined as the number of fission neutrons
C-M  reaching an arbitrarily chosen threshold energy. It is chosen
C-M  arbitrarily, but the value 2.6 eV is generaly agreed upon for
C-M  easier comparison of results). The "q" value characterises the
C-M  hardness of the spectrum. It is close to 1 for soft spectra
C-M  with little absorption and small for hard spectra. It is
C-M  formally defined by:
C-M
C-M      sum_g(g>2.6 eV)[ Phi(g) sum_h(h<2.6eV) Sig(g-->h) ]
C-M  q = ----------------------------------------------------
C-M                sum_g( Phi(g) nue(g) Sigf(g) )
C-M
C-M  where Phi is the cell spectrum read from the region edit.
C-M  Sig(g-->h) is the scattering matrix. Summation is performed
C-M  over matrix elements "h" corresponding to groups <2.6 eV (i.e.
C-M  scattering source to any anergy below 2.6 eV). Outer summation
C-M  has to be done over all groups "g" corresponding to energies
C-M  above 2.6 eV.
C-M  Reference: Dr.Andrej Trkov-IAEA-NDS (personal communication)
C
C In this program, the value of the threshold energy can be choosen,
C giving on input file QVALUE.INP the number of "epithermal"
C groups(ngep).
C The lower energy limit of this group will be the threshold energy.
C The value for this energy = 2.6 eV, corresponds to ngep = 29 for the
C standard 69-group WIMS-D library.
C The number of groups (ng) of the WIMS-D library can be changed on
C QVALUE.INP C file. This value, ng and ngep cannot be exactly the
C  number of groups of the
C  libraries but the main groups on WIMS-D calculations associated with
C  q-calculation.
C
C
C INPUT FILES:
C
C 1) QVALUE.INP : See the file included on the package for explanation
C      of this input. It is a standard file. It must be changed only if
C      the user wish to change some parameters included on it.
C      WARNING: This file is strongly dependent of the
C      other input files (names and order of data).
C
C        Rec. Parameter Content                                 Format
C        1    ng        Number of energy groups                  2I5
C             ngep      Number of epithermal groups
C        2    nlib      Number of libraries with Keff             I5
C      k=1,nlib:
C        3k   NOMLIBk   Names of libs (= smrlib.lst)             A10
C        4    nben      Number of benchmarks                      I5
C      i=1,nben:
C        5i   NOMBENi   Name of i-benchmark                       A7
C        6i   NOUTi     Number of WIMS-outp files f.bench.i       I5
C       j=1,NOUTi
C        7ij  NOMOUTij  Name of j-WIMS outp f.bench.i            A12
C        8ij  NCASij    Number of cases on j-WIMS outp f.bench.i  I5
C
C 2) SMRLIB.LST   This file is used for retrieving the Keff's of
C          WLUP-benchmarks calculated with  several libraries,
C          for output of tables ready for graph Keff vs.q.
C          It can be the output file of SMRLIB program or any file
C          containing Keff values of benchmarks after the names of
C          the libraries (a record for each lib. Format: A10,F8.5).
C          Before the Keff for the first lib, a record must be present
C          with the name of the present lattice (format: A10).
C
C 3) wimsX.OUT:WIMS-D outputs of all the X-benchmarks included in
C          SMRLIB.LST for any of the WIMS-D libraries (the q-value is
C          not strongly dependent on WIMS-library).
C          These WIMS-output files are used for reading the needed
C          parameters for calculating the "q" value for each case.
C          The names wimsX must be the names given on QVALUE.INP file.
C
C
C OUTPUT FILES
C
C benchY.DAT    : Tables of Keff vs.q-value for all benchmarks given in
C           QVALUE.INP. A file for each benchmark type, with the name
C           of each file equal to the benchmark-type name given in
C           QVALUE.INP.
C           Two columns are added to the graphs: ExpKeff (=1.0) and
C           ExpError (absolute) extraced from SMRLIB.LST file,
C           for plotting experimental error bars.
C           For graphing the content of these files, use the MSDOS
C           SORT command before, to put the q-values in ascending order.
C           There is an SORTALL.BAT file to sort all results.
C
C STEPS FOR GRAPHING Keff vs.q:
C
C 1. Run BNCHALL.BAT for all WIMS-libraries wished (do not delete the
C    wims output files for one lib to be used as reference for q-value
C    calculations)
C 2. Run SMRLIB.EXE for including results of all libraries
C 3. Edit QVALUE.INP for adjusting to the actual case.
C 4. Run QVALUE.EXE.
C 5. Sort benchY.DAT QVALUE-output files.
C 6. Enter to the desired graph program and import each benchY.DAT file
C    as source of data.
C 7. Enjoy with the nice graphs that you will obtain.
C
C LIMITATIONS:
C Number of libs:  <=MXLI
C Number of groups:<=MXGR
C***********************************************************************
      PARAMETER    (MXLI=10, MXGR=200)
C***********************************************************************
      DIMENSION AK(MXLI),FLUX(MXGR),ANUXSF(MXGR),XSGGP(MXGR,MXGR)
C
      CHARACTER*10  NOMLIB(MXLI)
      CHARACTER*2   NN(MXLI)
      CHARACTER*7   NOMBEN
      CHARACTER*12  NOMOUT
      CHARACTER*10  NOMCAS
      CHARACTER*80  LINE
      CHARACTER*6   TIT1
      CHARACTER*10  TIT2,TIT3
      CHARACTER*11  FORMAT1
      CHARACTER*130 FORMAT2
C
      DO I=1,MXLI
        WRITE(NN(I),'(I2)') I
      END DO
C
C***********************************************************************
C 1) OPEN QVALUE.INP AND READ GENERAL DATA
C
      OPEN(UNIT=1,FILE='QVALUE.INP',FORM='FORMATTED',STATUS='OLD')
      READ(1,1)NG,NGEP
      READ(1,2)NLIB
      READ(1,3)(NOMLIB(I),I=1,NLIB)
      READ(1,2)NBEN
C
C***********************************************************************
C 2) OPEN SMRLIB.LST
C
      OPEN(UNIT=2,FILE='SMRLIB.LST',FORM='FORMATTED',STATUS='OLD')
C
C***********************************************************************
C 3) LOOP OVER THE BENCHMARKS
C
      DO I1=1,NBEN
C
C    >READ BENCHMARK NAME AND NUMBER OF OUTPUTS FOR BENCH.,I1 FROM QVALUE.INP
C
       READ(1,4)NOMBEN
       READ(1,2)NOUT
C
C    >OPEN OUTPUT FILE NOMBEN.DAT FOR BENCHMARK I1
C
       OPEN(UNIT=4,FILE=TRIM(NOMBEN)//'.DAT',FORM='FORMATTED',
     1  STATUS='UNKNOWN')
C
C    >WRITE TITLE ON (NOMBEN).DAT FOR BENCH.,I1
C
       TIT1="     q"
       TIT2="   EXPKeff"
       TIT3="  EXPerror"
       FORMAT1="(A6,"//NN(NLIB)//"3A10)"
       WRITE(4,FORMAT1)TIT1,(NOMLIB(LN),LN=1,NLIB),TIT2,TIT3
C
C    >LOOP OVER NOUT WIMS-OUTPUTS OF BENCH I1
C
       DO J1=1,NOUT
C
C    >READ WIMS-OUTP NAME AND NUMB.OF CASES FOR BENH I1, OUT J1 FROM QVALUE.INP
C
        READ(1,5)NOMOUT
        READ(1,2)NCAS
C
C    >OPEN WIMS-OUTPUT NOMOUT FILE FOR BENCH I1, OUT J1
C
        OPEN(UNIT=3,FILE=NOMOUT,FORM='FORMATTED',STATUS='OLD')
C
C    >LOOP OVER NCAS CASES OF BENCH I1, OUT J1
C
        DO K1=1,NCAS
C
C    >READ Keff VALUES FOR BENCH I1,OUT J1,CASE K1 FOR ALL LIBS FROM SMRLIB.LST
C
  70     read(2,11)LINE
         DO WHILE(LINE(1:10).NE.NOMLIB(1))
          read(2,11,END=80)LINE
         END DO
         BACKSPACE(2)
         BACKSPACE(2)
         READ(2,3)NOMCAS
         IF(NOMCAS.EQ."   Average".or.NOMCAS.EQ." Average  ")
     &       read(2,11)LINE
         IF(NOMCAS.EQ."   Average".or.NOMCAS.EQ." Average  ")GOTO 70
         BACKSPACE(2)
         READ(2,6)NOMCAS,AKEXP,DKPEXP
         DKEXP=DKPEXP/100.0
         DO L=1,NLIB
          read(2,12)AK(L)
         ENDDO
         GOTO 81
C
  80     CLOSE(1)
         CLOSE(2)
         CLOSE(3)
         CLOSE(4)
         STOP "PROBLEM WITH SMRLIB.LST FILE"
C
C    >READ SCATTERING MATRIX FOR BENCH I1, OUT J1, CASE K1
C
  81     read(3,11)LINE
         DO WHILE(LINE(2:38).NE.'CELL AVERAGE SCATTERING CROSS SECTION')
          read(3,11,END=90)LINE
         END DO
         GOTO 91
C
  90     CLOSE(1)
         CLOSE(2)
         CLOSE(3)
         CLOSE(4)
         PRINT *,'PROBLEM WITH SCAT.XS MATRIX ON OUTPUT FILE ',NOMOUT
         STOP "PROBLEM WITH SCAT.XS MATRIX ON OUTPUT FILE"
C
  91     L1=1
         L10=10
         NJ=NG/10+1
         DO J=1,NJ
          DO I=1,4
           read(3,11)LINE
          ENDDO
          read(3,11)LINE
          IF(J.LT.NJ)THEN
           DO IG=1,NG
            read(3,21)(XSGGP(IG,IJ),IJ=L1,L10)
           ENDDO
          ELSE
           L11=10*NJ-NG
           DO IG=1,NG
            read(3,21)(XSGGP(IG,IJ),IJ=L1,L10-L11)
           ENDDO
          ENDIF
          L1=L10+1
          L10=L1+9
         ENDDO
C
C    >READ nu*XSf AND FLUXeff FOR BENCH I1, OUT J1, CASE K1
C
         read(3,11)LINE
         DO WHILE(LINE(2:33).NE.'GROUP     DIFFUSION    DIFFUSION')
          read(3,11)LINE
         END DO
         DO IG=1,NG
          read(3,22)ANUXSF(IG),FLUX(IG)
         ENDDO
C
C    >CALCUL Q-VALUE FOR BENCH I1, OUT J1, CASE K1
C
C     NUMERATOR
C
         Q1=0.0
         DO IG=1,NGEP
          Q2=0.0
          DO IJ=NGEP+1,NG
           Q2=Q2+XSGGP(IG,IJ)
          ENDDO
          Q1=Q1+Q2*FLUX(IG)
         ENDDO
C
C     DENOMINATOR
C
         Q3=0.0
         DO IG=1,NG
          Q3=Q3+FLUX(IG)*ANUXSF(IG)
         ENDDO
C
C     Q-VALUE
C
         QVALUE=Q1/Q3
C
C    >WRITE RESULTS FOR BENCH I1, OUT J1, CASE K1
C
         FORMAT2="(F6.3,"//NN(NLIB)//"F10.5,2F10.5,2X,A10)"
         WRITE(4,FORMAT2)QVALUE,(AK(NL),NL=1,NLIB),AKEXP,DKEXP,NOMCAS
C
        ENDDO
       ENDDO
       CLOSE(4)
      ENDDO
      CLOSE(1)
      CLOSE(2)
      CLOSE(3)
 
C
C***********************************************************************
  1   FORMAT(2I5)
  2   FORMAT(I5)
  3   FORMAT(A10)
  4   FORMAT(A7)
  5   FORMAT(A12)
  6   FORMAT(A10,F8.5,2X,F3.2)
C
 11   FORMAT(A80)
 12   FORMAT(10X,F8.5)
C
 21   FORMAT(3X,10(1X,E11.4))
 22   FORMAT(64X,E13.6,1X,E13.6)
C
      STOP 'Qvalue_w Completed'
      END
