       PROGRAM MTRB2
C-Title  : MTRB2 Program
C-Purpose: Summarize WLUP MTR-OWR Benchmark
C-Version: Original code.
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia (2000)
C-M
C-M  Manual for Program MTRB2
C-M  ========================
C-M  A summary of results for the WLUP MTR-OWR benchmark
C-M
C-M  The list files are those produced by analysing the WIMS-D
C-M  outputs with post-processing codes developed within the scope
C-M  of the WIMS-D Library Update Project (IAEA-CRP 1999-2000) by
C-M  Francisco Leszczynski.
C-M
C-M  Instructions:
C-M  The filenames for the output report file and the 5 list files
C-M  for the benchmarks described above are entered in response to
C-M  the prompt from the keyboard. Individual files may be omitted
C-M  from the analysis by entering "-" for the filename.
C-M
C-
      DIMENSION AVER(6),STDEV(6),EXPER(17,1),CALCUL(17,1),DCALC(17,1)
      CHARACTER*120 REC
      CHARACTER*40  BLNK,FLNM,FLNI(8),FLNO
      CHARACTER*10  NAME(17)
C* Filenames and logical file units
      DATA LIN,LOU,LKB,LTT / 1, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1    ,FLNI/'MTRBURN.lis'
     2    ,     '           '
     3    ,     '           '
     4    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '
     1    ,     '           '/
     1    ,FLNO/'MTRBURN.LST'/
C*
      DATA NAME/' E-CYCLE 1'
     1         ,' E-CYCLE 2'
     2         ,' E-CYCLE 3'
     3         ,' E-CYCLE 4'
     4         ,' E-CYCLE 5'
     5         ,' E-CYCLE 6'
     6         ,' E-CYCLE 7'
     7         ,' E-CYCLE 8'
     8         ,' E-CYCLE 9'
     9         ,' E-CYCLE10'
     A         ,' E-CYCLE11'
     B         ,' E-CYCLE12'
     C         ,' E-CYCLE13'
     D         ,' E-CYCLE14'
     E         ,' E-CYCLE15'
     F         ,' E-CYCLE16'
     G         ,' E-CYCLE17'/
C*
C* Define input parameters - Write banner to terminal
      WRITE(LTT,693)' MTR-OWR comparison AVERAGE VALUES (W%U235)(DIFF.)'
      WRITE(LTT,693)' ================================================='
      WRITE(LTT,692)
C*
C* Define the output file
      WRITE(LTT,691) ' Default output filename              : ',FLNO
      WRITE(LTT,692) '$          Enter new name to redefine : '
      READ (LKB,692) FLNM
      IF(FLNM.NE.BLNK) FLNO=FLNM
C*
C* Define the output file
      IC=1
   11 WRITE(LTT,691) ' Default MTR Burnup Benchmark file  :   ',FLNI(IC)
      WRITE(LTT,692) '$          Enter new name to redefine : '
      READ (LKB,692) FLNM
      IF(FLNM.NE.BLNK) FLNI(IC)=FLNM
      IF(FLNM(1:1).NE.'-') THEN
        OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD',ERR=11)
        CLOSE(UNIT=LIN)
      END IF
C*
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
      WRITE(LOU,693)' MTR-OWR comparison AVERAGE VALUES (W%U235)(DIFF.)'
      WRITE(LOU,693)' ================================================='
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
C*
      WRITE(LOU,694) '       CYC W%U235                            '
      WRITE(LOU,694) ' ============================================'
C
C* Begin processing the MTR Benchmark
      IC=1
      OPEN (UNIT=LIN,FILE=FLNI(IC),STATUS='OLD')
      DO ILIN=1,3
       READ (LIN,692) REC
      ENDDO
      DO N=1,17
      READ (LIN,700)EXPER(N,1)
      READ (LIN,701)CALCUL(N,1),DCALC(N,1)
      ENDDO
C* Print the results for this benchmark
      DEXPE= 2.0
      DO ICASO=1,17
       WRITE(LOU,702) NAME(ICASO),EXPER(ICASO,1),DEXPE
       WRITE(LOU,702)'          ',CALCUL(ICASO,1),DCALC(ICASO,1)
       WRITE(LOU,692)
      ENDDO
      WRITE(LOU,694) ' ============================================'
      WRITE(LOU,719)' Average  '
      WRITE(LOU,719)' Dif/St.Dv'
      WRITE(LOU,718)' # '
 
C
C ACTINIDE ATOMIC DENSITIES
C
      WRITE(LOU,693)' MTR-OWR comparison Atomic Densities for Actinides'
      WRITE(LOU,693)' ================================================='
      WRITE(LOU,692)
      WRITE(LOU,691) ' Processed Benchmark analysis reports : '
      IF(FLNI(1).NE.'-')
     1WRITE(LOU,691) '                              1 case  : ',FLNI(1)
      WRITE(LOU,691)
      DO ILIN=1,2
       READ (LIN,692) REC
      ENDDO
      WRITE (LOU,692) REC
      WRITE(LOU,694) ' ============================================'
      DO ILIN=1,17
       READ(LIN,715,END=300) NAME(ICASO),EXPER(ICASO,1),DEXPE
       WRITE(LOU,717) NAME(ICASO),EXPER(ICASO,1),DEXPE
       READ(LIN,716,END=300) CALCUL(ICASO,1),DCALC(ICASO,1)
       WRITE(LOU,717)'          ',CALCUL(ICASO,1),DCALC(ICASO,1)
       WRITE(LOU,692)
      ENDDO
 300  CLOSE(UNIT=LIN)
      WRITE(LOU,694) ' ============================================'
      WRITE(LOU,719)' Average  '
      WRITE(LOU,719)' Dif/St.Dv'
C
      CLOSE(UNIT=LOU)
C* End of file processing
  80  STOP 'MTRB2 Completed'
C*
 691  FORMAT(2A40)
 692  FORMAT(A120)
 693  FORMAT(A50)
 694  FORMAT(A45)
 700  FORMAT(10X,F6.1)
 701  FORMAT(10X,2F6.1)
 702  FORMAT(A10,F6.1,'(',F4.1,')')
 715  FORMAT(A10,1X,E9.2,2X,E9.2)
 716  FORMAT(11X,E9.2,2X,E9.2)
 717  FORMAT(A10,1X,1PE9.2,'(',1PE9.2,')')
 718  FORMAT(A4)
 719  FORMAT(A10)
C
      END
