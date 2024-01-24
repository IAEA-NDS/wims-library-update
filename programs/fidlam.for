      PROGRAM FIDLAM
C-Title  : Program FIDLAM
C-Purpose: Fiddle the Am-241 fission cross section
C-Author : A.Trkov, IAEA
C-Version: Sept.2000
C-V  06/09 - Fix thermal fission loop index (lowest energy groups
C-V          were not corrected).
C-V        - Leave fission yield unchanged (unphysical nu-bar but
C-V          correct fission yield for criticality).
C-V        - Change logic to save original file on scratch.
C-M
C-M  Manual for Program FIDLAM
C-M  =========================
C-M
C-M  The program redefines the fission cross section to be
C-M  proportional to the absorption cross section such that the
C-M  capture to fission ratio is equal to CFR, defined in the
C-M  DATA statement in the programs.
C-M    The fission channel in Am-241 has a threshold, therefore
C-M  the capture to fission ratio is strongly spectrum
C-M  dependent. The ratio may reach a value 124 for well
C-M  thermalised lattices and may be as low as 42 for lattices
C-M  with a high content of degraded plutonium. For a typical
C-M  PWR lattice the ratio is about 92.
C-M    Burnup of Am-241 in the WIMS-D library with branching
C-M  to Am-242g and Am-242m can not a be represented accurately
C-M  because the WIMS-D library allows a single capture product
C-M  to be specified. The Am-242m nuclide is considered more
C-M  important from the reactivity point of view, therefore
C-M  its branch is treated explicitly. However, Am-242g is
C-M  important due to the decay into Cm-242 which in turn decays
C-M  into Pu-238. Am-242g production can only be dealt with by
C-M  treating it as a fission product of Am-241 with a yield
C-M  equal to the capture to fission ratio. To avoid spectrum
C-M  dependence of the effective Am-242g production the shape
C-M  of the fission cross section is forced to be proportional
C-M  to the absorption cross section such that the capture to
C-M  fission ratio of 92 is approximately conserved for an average
C-M  lattice. This crude adjustment is tolerable because Am-241
C-M  is not very important from the reactivity point of view.
C-M  Note that the fission yield is left intact, which is
C-M  important for criticality calculation. The effective
C-M  number of neutrons per fission is unphysical. The power
C-M  calculated from the fission cross section may be incorrect.
C-M
C-M  Instructions
C-M  The filename "Am241g.xsw" is fixed. The original contents of
C-M  the file is copied to scratch "FidlAm.tmp" and the corrected
C-M  file is written back to the original file. No other input is
C-M  necessary.
C-
      PARAMETER   (MXGR=200)
      CHARACTER*75 REC
      CHARACTER*40 FLNI,FLSC
C*
      DIMENSION    SGP(MXGR),XSI(MXGR),XXX(MXGR),ALM(MXGR)
     1            ,SGA(MXGR),STR(MXGR),SGF(MXGR),VSF(MXGR)
     2            ,TEMP(10)
C* File units
      DATA FLNI/'Am241g.xsw'/
     1     FLSC/'FidlAm.tmp'/
      DATA LIN,LSC,LTT/ 1, 3, 6 /
C* Default 69-group structure (fast/resonance/thermal)
      DATA NG1,NG2,NG3/ 14, 13, 42 /
C* Alternative 172-group structure (fast/resonance/thermal)
      DATA MG1,MG2,MG3/ 45, 47, 80 /
C*
C* Capture/fission ratio (consistent with reduced Am-242g yield)
      DATA CFR/ 92. /
C*
      IER=0
C*
      WRITE(LTT,900) ' FIDLAM - Adjust Am-241 Fiss. X-sect.   '
      WRITE(LTT,900) ' ====================================   '
      WRITE(LTT,900)
C*
      OPEN (UNIT=LIN,FILE=FLNI,STATUS='OLD')
      OPEN (UNIT=LSC,FILE=FLSC,STATUS='UNKNOWN')
C*
C* Save copy of the original file to scratch
   14 READ (LIN,901,END=16) REC
      WRITE(LSC,901)        REC
      GO TO 14
   16 REWIND LIN
      REWIND LSC
      GO TO 20
C*
C* Try alternative group structure if read-error occurs
   18 IER=IER+1
      IF(IER.GT.1)
     1STOP 'FIDLAM ERROR - Unrecognised format or group structure'
      NG1=MG1
      NG2=MG2
      NG3=MG3
      REWIND LIN
      REWIND LSC
      WRITE(LTT,900) ' FIDLAM has problems reading the file - '
     1              ,'Try alternative 172-group structure     '
C*
C* Copy the burnup data to scratch
   20 READ (LSC,901) REC
      IF(REC(1:15).NE.'      999999999') GO TO 30
   22 WRITE(LIN,901) REC
      READ (LSC,901) REC
      IF(REC(1:15).NE.'      999999999') GO TO 22
      WRITE(LIN,901) REC
      READ (LSC,901) REC
C*
C* Begin processing the cross sections
   30 READ (REC,912) MAT,AWT,IZ,NF,NT
      IF(NF.LT.3) STOP 'FIDLAM is confused - no Fiss.x-sect'
      WRITE(LIN,901) REC
C* Copy the fast cross sections
      N12=NG1+NG2
      READ (LSC,914) (SGP(J),J=1,NG2),(XSI(J),J=1,NG2)
     1              ,(STR(J),J=1,N12),(SGA(J),J=1,N12)
     2              ,(XXX(J),J=1,NG2),(ALM(J),J=1,NG2)
      WRITE(LIN,914) (SGP(J),J=1,NG2),(XSI(J),J=1,NG2)
     1              ,(STR(J),J=1,N12),(SGA(J),J=1,N12)
     2              ,(XXX(J),J=1,NG2),(ALM(J),J=1,NG2)
C* Process the fast fission and fission yield
      READ (LSC,914) (VSF(J),J=1,N12),(SGF(J),J=1,N12)
      RR=1/(CFR+1)
      DO J=1,N12
C...    VV=2.55
C...    IF(SGF(J).GT.0) VV=VSF(J)/SGF(J)
        SGF(J)=RR*SGA(J)
C...    VSF(J)=VV*SGF(J)
      END DO
      WRITE(LIN,914) (VSF(J),J=1,N12),(SGF(J),J=1,N12)
C* Copy the fast scattering matrix
      READ (LSC,915,ERR=18) LSM
      WRITE(LIN,915)        LSM
      LSM=(LSM+4)/5
      DO I=1,LSM
        READ (LSC,901) REC
        WRITE(LIN,901) REC
      END DO
C* Copy the temperatures
      READ (LSC,914) (TEMP(J),J=1,NT)
      WRITE(LIN,914) (TEMP(J),J=1,NT)
C*
C* Process the thermal cross section
      DO IT=1,NT
C* Copy the absorption and transport cross sections
        READ (LSC,914) (STR(J),J=1,NG3),(SGA(J),J=1,NG3)
        WRITE(LIN,914) (STR(J),J=1,NG3),(SGA(J),J=1,NG3)
C* Process the thermal fission and fission yield
        READ (LSC,914) (VSF(J),J=1,NG3),(SGF(J),J=1,NG3)
        RR=1/(CFR+1)
        DO J=1,NG3
C...      VV=2.55
C...      IF(SGF(J).GT.0) VV=VSF(J)/SGF(J)
          SGF(J)=RR*SGA(J)
C...      VSF(J)=VV*SGF(J)
        END DO
        WRITE(LIN,914) (VSF(J),J=1,NG3),(SGF(J),J=1,NG3)
C* Copy the thermal scattering matrix
        READ (LSC,915,ERR=18) LSM
        WRITE(LIN,915)        LSM
        LSM=(LSM+4)/5
        DO I=1,LSM
          READ (LSC,901) REC
          WRITE(LIN,901) REC
        END DO
      END DO
C*
C* Copy the rest of the file
   74 READ (LSC,901,END=90) REC
      WRITE(LIN,901)        REC
      GO TO 74
C*
   90 STOP 'FIDLAM Completed'
C*
  900 FORMAT(2A40)
  901 FORMAT(A75)
  912 FORMAT(I6,F15.0,5I6)
  914 FORMAT(1P,5E15.8)
  915 FORMAT(5I15)
      END
