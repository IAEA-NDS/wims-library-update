      PROGRAM ENFISS
C-Title  : Program ENFISS
C-Purpose: Retrieve Energy/Mole from fission [J/mole] from an ENDF file
C-M
C-M  Manual for Porgram ENFISS
C-M
C-M  The energy release per fission is extracted from FM 1, MT 458
C-M  data in an ENDF file. The following assumptions are made:
C-M  - The total energy released is equal to the total available
C-M    minus the part carried away by neutrinos (ER).
C-M  - The kinetic energy of the incident neutron is defined
C-M    internally in the code. The values are obtained by
C-M    approximately averaging the energy with the fission
C-M    reaction rate [3]. Averaging is done  based on the constants
C-M    in the updated WIMS-D library based on ENDF/B-VI data.
C-M    The average energy is the simple mean of the group boundary
C-M    values, except for the first three groups where more
C-M    accurate values are taken (7.2747, 4.5933, 2.8615 MeV,
C-M    respectively, which is the average energy in a Maxwellian
C-M    fission spectrum at a temperature corresponding to 1.4 MeV:
C-M             Int( E* sqrt(E)*exp(-E/Temp) *dE )
C-M       Ea =  ----------------------------------
C-M             Int(    sqrt(E)*exp(-E/Temp) *dE )
C-M  - The additional energy released due to gamma activation is
C-M    calculated form
C-M                     Qc = (nubar - 1)*Qbar
C-M    where nubar is the average number of neutrons per fission
C-M    and Qbar=6.1MeV [1,2]. The value of nubar is calculated from
C-M    the constants in the updated WIMS-D library based on ENDF/B-VI
C-M    data [3]. Condensation to one group is done assuming a typical
C-M    light water reactor spectrum used in NJOY for averaging the
C-M    cross sections (IWT option 5).
C-M
C-M  Instructions:
C-M  The name of the evaluated nuclear data file is requested from
C-M  input. The data for the energy released per fission for the
C-M  actinides is processed (MF1, MT 458). The report is written
C-M  to ENFISS.LST file.
C-M
C-References:
C-R [1] M.F.James: Energy Released in Fission, Jou.Nucl.Energy,23(1969).
C-R [2] M.F.James: Energy Released in Fission of Th-232, U-233, U-234,
C-R     U-236, Np-237, Pu-238, Pu-240 and Pu-242,
C-R     Jou.Nucl.Energy,25(1971).
C-R [3] A.Persic, A.Trkov: The energy Released by Neutron Capture in
C-R     Thermal Reactors, International Conference on Nuclear Energy
C-R     in Central Europe '99, Portotoz, 6-9 September 1999.
C-
      CHARACTER*66     REC
      CHARACTER*40     FLNI,FLNO
C*         Avogadro's No., eV to Joules
      DATA AVG/ 0.6022 / , EV/1.60219E-19/
C*
C* Default energy from gamma capture [eV]
      DATA EGAM/ 9.15E6 /
C*
      DATA LIN,LOU,LTT,LKB / 1, 2, 6, 5 /
C*      DATA FLNI/'TAPE20.'/
      DATA FLNO/'ENFISS.LST'/
      DIMENSION ER(10)
C*
      WRITE(LTT, 15)
      WRITE(LTT, 15) ' ENFISS - Energy released per fission   '
      WRITE(LTT, 15) ' ====================================   '
      WRITE(LTT, 15)
    1 WRITE(LTT, 15) '$   Define Evaluated Nuclear Data Lib.: '
      READ(LKB,15)FLNI
      OPEN(UNIT=LIN, FILE=FLNI, STATUS='OLD',ERR=1)
 
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
 
      WRITE(LOU, 15)
      WRITE(LOU, 15) ' ENFISS - Energy released per fission   '
      WRITE(LOU, 15) ' ====================================   '
      WRITE(LOU, 15)
      WRITE(LOU, 15) ' Source evaluated nuclear data file   : ',FLNI
      WRITE(LOU, 15)
      WRITE(LOU, 15) ' Etot=Fission energy excluding neutrinos'
      WRITE(LOU, 15) ' Eeff=Including capt.product decay      '
      WRITE(LOU, 15)
      WRITE(LOU, 15) '   MAT       ZA       AWR        Etot   '
     1              ,'     Eeff        Eeff                   '
      WRITE(LOU, 15) '                                [MeV]   '
     1              ,'    [MeV]    [J/mole]                   '
 
   20 READ (LIN,801,END=90) REC,MAT,MF,MT
 
      IF(MAT.LT.  0) GO TO 90
      IF(MF .NE.  1) GO TO 20
      IF(MT .EQ. 451)THEN
        READ(LIN,803)LIS,LIS0
   25   READ (LIN,801,END=90) REC,MAT,MF,MT
        IF (MT. EQ. 451) GOTO 25
      ENDIF
      IF(MT .NE.458) GO TO 20
C* Section giving energy released per fission
      READ (REC,802) ZA,AWR
      READ (LIN,803) NN,NPLY,NPLY18,NPLY9
      NN=NPLY+1
      DO I=1,NN
        READ (LIN,802)
        READ (LIN,802)
C*      ER=total energy less neutrinos
        READ (LIN,802) ENU,ENUD,ER(I),ERDD,ET,ETDD
      END DO
      READ (LIN,802)
C* Define the energy released by capture EG
C* and kinetic energy of the incoming neutron EN. In lie of other
C* data set: therm.fiss.=0.0025, fast fiss=2MeV.
      EG=EGAM
      EN=0.0025
      IZA=NINT(ZA)
      IF     (IZA.EQ.90230) THEN
        EN= 2.E6
      ELSE IF(IZA.EQ.90232) THEN
        EN= 2.E6
      ELSE IF(IZA.EQ.91233) THEN
        EG=10.1E6
        EN= 3.1E6
      ELSE IF(IZA.EQ.92233) THEN
        EG= 9.1E6
        EN=20000.
      ELSE IF(IZA.EQ.92234) THEN
        EG= 9.7E6
        EN= 1.8E6
      ELSE IF(IZA.EQ.92235) THEN
        EG= 8.8E6
        EN=10000.
      ELSE IF(IZA.EQ.92236) THEN
        EG= 9.4E6
        EN= 1.7E6
      ELSE IF(IZA.EQ.92237) THEN
        EG= 9.4E6
        EN=700000.
      ELSE IF(IZA.EQ.92238) THEN
        EG=10.9E6
        EN= 3.1E6
      ELSE IF(IZA.EQ.93237) THEN
        EG=11.9E6
        EN= 1.9E6
      ELSE IF(IZA.EQ.93239) THEN
        EG=12.1E6
        EN= 1.92E6
      ELSE IF(IZA.EQ.94236) THEN
        EN= 1.E6
      ELSE IF(IZA.EQ.94238) THEN
        EG=11.9E6
        EN=500000.
      ELSE IF(IZA.EQ.94239) THEN
        EG=11.4E6
        EN=8000.
      ELSE IF(IZA.EQ.94240) THEN
        EG=12.7E6
        EN= 1.8E6
      ELSE IF(IZA.EQ.94241) THEN
        EG=11.9E6
        EN=6000.
      ELSE IF(IZA.EQ.94242) THEN
        EG=13.1E6
        EN= 2.2E6
      ELSE IF(IZA.EQ.94244) THEN
        EN=1.E6
      ELSE IF(IZA.EQ.95241) THEN
        EG=14.2E6
        EN=800000.
      ELSE IF(IZA.EQ.95242) THEN
        EG=13.8E6
        EN=1000.
      ELSE IF(IZA.EQ.95243) THEN
        EG=16.5E6
        EN= 2.0E6
      ELSE IF(IZA.EQ.96242) THEN
        EN=1.E6
      ELSE IF(IZA.EQ.96244) THEN
        EN=1.E6
      ELSE IF(IZA.EQ.96246) THEN
        EN=1.E6
      ELSE IF(IZA.EQ.96248) THEN
        EN=1.E6
      ELSE IF(IZA.EQ.98250) THEN
        EN=500000.
      ELSE IF(IZA.EQ.98252) THEN
        EN=500000.
      ELSE IF(IZA.EQ.98254) THEN
        EN=500000.
      END IF
C*
C*      ERT=ER(1)
C*      IF (NN.GT.1) THEN
C*        DO I=2,NN
C*         J=I-1
C*         ERT=(EN**J)*ER(I)+ERT
C*        ENDDO
C*      ELSE
C*        ERT=ERT+EN
C*      ENDIF
C*      EE=ERT+EG
C*
      ERT=ER(1)
      EE=ERT+EN+EG
C*
C* Convert to [Joules/mole *10**-24] (factor included in AVG definition)
      EM=EV*AVG*EE
 
      WRITE(LOU,804) MAT,ZA+0.1*LIS0,AWR,ERT*1.E-6,EE*1.E-6,EM
 
      GO TO 20
 
   90 STOP 'ENFISS Done'
C*
   15 FORMAT(2A40)
  801 FORMAT(A66,I4,I2,I3)
  802 FORMAT(6F11.0)
  803 FORMAT(22X,4I11)
  804 FORMAT(I6,F10.1,F10.4,2F12.4,1P,E12.4)
      END
