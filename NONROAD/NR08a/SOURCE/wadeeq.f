      REAL FUNCTION WADEEQ(FrcFul,TnkSiz,RVP,MinTmp,MaxTmp)
C
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      2004 -dfk- original development
c   7/18/05 -epa- fix AdjMax glitch that would yield tiny negative or
c                 positive VapGen due to floating point calcs when
c                 MaxTmp equals MinTmp, as when both are set to 40F.
c
c-----------------------------------------------------------------------
c
      IMPLICIT NONE
C
      REAL FrcFul  ! Fraction of tank full of fuel
      REAL TnkSiz  ! Tanks size, gallons
      REAL RVP     ! Fuel RVP, psi
      REAL MinTmp  ! Minimum temperature, F
      REAL MaxTmp  ! Maximum temperature, F
      REAL VapSpc  ! Vapor space
      REAL AdjMax  ! Adjusted maximum temperature
      REAL VapPrs  ! Vapor pressure
      REAL PctEvp  ! Percent evap
      REAL DMinTp  
      REAL DMaxTp
      REAL IntPrs  ! Initial pressure
      REAL FinPrs  ! Final pressure
      REAL FlDens  ! Fuel Density
      REAL MolWt   ! Molecular weight
      REAL VapGen  ! Generated vapor (in g/day)
C     
      VapSpc=((1. - FrcFul) * TnkSiz + 0.15 * TnkSiz) / 7.841
C
      if( MaxTmp .GT. MinTmp) then
        AdjMax = (MaxTmp - MinTmp) * 0.992 + MinTmp
C
        VapPrs=1.0223 * RVP + (0.0119 * 3. * RVP) / (1. - 0.0368 * RVP)
C
        PctEvp=66.401 - 12.718 * VapPrs + 1.3067 * VapPrs ** 2 - 
     &    0.077934 * VapPrs ** 3 + 0.0018407 * VapPrs ** 4
C
        DMinTp=PctEvp + ((262. / ((PctEvp / 6.) + 560.)) - 0.0113) * 
     &    (100. - MinTmp)
C
        DMaxTp=PctEvp + ((262. / ((PctEvp / 6.) + 560.)) - 0.0113) * 
     &    (100. - AdjMax)
C
        IntPrs=14.697 - 0.53089 * DMinTp + 0.0077215 * DMinTp ** 2 - 
     &    0.000055631 * DMinTp ** 3 + 0.0000001769 * DMinTp ** 4
C
        FinPrs=14.697 - 0.53089 * DMaxTp + 0.0077215 * DMaxTp ** 2 - 
     &    0.000055631 * DMaxTp ** 3 + 0.0000001769 * DMaxTp ** 4
C
        FlDens=6.386 - 0.0186 * RVP
C
        MolWt=(73.23 - 1.274 * RVP) + (((MinTmp + AdjMax) / 2.) - 60.)
     &    * 0.059
C
        VapGen=VapSpc * 454. * FlDens * (520. / (690. - 4. * MolWt)) * 
     &    ((IntPrs / (14.7 - IntPrs) + FinPrs / (14.7 - FinPrs)) / 2.) *
     &    (((14.7 - IntPrs) / (MinTmp + 460.)) - ((14.7 - FinPrs) / 
     &    (AdjMax + 460.)))
C
      else
        VapGen = 0.
      end if
c
      WADEEQ=VapGen  ! g/day
c     WADEEQ=VapGen/ TnkSiz  ! g/gal
C
      RETURN
      END
