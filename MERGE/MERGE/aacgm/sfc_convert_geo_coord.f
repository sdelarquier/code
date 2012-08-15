      SUBROUTINE SFC$$CONVERT_GEO_COORD(R_LAT_IN,
     $                                  R_LON_IN,
     $                                  R_HEIGHT_IN,                
     $                                  R_LAT_OUT,
     $                                  R_LON_OUT,
     $                                  I_FLAG,
     $                                  I_ERROR)                     
      REAL*4 R_LAT_IN, R_LON_IN, R_HEIGHT_IN, R_LAT_OUT, R_LON_OUT
      INTEGER*4 I_FLAG, I_ERROR
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     $Revision: 1.6 $
c    The initial version of this subroutine was written by RADEX, INC.
c     for use on VAX/VMS systems.
c
c     Subsequent revisions for UNIX systems have been made by KBB at
c     The Johns Hopkins Univ. Applied Physics Laboratory.  These revisions
c     have been managed using the Revision Control System (RCS) and a
c     log of the revisions will be found at the end of the comments.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C       
C
C     Purpose:
C                                                                    
C         This subroutine uses a set of spherical harmonic coefficients to
C         perform a coordinate conversion between geographic and corrected
C         geomagnetic coordinates (or vice versa). 
C
C         The spherical harmonics for the geographic to corrected geomagnetic
C         coordinate system correspond to a conversion from geographic to
C         the corrected geomagnetic coordinates (expressed in terms of 
C         centered dipole coordinates at the input altitude), and are then  
C         transformed to ground level.  
C
C         The spherical harmonic coefficients used for the inverse are
C         computed relative to centered dipole coordinates at the input
C         altitude. The input CGM coordinates are converted to equivalent 
C         values in this coordinate sytem using the inverse altitude 
C         algorithm before the evaluation of the geographic spherical 
C         harmonic expansion.   
C          
C     Method:  
C
C         This subroutine uses a five-step process in converting a position
C         in one coordinate system to a position in the other. 
C         The five steps are as follows:
C
C         1.  The appropriate spherical harmonic coefficients for the  
C             coordinate conversion are computed for the input altitude.
C
C         2.  The appropriate coordinates for use in the spherical harmonic
C             expansion are computed. For the geographic ==> corrected
C             geomagnetic coordinate version, these are geographic colatitude
C             and longitude. For the inverse coordinate conversion, the 
C             input coordinates are first converted into equivalent dipole
C             coordinates at the input altitude. 
C
C         3.  The Cartesian coordinates of the unit vector in the desired
C             coordinate system are then computed using the appropriate
C             spherical harmonic expansion.
C
C         4.  For the geographic ==> corrected geomagnetic coordinate
C             conversion, the dipole-equivalent coordinates at input
C             altitude are converted to their cgm cartesian coordinates.
C
C         5.  Standard trigonometric identities are used to compute the 
C             latitude and longitude in the desired output coordinate system.
C
C     Input Arguments:
C
C         R_LAT_IN     - REAL*4 - The input latitude in degrees. 
C                        This could be either geographic latitude
C                        or corrected geomagnetic latitude.  The
C                        acceptable range of values is (-90 to +90
C                        degrees).
C
C         R_LON_IN     - REAL*4 - The input longitude in degrees east.     
C                        This could be either geographic longitude
C                        or corrected geomagnetic longitude.  The
C                        acceptable range of values is (0 to 360 degrees).
C
C         R_HEIGHT_IN  - REAL*4 - The height in kilometers for which the
C                        coordinate transformation will be accomplished.
C                        The acceptable range of values is (0 km to 
C                        2000 km)
C
C         I_FLAG       - INTEGER - The flag that indicates which way
C                        the conversion will proceed.
C                     
C                        = 1  convert geographic to corrected
C                                 geomagnetic coordinates
C
C                        = 2  convert corrected geomagnetic
C                                 to geographic coordinates
C
C     Output Arguments:
C
C         R_LAT_OUT   - REAL*4 - The output latitude in degrees.
C                       This could be either the geographic latitude
C                       or corrected geomagnetic latitude.  This 
C                       value will be between -90 and +90 degrees. 
C
C         R_LON_OUT   - REAL*4 - The output longitude in degrees.
C                       This could be either geographic longitude or
C                       corrected geomagnetic longitude.  This 
C                       value will be between 0 and 360 degrees.
C
C         I_ERROR     - INTEGER - The error flag 
C
C                       =  0  normal processing
C                       = -2  R_HEIGHT_IN is outside the allowable range
C                       = -4  I_FLAG value is invalid
C                       = -8  R_LAT_IN is outside the allowable range
C                       = -16 R_LON_IN is outside the allowable range
C                       = -32 Magnitude of the "unit vector" of the 
C                             target coordinate system deviates 
C                             significantly (+/- 10% or more) from 1. 
C                       = -64 For altitudes > 0, for the corrected 
C                             geomagnetic to geographic coordinate
C                             conversion there is a range of latitudes
C                             for which the transformation is invalid.
C                             This flag is set when the requested input
C                             cgm latitude falls within the invalid
C                             region.
C
C     Local Variables:
C
C         C_CLASS          - Character String, used to identify message 
C                            class in error messages.
C
C         C_FLAG           - Character String used to store I_FLAG
C                            as a character string for use in error
C                            message.
C
C         C_HEIGHT_IN      - Character String used to store R_HEIGHT_IN
C                            as a character string for use in error
C                            message.
C
C         C_LAT_IN         - Character String used to store R_LAT_IN
C                            as a character string for use in error
C                            message.
C
C         C_LON_IN         - Character String used to store R_LON_IN
C                            as a character string for use in error
C                            message.
C
C         C_R              - Character String used to store D_R
C                            as a character string for use in error
C                            message.
C
C         C_REP_PARAMS     - Character String used to store error
C                            message.
C
C
C         C_ROUTINE_NAME   - Character String containing name of
C                            subroutine generating error message.
C
C         C_UNDEFINED      - Character String used to store input data
C                            (R_HEIGHT, R_LAT_IN, R_LON_IN) as a   
C                            character string for use in error message.
C
C         D_CINT(,,)       - Double Precision Array (3-D) - 
C                            Contains the spherical harmonic coefficients
C                            interpolated to the input height, R_HEIGHT_IN.
C
C         D_COEF(,,,)      - Double Precision Array (3-D) -
C                            Contains the spherical harmonic coefficients
C                            used to compute the Cartesian coordinates of
C                            unit vector in the target coordinate system.
C
C                            First index: sp. harm. coeff. index
C                            Second       x, y, z components of unit vector
C                            Third        altitude indices 0, 300, 1200 km
C                            Fourth       direction of conversion index
C
C                            coefficients for a given altitude have the form
C                      
C                            a0 + h a1 + h * h a2 where h = alt/1000 [km] 
C
C         D_COLAT_INPUT    - Double Precision - colatitude (radians) in the
C                            input coordinate system
C
C         D_COLAT_OUTPUT   - Double Precision - colatitude (radians) in the
C                            output coordinate system
C
C         D_LON_INPUT      - Double Precision - longitude (radians) in the
C                            input  input system
C
C         D_LON_OUTPUT     - Double Precision - longitude (radians) in the
C                            output coordinate system 
C
C         D_R              - Double Precision - magnitude of the 
C                            unit radius vector in the target coordinate
C                            system.  The target coordinate system is 
C                            the system to which this subroutine is 
C                            converting the latitude and longitude.
C
C         D_X              - Double Precision - the X-component of the
C                            unit radius vector in the target coordinate
C                            system
C
C         D_Y              - Double Precision - the Y-component of the
C                            unit radius vector in the target coordinate
C                            system
C
C         D_Z              - Double Precision - the Z-component of the
C                            unit radius vector in the target coordinate
C                            system 
C
C         D_YLMVAL         - Double Precision - the array of spherical
C                            harmonic basis functions evaluated at
C                            a particular colatitude and longitude.      
C
C
C         I_COND_VALUE     - Integer - the condition value returned from the
C                            call to SFC$$PUT_USER_MSG   
C
C         I_ERR64          - Logical - error flag for CGM_TO_ALTITUDE
C                            Routine.
C
C
C         I_SYSTEM_ERR     - Integer  - Set to the value, SFC$$_NORMAL in
C                            all calls to SFC$$PUT_USER_MSG      
C
C         K                - Integer  - first index of the D_CINT 
C                            interpolated spherical harmonic coefficient
C                            array.
C
C         L                - Integer  - order of each spherical harmonic
C                            coefficient and spherical harmonic function.
C
C         M                - Integer  - zonal index of the spherical
C                            harmonic functions.  
C
C         R_HEIGHT_OLD()   - Real*4 Array (1-D)  - Variable containing
C                            previous height (km) used to determine whether
C                            the interpolation to compute D_CINT() from
C                            D_COEF() needs to be done.
C           
C         R_LAT_ADJ        - Real*4 Variable used to store result of
C                            ALTITUDE_TO_CGM or CGM_TO_ALTITUDE 
C                            conversion.
C
C         R_LAT_ALT        - Real*4 Variable used to store at altitude
C                            dipole latitude.
C
C         R_R              - Real*4 - magnitude of unit vector (= D_R)
C                            (D_X, D_Y, D_Z) in single precision
C
C     Constants:
C
C
C         I_MAX_LENGTH     - Integer  - the length of the D_COEF(,,,)
C                            and D_YLMVAL spherical harmonic function
C                            arrays, corresponding to the order of the
C                            spherical harmonic expansion used.  
C                            I_MAX_LENGTH = (I_ORDER + 1) * (I_ORDER + 1)
C 
C         I_MSGID          - Integer  - contains a message shell number of
C                            4014 which will be used in all calls to
C                            SFC$$PUT_USER_MSG
C
C         I_NUM_AXES       - Integer  - the number of axes in a Cartesian
C                            coordinate system (3)
C
C         I_NUM_FLAG       - Integer  - the number of coordinate trans-
C                            formation flags available (2)
C
C         I_NUM_LEVEL      - Integer  - the number of grid levels 
C                            available.  This is the number of
C                            distinct heights for which there are
C                            spherical harmonic coefficients available
C
C         I_ORDER          - Integer  - the order of the spherical harmonic
C                            expansion used.
C
C         DEGRAD           - Double Precision  - the multiplicative
C                            conversion factor for converting degrees
C                            to radians
C
C         PI               - Double Precision  - mathematical constant
C      
C     Subroutines Required:
C
C         RYLM              - This subroutine returns the spherical harmonic
C                            function value array for a given colatitude and 
C                            longitude.  The spherical harmonics are returned
C                            in the D_YLMVAL array. 
C
C         CG_ALT_DIP       - Given altitude (km), latitude, and a direction
C                            flag, returns altitude corrected latitude, and
C                            an error flag. See comments in subroutine for
C                            additional information.
C
C
C     SFC$$PUT_USER_MSG    - This subroutine is used to send error messages
C                            to the Message Handling System.
C
C     Files Used at Compile Time: 
C
C     SFCLIB$$DEFS$$:SFC$$SFCDEF/LIST 
C
C                          - This include file contains useful system 
C                            constants and definitions. (eg. this is where
C                            SFC$$_NORMAL is defined)
C
C
C     Files Used at Run Time:  None
C
C
C     Databases Accessed:  None
C
C
C     Warnings:  
C
C     1. For certain values of Corrected Geomagnetic Coordinates, the 
C        transformation to Geographic Coordinates (Geocentric) is
C        undefined. When this situation occurs, an error flag is set
C        I_ERROR = -64 and returned to the calling routine.
C
C
C     2. This subroutine contains the non-standard include statement
C
C
C
C
C     Revision History: 
C
C        Original Routine was converted to a standard library
C        subroutine for SFC$$ by MFS on  14 OCT 1992.
C
C
C     09/17/93 SPR1993-0203 (SWS) The return condition value needs to 
C             be initialized before coordinate conversion processing begins
C
C     08/18/93 SPR1993-0175 (SWS) Routine performed a comparison of
C             Z with a -1.D0. The variable Z should be D_Z.
C
C     The present routine represents the use of an improved algorithm for
C     the computation of corrected geomagnetic coordinates and (where
C     it exists) the inverse, together with the use of an updated
C     magnetic field model (IGRF 1995). 
C
C     The present routine incorporates the functionality of the previous
C     routine, and, uses the same subroutine calls. Relevent portions of
C     the old FORTRAN code have been retained where possible.
C
C     The new algorithm and the current code have been developed by 
C     Radex, Inc, 3 Preston Court, Bedford, MA 01730. A technical 
C     report describing the improved algorithm, and discussing its
C     limitations is in preparation, and will be published as a 
C     Philips Laboratory technical report entitled:.
C
C     An Improved Algorithm for the Computation of Corrected Geomagnetic
C     Coordinates, by K. Bhavnani and C. Hein.
C
C     The principal changes implementing the new algorithm and code are
C     as follows:
C
C        (1) The set of spherical harmonic function values are computed 
C            recursively using a single call to a new subroutine RYLM  
C            rather than using repetitive function call. 
C
C            Computationally this method is much faster than computing 
C            each of the spherical harmonics as they are needed.
C
C        (2) The new spherical harmonic coefficients are based upon 
C            the IGRF 95 magnetic field model. The order of the
C            spherical harmonic expansion used here is 10 (previously 
C            was 4).
C
C        (3) The coefficients represent a quadratic fit to the spherical
C            harmonic fits at 0, 300 and 1200 km altitude. The allowable  
C            range of altitudes is 0 - 2000 km. The previous version used
C            polynomial interpolations of the coefficients at 0, 150,
C            300 and 450 km. 
C  
C        (4) The spherical harmonic coefficients were computed in an
C            auxilliary coordinate system, which is aligned with the 
C            desired target coordinate system. This results in a
C            considerable simplification of the code, eliminating the 
C            need to perform multiple coordinate system rotations. 
C
C     17/07/95
C
C     MODIFIED FOR THE IGRF 95 MAGNETIC FIELD MODEL. 
C
C     CHANGES: BLOCKDATA SECTION HAS BEEN REPLACED WITH COEFFICIENTS
C              APPROPRIATE FOR THE IGRF 1995 MODEL
C
C              REFERENCES TO IGRF 90 IN THE COMMENT LINES HAVE BEEN
C              CHANGED TO IGRF 95.
C
C     Subroutine begins:
C
C**** INCLUDE 'SFCLIB$$DEFS$$:SFC$$SFCDEF/LIST'
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   RCS revision log for POSIX systems
c
c $Log: sfc_convert_geo_coord.f,v $
c Revision 1.6  1996/03/20  22:53:03  baker
c Explicitly declared the sizes of the arguments to the
c subroutine.  This should help avoid compatibility
c problems when mixing C and Fortran with different
c machines and different compilers.
c
c Revision 1.5  1996/03/12  18:59:18  baker
c Added code to force a recomputation of the
c conversion coefficients at a given ghheight if the
c coordinates model has changed from the last call.
c
c Revision 1.4  1996/03/11  19:25:36  baker
c Modifications for the 1995 version of AACGM.
c
c
c Revision 1.3  94/10/17  12:35:32  12:35:32  baker (Kile Baker S1G)
c added error code -64 to indicate invalid magnetic coordinates specified
c as input.  This also requires a change in the call to cg_alt_dip
c 
c Revision 1.2  94/10/14  10:53:36  10:53:36  baker (Kile Baker S1G)
c Added the SAVE instruction to make variables static
c 
c Revision 1.1  94/10/12  15:28:38  15:28:38  baker (Kile Baker S1G)
c Initial revision
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SAVE    !make all variables static
C
      COMMON / SFC$$COEFFS_COM / D_COEF
C
      PARAMETER (PI     = 3.141592656589793D0 )
      PARAMETER (DEGRAD = 1.745329251994330D-2)
C
C         Set the order used in this algorithm to 10. 
C     
      PARAMETER (I_ORDER = 10)
C                                
C         Parameterize the maximum values of the indices of the
C         spherical harmonic coefficient array D_COEF( , , , ) and the
C         interpolated spherical harmonic coefficient array, D_CINT( , , ).
C      
      PARAMETER (I_NUM_TERMS = 121)
      PARAMETER (I_NUM_AXES  =   3)
      PARAMETER (I_NUM_LEVEL =   3)
      PARAMETER (I_NUM_FLAG  =   2) 
C
      DOUBLE PRECISION D_CINT(I_NUM_TERMS,I_NUM_AXES,I_NUM_FLAG)
      DOUBLE PRECISION D_COEF(I_NUM_TERMS,I_NUM_AXES,I_NUM_LEVEL,
     $                        I_NUM_FLAG)
C
      DOUBLE PRECISION D_COLAT_INPUT
      DOUBLE PRECISION D_COLAT_OUTPUT
      DOUBLE PRECISION D_LON_INPUT
      DOUBLE PRECISION D_LON_OUTPUT
C                                           
      DOUBLE PRECISION D_X
      DOUBLE PRECISION D_Y  
      DOUBLE PRECISION D_Z
      DOUBLE PRECISION D_R 
C       
      DOUBLE PRECISION D_YLMVAL(I_NUM_TERMS) 
C                                                                  
      REAL R_HEIGHT_OLD(2)
      DOUBLE PRECISION FIRST_COEFF_OLD
C
      CHARACTER*1   C_CLASS
      CHARACTER*300 C_REP_PARAMS
      CHARACTER*24  C_ROUTINE_NAME      
      CHARACTER*8   C_LAT_IN
      CHARACTER*8   C_LON_IN
      CHARACTER*8   C_HEIGHT_IN
      CHARACTER*2   C_FLAG
      CHARACTER*6   C_R 
      CHARACTER*70  C_UNDEFINED 
C
      LOGICAL       I_ERR64
C                                                               
C         Initialize R_HEIGHT_OLD() to impossible values.
C                               
      DATA R_HEIGHT_OLD(1)/-1./
      DATA R_HEIGHT_OLD(2)/-1./
      DATA FIRST_COEFF_OLD/-1./
C
C
C
      DATA C_CLASS /'L'/
      DATA I_MSGID /4014/
      DATA C_ROUTINE_NAME /'SFC$$CONVERT_GEO_COORD'/
C**** DATA I_SYSTEM_ERR /SFC$$_NORMAL/                    
C                                                               
C
C
C     The COMMON block / SPH_HARM_MODEL / array D_COEF contains
C     the spherical harmonic coefficients used to generate the 
C     Cartesian coordinates of the unit vector in the target coordinate
C     system.  The target coordinate system is the system to which
C     the algorithm is converting and is the coordinate system 
C     corresponding to the output variables, R_LAT_OUT and R_LON_OUT. 
C         
C     The coefficient set is stored in a 4-dimensional array with
C     indices defined as follows:
C
C         First Index  - Represents the number of terms used in the 
C                        spherical harmonic expansion. Equal to
C                        (I_ORDER + 1) * (I_ORDER + 1)
C
C         Second Index - Represents the Cartesian coordinate a particular 
C                        coefficient will be used to generate.  Indices are
C                        defined as follows:
C
C                        1  - X-coordinate (the X-axis points from the
C                             center of the earth to where the prime
C                             meridian crosses the equator.
C
C                        2  - Y-coordinate (the Y-axis points from the
C                             center of the earth to 90 degrees east
C                             of the X-axis)
C
C                        3  - Z-coordinate (the Z-axis points from the
C                             center of the earth to the north pole)  
C 
C         Third Index  - Represents the terms of a quadratic fit to
C                        altitude (independent variable h = 
C                        altitude [km]/1200) for a given spharical 
C                        harmonic coefficient. The indices are defined 
C                        as follows:
C 
C                        1  - Constant term: corresponds to the fit at
C                             0 km altitude
C                        2  - linear term
C                        3  - quadratic term
C
C         Fourth Index - Represents the direction of the coordinate 
C                        transformation.  Indices are defined as follows:
C
C                        1  - Conversion of geographic coordinates to 
C                             corrected geomagnetic coordinates.
C
C                        2  - Conversion of corrected geomagnetic coordinates
C                             to geographioc coordinates.  
C
C
C     The Data for D_COEF is provided in a BLOCK DATA file (see below)    
C
C
C     Initialize the error return condition indicator
C
      I_ERROR = 0
C
C     This IF statement checks to see if the magnetic
C     coordinates model has been changed.  If so, we
C     have to force the recalculation of the conversion
C     coefficients, by resetting the values of the old height
C
      IF ( FIRST_COEFF_OLD .NE. D_COEF(1,1,1,1)) THEN
         R_HEIGHT_OLD(1) = -1
         R_HEIGHT_OLD(2) = -1
      ENDIF
      FIRST_COEFF_OLD = D_COEF(1,1,1,1)
C
C
C     The following IF-THEN-ELSE block checks the input arguments to
C     ensure they are within allowable ranges.  If any argument is
C     outside the acceptable range, the error flag, I_ERROR will be
C     set to some non-zero value, and control will be returned to the
C     calling program.
C
      IF ((R_HEIGHT_IN .LT. 0.) .OR. (R_HEIGHT_IN .GT. 2000.)) THEN
C
C     The height in kilometers is outside the allowable range.
C
C     Convert the height value R_HEIGHT_IN to a character string
C     and send an error message to the user.
C
        WRITE(UNIT = C_HEIGHT_IN,
     $        FMT = '(F8.2)',
     $        IOSTAT = I_STATUS) R_HEIGHT_IN
C
        C_REP_PARAMS = C_ROUTINE_NAME//' HEIGHT '//C_HEIGHT_IN
        CALL SFC$$PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
     $                         I_SYSTEM_ERR, I_COND_VALUE)
C
C     Set the error flag to the appropriate value and return
C
        I_ERROR = -2
        RETURN
C
      ELSEIF ((I_FLAG .LT. 1) .OR. (I_FLAG .GT. 2)) THEN
C
C     The conversion flag is neither 1 nor 2.
C
C     Convert I_FLAG to a character string C_FLAG and send an error
C     message to the user
C
        WRITE(UNIT = C_FLAG,
     $        FMT = '(I2)',
     $        IOSTAT = I_STATUS) I_FLAG
C
        C_REP_PARAMS = C_ROUTINE_NAME//' CONVERSION FLAG '//C_FLAG
        CALL SFC$$PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
     $                         I_SYSTEM_ERR, I_COND_VALUE)
C
C     Set the error flag to the appropriate value and return
C
        I_ERROR = -4
        RETURN
C
      ELSEIF (ABS(R_LAT_IN) .GT. 90.) THEN
C
C     The latitude is outside the allowable range.
C
C     Convert R_LAT_IN to a character string C_LAT_IN and send
C     an error message to the user.
C
        WRITE(UNIT = C_LAT_IN,
     $        FMT = '(F8.2)',
     $        IOSTAT = I_STATUS) R_LAT_IN
C
        C_REP_PARAMS = C_ROUTINE_NAME//' LATITUDE '//C_LAT_IN
        CALL SFC$$PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
     $                         I_SYSTEM_ERR, I_COND_VALUE)
C
C     Set the error flag to the appropriate value and return
C
        I_ERROR = -8
        RETURN
C
      ELSEIF ((R_LON_IN .LT. 0.) .OR. (R_LON_IN .GT. 360.)) THEN
C
C     The longitude is outside the allowable range.
C
C     Convert R_LON_IN into a character string C_LON_IN and send
C     an error message to the user.
C
        WRITE(UNIT = C_LON_IN,
     $        FMT = '(F8.2)',
     $        IOSTAT = I_STATUS) R_LON_IN
C
        C_REP_PARAMS = C_ROUTINE_NAME//' LONGITUDE '//C_LON_IN
        CALL SFC$$PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
     $                         I_SYSTEM_ERR, I_COND_VALUE)
C
C     Set the error flag to the appropriate value and return
C
        I_ERROR = -16
        RETURN
C
      ENDIF
C
C     All input arguments are within allowable ranges.
C
C     Compute Spherical Harmonic Coefficients for current
C     altitude if required
C
      IF (R_HEIGHT_IN .NE. R_HEIGHT_OLD(I_FLAG)) THEN
C
      ALT_VAR    = R_HEIGHT_IN/1200.0
      ALT_VAR_SQ = ALT_VAR * ALT_VAR
C
      DO 20 I = 1, 3
C
         DO 10 J = 1, 121
C
         D_CINT(J,I,I_FLAG) = D_COEF(J,I,1,I_FLAG) + ALT_VAR *
     $   D_COEF(J,I,2,I_FLAG) + ALT_VAR_SQ *  D_COEF(J,I,3,I_FLAG)
C
 10      CONTINUE
C
 20   CONTINUE
C
        R_HEIGHT_OLD(I_FLAG) = R_HEIGHT_IN
C
      ENDIF
C
C     Zero Sums for Spherical Harmonic Expansion Computation
C
      D_X = 0.0D0
      D_Y = 0.0D0
      D_Z = 0.0D0
C
C
C     Prepare for Spherical Harmonic Expansion Computation
C
C
      D_LON_INPUT   = DBLE(R_LON_IN) * DEGRAD
C
      IF (I_FLAG .EQ. 1) THEN
C
C     Computing CGM from Geographic Coordinates. No altitude
C     correction required
C
        D_COLAT_INPUT = (90.0D0 - DBLE(R_LAT_IN)) * DEGRAD
C
      ELSE
C
C     Computing Geographic Coordinates from CGM Coordinates.
C
C     Converrt CGM Latitude to Dipole Latitude at Altitude R_HEIGHT
C
        CALL CGM_TO_ALTITUDE(R_HEIGHT_IN,R_LAT_IN,R_LAT_ADJ,I_ERR64)
C
        IF (I_ERR64 .EQV. .TRUE.) THEN
C
            WRITE(UNIT = C_UNDEFINED,
     $          FMT = 90,
     $          IOSTAT = I_STATUS) R_LAT_IN, R_LON_IN, R_HEIGHT_IN
C
            C_REP_PARAMS = C_ROUTINE_NAME//C_UNDEFINED
C
            CALL SFC$$PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
     $                           I_SYSTEM_ERR, I_COND_VALUE)
C
            I_ERROR = -64
            RETURN
        ENDIF
C
        D_COLAT_INPUT = (90.0D0 - DBLE(R_LAT_ADJ)) * DEGRAD
C
      ENDIF
C
C       Generate the spherical harmonics at the coordinate point
C
        CALL RYLM(D_COLAT_INPUT,D_LON_INPUT,I_ORDER,D_YLMVAL)
C
C       Calculate the Cartesian coordinates of the unit vector
C       in the target coordinate system.
C
        DO 40 L= 0, I_ORDER
          DO 30 M = -L, L
C
            K = L * (L + 1) + M + 1
C
C           Add the contribution of each spherical harmonic to the
C           Cartesian components of the unit vector in the
C           appropriate coordinate system.
C
C
            D_X = D_X + D_CINT(K,1,I_FLAG) * D_YLMVAL(K)        
            D_Y = D_Y + D_CINT(K,2,I_FLAG) * D_YLMVAL(K)                     
            D_Z = D_Z + D_CINT(K,3,I_FLAG) * D_YLMVAL(K)  
C                                       
 30       CONTINUE                                                            
 40     CONTINUE 
C             
C       Compute the magnitude of the Cartesian unit vector of the
C       magnetic dipole coordinate system.
C                                                
        D_R = SQRT(D_X * D_X + D_Y * D_Y + D_Z * D_Z) 
C
C       If the magnitude of the unit vector differs significantly 
C       from 1, set the error flag and continue processing.
C                      
        IF ((D_R .LT. 0.9D0) .OR. (D_R .GT. 1.1D0)) THEN          
C
C       The magnitude of the target unit vector differs significantly
C       from 1.  Convert the value D_R into the character string C_R,
C       send a routine message to the user, and continue processing.
C        
          R_R = SNGL(D_R)
C
          WRITE(UNIT = C_R,
     $          FMT = '(F6.2)',
     $          IOSTAT = I_STATUS) R_R          
C
          C_REP_PARAMS = C_ROUTINE_NAME//' UNIT VECTOR '//C_R
          CALL SFC$$PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
     $                           I_SYSTEM_ERR, I_COND_VALUE)   
C
C       Set the error flag to the appropriate value and return.
C
          I_ERROR = -32
          RETURN                                   
        ENDIF 
C         
C           Adjust the components so they do represent the components of
C           a unit vector.  If D_R is equal to 1.0D0, this step will not 
C           change the values of D_X, D_Y, or D_Z. 
C                                                        
        D_Z = D_Z / D_R                                                      
        D_X = D_X / D_R                                               
        D_Y = D_Y / D_R                                                   
C          
C           Obtain output co_latitude and longitude from the unit                                                      
C           vector using standard formulas
C                                                      
        IF (D_Z .GT. 1.0D0) THEN                                     
          D_COLAT_TEMP = 0.0D0                                   
        ELSE IF (D_Z .LT.- 1.0D0) THEN                    
          D_COLAT_TEMP = PI                                
        ELSE                                                   
          D_COLAT_TEMP = DACOS(D_Z)                             
        ENDIF 
C
        IF ((ABS(D_X) .LT. 1.0D-8) .AND. (ABS(D_Y) .LT. 1.0D-8)) THEN        
          D_LON_TEMP = 0.0D0                                                
        ELSE                                                    
          D_LON_TEMP = DATAN2(D_Y,D_X)                              
        ENDIF 
C
C       Prepare Latitude Data for Output     
C                                                                    
       D_LON_OUTPUT   = D_LON_TEMP
C
       IF (I_FLAG .EQ. 1) THEN  
C
         R_LAT_ALT = 90.0 - D_COLAT_TEMP/DEGRAD
C
         CALL ALTITUDE_TO_CGM(R_HEIGHT_IN,R_LAT_ALT,R_LAT_ADJ)
C
         D_COLAT_OUTPUT = (90.0D0 - DBLE(R_LAT_ADJ)) * DEGRAD            
C
       ELSE
C
         D_COLAT_OUTPUT = D_COLAT_TEMP
C
       ENDIF         
C         
C     Convert colatitude into latitude and convert both coordinates
C     from DOUBLE PRECISION radians to REAL degrees. 
C                              
      R_LAT_OUT = SNGL(90.0D0 - D_COLAT_OUTPUT / DEGRAD)          
      R_LON_OUT = SNGL(D_LON_OUTPUT / DEGRAD)
C                                                 
      IF (R_LON_OUT .LT. 0.0) R_LON_OUT = R_LON_OUT + 360.   
C                                                            
C
      RETURN
90    FORMAT(' INVERSE UNDEFINED AT CGM LAT ',F6.2,' CGM LON ',F7.2,
     $ ' ALTITUDE ',F8.2)
      END

c
c	This block data routine initializes the coefficients
c	to the most recent AACGM model.  These default coefficients
c	can be overridden by using the routine aacgm_init.f to
c	read in a different set of coefficients from a data file.
c
      BLOCKDATA CGM_SPH_HARM_COEFFICIENTS
C
C     COEFFICIENTS GENERATED USING THE IGRF 95 MODEL 12/7/95 10:45 AM
C
      DOUBLE PRECISION D_COEF(121,3,3,2)
C
      COMMON / SFC$$COEFFS_COM / D_COEF
C
      DATA (D_COEF(J,1,1,1),J=1,62)/     2.4696818D-02, -9.1721049D-01,
     $  -1.8735510D-01, -3.2032543D-01, -1.3857819D-03,  8.7406641D-03,
     $   5.0730052D-02, -9.8612611D-03,  8.0511389D-04,  1.5091264D-05,
     $   1.4581689D-05,  1.0411974D-02, -1.2812830D-02, -3.0956266D-03,
     $   2.5055037D-03, -2.6251723D-05,  1.0235655D-05,  1.4151064D-04,
     $  -4.1642171D-04,  1.9148532D-03, -7.6473303D-03,  7.5244732D-04,
     $  -4.7608460D-05, -2.2629696D-04,  1.4039413D-05,  6.8773096D-07,
     $  -1.0507161D-05,  2.1704311D-05,  2.9272927D-04,  1.8696591D-04,
     $   4.9370251D-03,  1.6306555D-03,  5.7586065D-05,  2.9783184D-05,
     $   1.2234685D-06, -1.4947513D-06, -1.2784650D-08, -3.6427977D-08,
     $   2.6184649D-06,  4.4784233D-06, -8.5207831D-05,  4.2389063D-04,
     $  -5.8961706D-05,  2.1630833D-04, -1.2026678D-04, -4.1060348D-07,
     $  -2.6399548D-07, -1.1775181D-07,  2.2940802D-08,  1.7471320D-09,
     $  -9.6861310D-10, -1.4201465D-07,  6.3814465D-07,  7.0815908D-06,
     $  -5.8186829D-05, -5.3242523D-05,  1.0216748D-03, -3.3328406D-04,
     $  -7.2465818D-06,  6.0398548D-06,  6.3251099D-07, -2.8121672D-08/
C
      DATA (D_COEF(J,1,1,1),J=63,121)/  -1.3780723D-08, -2.5483942D-09,
     $   8.5910552D-11, -8.3515248D-10,  3.5177295D-09, -2.0224724D-08,
     $  -9.2304295D-08,  3.1300955D-06,  7.0828752D-06, -1.1196158D-04,
     $  -1.2617032D-03, -1.7960457D-04,  1.1618458D-05,  1.3919781D-06,
     $   4.8872799D-08, -3.0522299D-08, -2.8053146D-09, -2.1133599D-10,
     $  -1.9869326D-11, -2.8881556D-12, -5.9069842D-12, -2.5669033D-11,
     $   1.4973667D-09,  1.0835924D-08, -1.5659615D-07, -1.2030191D-06,
     $   1.3625356D-05,  8.2986411D-05, -1.5387498D-04,  1.9159484D-05,
     $   1.4678753D-05,  3.2312671D-07, -1.0429160D-07, -4.2097542D-09,
     $  -3.7769987D-10,  9.2753711D-11, -3.6732835D-12, -8.5412199D-13,
     $   9.8526080D-14, -2.4284619D-13, -6.9151918D-12, -8.3881949D-12,
     $  -6.4518826D-12,  6.7807424D-09,  3.1534189D-08, -7.1587320D-07,
     $  -6.3367331D-06,  4.8567828D-05, -4.6071274D-04,  1.5280550D-04,
     $   2.1846714D-06, -6.8182055D-07, -3.9102300D-08,  1.0070311D-09,
     $   3.8852517D-10,  6.0382915D-11,  2.0385033D-12, -1.3823552D-12,
     $  -1.0459279D-13/
C
      DATA (D_COEF(J,2,1,1),J=1,62)/     1.3150678D-02,  3.0976683D-01,
     $   2.4137941D-03, -9.4995102D-01,  3.5511217D-03, -7.2829117D-03,
     $   2.9401111D-02, -4.0193417D-02, -2.7484903D-03, -5.1535709D-04,
     $   1.8766501D-03, -1.1959477D-03, -4.2623925D-02, -7.9451478D-04,
     $   4.3943004D-03,  5.0647022D-04, -8.8194658D-06, -2.4143098D-04,
     $  -6.4262010D-04, -1.6042203D-04,  9.8902801D-03,  3.9059605D-03,
     $  -1.3494874D-03,  1.3284573D-04, -4.1819929D-05,  2.3494912D-06,
     $  -7.7426698D-06,  2.9616736D-06, -1.4098301D-04,  3.0186759D-05,
     $   6.3129036D-03, -6.6486712D-04, -1.1150691D-04,  3.5674639D-05,
     $  -1.5621612D-05,  7.7080326D-07, -1.8599226D-07,  8.4299182D-07,
     $  -1.9562940D-07,  7.6843398D-07, -8.4475792D-05,  1.3574005D-04,
     $   2.5853951D-03,  1.1289824D-03, -1.4738101D-04, -5.7585400D-06,
     $   6.5241362D-06,  1.2022231D-06,  4.9152356D-08,  2.5275245D-09,
     $  -1.1219630D-08, -1.5400487D-07,  2.2628648D-07,  2.8927308D-06,
     $  -1.6319412D-05, -1.3231500D-04,  2.0187127D-03, -7.4064747D-05,
     $  -9.0645651D-05,  2.2080555D-06,  1.1983585D-06, -6.9309465D-08/
C
      DATA (D_COEF(J,2,1,1),J=63,121)/  -7.4340640D-09, -2.5238242D-09,
     $   5.3449352D-11,  6.5040385D-10,  1.0048905D-08, -2.1566347D-08,
     $  -1.6933128D-07,  1.0382563D-06,  5.8072630D-06, -4.3035574D-05,
     $  -3.9818955D-04, -5.4398961D-04,  1.0313683D-05,  7.8480270D-06,
     $   1.5118493D-07, -8.2434035D-08, -6.0093988D-09, -2.4905059D-10,
     $   2.2828111D-10, -3.1420256D-12, -2.7179951D-11, -1.7198860D-11,
     $   1.8850753D-09,  8.2432349D-09, -7.1125444D-08, -1.5455328D-06,
     $   3.4304494D-06,  8.5192435D-05, -2.6910293D-03,  2.0559311D-05,
     $   4.4337743D-05,  4.6070506D-07, -3.3238872D-07, -1.2310583D-08,
     $   1.7583852D-09,  3.3948360D-10,  1.6641265D-11, -5.8214914D-12,
     $   2.1268306D-13,  4.5048116D-13, -9.8822400D-12, -9.4650941D-11,
     $   4.7184583D-11,  7.8888234D-09,  6.5495530D-08, -2.8440009D-07,
     $  -1.0605738D-05,  2.4564938D-05, -3.8373122D-04,  4.0354043D-04,
     $   7.4762728D-06, -2.8008071D-06, -9.2036786D-08,  8.5278152D-09,
     $   1.0364825D-09,  2.7578961D-11, -1.0398149D-11, -1.7334296D-12,
     $  -6.4645308D-16/
C
      DATA (D_COEF(J,3,1,1),J=1,62)/    -2.7953870D-02, -1.6170878D-01,
     $   9.8661302D-01,  1.3343523D-02,  4.9007123D-04,  2.1771995D-02,
     $   4.9849649D-02,  1.5647170D-02, -2.3930331D-02,  9.8462764D-04,
     $  -1.9176658D-03, -2.3149224D-03, -7.1453565D-03, -1.7823814D-02,
     $  -2.9126191D-03,  2.5277989D-03, -2.0805856D-04,  1.6184112D-04,
     $  -1.9977730D-05, -1.4232700D-03,  1.9019987D-03, -1.9262981D-03,
     $  -7.9549456D-04,  2.5143100D-05, -6.5579130D-07, -1.8115694D-06,
     $  -1.5771707D-05,  2.0404881D-05,  5.4761188D-04,  5.6821893D-04,
     $  -9.0512533D-03, -2.5072262D-03,  4.3859516D-04,  4.5209375D-05,
     $  -3.1545464D-06, -7.8701105D-06,  2.9236557D-07,  3.4598529D-07,
     $  -5.9443911D-07, -4.7160340D-05,  3.5512569D-05,  4.4254802D-04,
     $  -1.3978532D-02,  2.8883235D-03,  4.6141276D-04, -5.3586814D-05,
     $  -9.1945816D-06,  4.1158398D-07,  8.2999660D-07, -1.7720350D-08,
     $  -5.7069255D-08,  2.3118592D-07,  3.0395126D-06, -4.3782358D-06,
     $  -1.1059213D-04,  8.5613183D-05,  3.6940046D-03,  2.7432631D-03,
     $  -1.2763751D-04, -3.3373821D-05,  2.2204218D-07,  3.9293531D-07/
C
      DATA (D_COEF(J,3,1,1),J=63,121)/   3.4801953D-08, -2.2229161D-09,
     $   1.8209396D-10,  1.9583517D-10, -1.5142103D-08, -7.3724313D-08,
     $   4.1856902D-07,  7.2139090D-06, -3.1418463D-05, -2.7898555D-04,
     $   8.7825367D-03, -4.6357960D-04, -2.1130403D-04,  1.4534056D-06,
     $   2.0042898D-06,  1.1111287D-07, -1.2601632D-08, -3.8656443D-09,
     $  -2.5782320D-10, -1.4513005D-12,  9.8582741D-11,  4.0218873D-10,
     $   4.0198434D-10, -4.0143028D-08, -3.5124717D-07,  1.8192724D-06,
     $   2.8842137D-05,  8.5055045D-05, -2.8533763D-03, -1.3281040D-03,
     $  -1.1517732D-06,  1.0599352D-05,  3.3929165D-07, -5.0324863D-08,
     $  -7.4650773D-09, -2.7246735D-10,  1.2995104D-10,  2.8008378D-11,
     $  -1.3306657D-12, -5.6799767D-12, -1.5075146D-12,  1.9979552D-10,
     $   1.6517927D-09,  5.5882055D-09, -5.9597995D-08, -1.5824712D-06,
     $   5.0923480D-06,  6.8049685D-05, -4.2627983D-03, -1.1340053D-04,
     $   6.4696873D-05,  1.6410167D-06, -3.4257863D-07, -2.8672401D-08,
     $  -1.3823089D-10,  2.9362712D-10,  3.0625074D-11,  2.5782022D-13,
     $  -9.2422900D-13/
C
      DATA (D_COEF(J,1,2,1),J=1,62)/    -4.5610261D-03, -5.7028808D-03,
     $   1.3717906D-03,  1.0641473D-03,  6.9726957D-04, -1.2857128D-03,
     $  -1.0726297D-02,  9.1906307D-04,  7.1765970D-04, -5.8700425D-05,
     $  -5.9402338D-05, -3.3761111D-03,  3.8130138D-03,  1.9092767D-03,
     $  -7.9367392D-04, -6.2364763D-06,  1.7624040D-06, -3.3673698D-05,
     $  -4.4703014D-05, -1.7861028D-03,  2.8000447D-03, -7.1143506D-05,
     $   7.5682248D-05,  1.1438020D-04, -3.7386286D-07, -3.9855777D-07,
     $   4.2681253D-06,  3.1705138D-06, -1.1224550D-04, -3.0058874D-04,
     $  -2.2877635D-03, -8.5661704D-04, -1.0715473D-04, -2.3900340D-05,
     $   9.0235216D-07,  1.3091180D-06, -1.3918895D-08,  1.1309179D-07,
     $  -2.1355177D-06,  7.9029865D-07,  2.1106541D-05, -4.4337982D-04,
     $  -6.3046070D-04, -3.1064785D-04,  9.6760314D-05,  4.3466012D-06,
     $   8.8578405D-07, -4.8653276D-08, -1.2724855D-08,  2.0017124D-09,
     $   2.0048833D-09,  4.0576893D-08, -4.4673881D-07, -3.2239045D-06,
     $   4.3529558D-05, -4.8881079D-05, -5.3218034D-04,  6.5576428D-05,
     $  -3.0350053D-06, -2.0477697D-06, -4.1826274D-07, -2.2384947D-08/
C
      DATA (D_COEF(J,1,2,1),J=63,121)/   5.1235701D-09,  3.2649689D-09,
     $  -2.0444843D-10,  1.3789085D-09,  3.0476887D-09, -8.0340968D-09,
     $  -1.3937895D-07, -9.7240569D-07,  2.8737591D-06,  6.5805298D-05,
     $  -7.0643219D-04,  6.3136022D-05,  1.2745418D-05,  9.5167444D-07,
     $  -8.2831046D-08,  7.0934679D-09,  3.2385871D-11,  7.1694347D-10,
     $   2.3496525D-10, -1.6625924D-11, -7.7394533D-11,  2.2645673D-10,
     $   5.6294138D-09,  1.4491696D-08, -3.7433659D-07, -2.5815765D-06,
     $   3.4356998D-06, -2.1605207D-04, -1.3665453D-03, -2.3946460D-04,
     $   3.5937738D-05,  1.9283482D-06, -1.6692551D-07, -4.2857586D-08,
     $  -1.2890259D-09,  7.8518905D-10,  9.7785581D-11, -1.0141687D-11,
     $   8.1081240D-13,  1.3550758D-12, -2.6676016D-11, -3.1774849D-10,
     $   7.1219923D-11,  2.6233417D-08,  1.5393740D-07, -1.6701615D-06,
     $  -1.2216606D-05,  5.7661662D-05, -1.5828935D-04,  2.5336192D-04,
     $   6.6445480D-06, -1.7409339D-06, -1.7281578D-07,  2.5039559D-09,
     $   2.4087767D-09,  1.4240679D-10, -2.8963106D-11, -4.0098532D-12,
     $   7.3469116D-14/
C
      DATA (D_COEF(J,2,2,1),J=1,62)/     2.4776950D-04,  3.5132839D-03,
     $   2.5543325D-04, -5.1588012D-03, -4.8899310D-04,  1.9215974D-03,
     $  -1.1590335D-02,  7.1339540D-03,  2.5182468D-03,  1.2736378D-04,
     $  -7.2377420D-04,  4.2911865D-04,  1.3198408D-02,  2.4478084D-03,
     $  -1.3515313D-03, -4.3516760D-04,  1.2578886D-05,  1.2407275D-04,
     $   6.7963829D-05,  5.8120105D-04, -2.8776941D-03, -5.6679144D-04,
     $   3.6128335D-04, -8.1184683D-05,  2.5303064D-05, -1.8752447D-06,
     $   5.8197510D-06,  9.9946325D-06, -4.0297893D-06, -3.0169336D-04,
     $  -1.1474885D-03,  1.2058761D-04, -2.4167114D-05, -1.2417644D-05,
     $   9.9606019D-06,  3.5781892D-07,  8.3694613D-08, -6.8761107D-07,
     $  -2.2451924D-07,  5.3745436D-06,  7.4761265D-05,  5.0552404D-05,
     $  -2.1890792D-03, -1.0167817D-03,  1.0167316D-04,  1.0333879D-05,
     $  -3.4498420D-06, -9.0834903D-07, -1.8028882D-07,  3.4138935D-09,
     $   9.4275019D-09,  8.3822829D-08, -5.2806674D-09, -2.3925612D-06,
     $   1.6190271D-05,  9.5171181D-05, -1.2220919D-03, -2.0236772D-04,
     $   5.0902763D-05,  4.1721889D-06, -2.5386738D-07, -5.5104376D-08/
C
      DATA (D_COEF(J,2,2,1),J=63,121)/  -1.1887524D-08,  7.1566694D-09,
     $  -3.8915298D-10,  6.2511840D-11, -3.3797181D-10, -1.7107025D-08,
     $  -2.0370435D-07,  5.4812769D-07, -8.5669935D-06,  5.0027233D-05,
     $  -1.7854152D-03,  1.5732466D-04,  3.8744031D-05, -2.2746425D-06,
     $  -7.0715077D-07, -7.5019242D-09,  1.0258559D-08,  1.5425283D-09,
     $  -2.8665160D-10,  2.5765040D-12,  2.8536515D-11,  4.0197077D-10,
     $   1.5825115D-09, -2.1354121D-10, -3.1449754D-07, -8.9452561D-07,
     $   1.5128077D-05,  1.4789777D-04, -5.2197442D-03, -4.6532929D-04,
     $   1.0114842D-04,  2.6572250D-06, -5.5536383D-07, -2.8299651D-08,
     $  -8.7715572D-10, -8.2248366D-11,  4.4511017D-11,  1.7238163D-11,
     $  -4.6049560D-13, -1.5698766D-12, -5.1849849D-12,  2.5368301D-11,
     $   2.2912952D-10,  9.8811732D-09,  6.7003200D-08, -1.1866325D-06,
     $  -8.2832492D-06,  2.5074396D-05, -6.9691006D-04,  7.3459066D-04,
     $   1.0830451D-05, -4.6983429D-06, -1.5934778D-07,  1.6977366D-08,
     $   1.0313413D-09,  5.2082027D-11,  7.5049762D-12,  3.1410875D-13,
     $  -2.0526267D-14/
C
      DATA (D_COEF(J,3,2,1),J=1,62)/     7.5154406D-03, -4.8856028D-03,
     $  -9.2074928D-03, -2.9182384D-02,  5.8318735D-04, -3.6695277D-03,
     $  -1.9874572D-02,  8.4268275D-03,  4.6158068D-03, -3.7682413D-04,
     $   1.1640278D-04,  4.1443765D-04,  2.7333863D-02,  1.4109958D-02,
     $  -1.9378575D-03, -1.4109158D-03,  1.3375156D-04,  1.1753998D-04,
     $  -5.5166307D-05, -1.2824616D-03,  1.8600955D-02, -1.0790456D-02,
     $  -1.1046440D-03,  3.3776883D-04,  4.9158554D-05, -3.9595667D-06,
     $  -1.3773409D-05,  7.7036724D-06,  4.0523625D-04,  7.9261651D-04,
     $  -3.2259405D-02, -1.0007281D-02,  1.3511372D-03,  2.6162012D-04,
     $  -1.8559019D-05, -2.6493026D-07,  3.5299515D-07,  1.4060918D-06,
     $  -5.4657027D-06, -8.3500871D-05,  4.9171503D-06,  1.9068728D-03,
     $  -3.2269985D-02,  7.6445926D-03,  1.3863998D-03, -7.2030967D-05,
     $  -1.9711194D-05, -1.0413205D-06, -4.6450989D-07, -1.8886708D-08,
     $   1.0350703D-09,  7.7147538D-07,  6.9301699D-06, -1.3262363D-05,
     $  -4.6248373D-04, -1.1067100D-03,  2.8733645D-02,  1.1395878D-02,
     $  -6.2034812D-04, -1.5742947D-04, -6.7717640D-07,  1.0570644D-06/
C
      DATA (D_COEF(J,3,2,1),J=63,121)/   1.3225584D-07,  2.6274572D-08,
     $   1.2059296D-09, -7.4717888D-09, -6.6042664D-08, -2.8633291D-07,
     $   2.3348035D-06,  5.7406165D-05, -2.2901602D-05, -1.9802560D-03,
     $   5.0555717D-02, -4.2701005D-03, -1.1587812D-03,  1.2479861D-05,
     $   9.5447303D-06,  5.8621925D-07, -1.3146435D-08, -8.6439248D-09,
     $  -2.3494643D-09,  5.1607020D-11,  6.6842590D-10,  2.5736171D-09,
     $   1.9582097D-09, -2.4127491D-07, -3.6426066D-06,  4.3069290D-06,
     $   3.5194180D-04,  8.6780807D-04, -1.9905296D-02, -1.0381768D-02,
     $   1.8048362D-04,  8.4803460D-05,  2.3917561D-06, -3.0478697D-07,
     $  -4.0719295D-08, -2.5640647D-09,  1.7996742D-10,  7.7232216D-11,
     $  -2.6583256D-12, -2.6583380D-11, -6.2463684D-11,  1.5319335D-09,
     $   1.7805525D-08,  9.8034953D-08, -8.7915325D-07, -3.3938396D-05,
     $   2.1826947D-05,  1.9496114D-03, -5.4163159D-02,  7.5521140D-04,
     $   7.9825592D-04,  1.3910995D-05, -4.1409238D-06, -3.1747899D-07,
     $  -6.4046242D-10,  1.9528208D-09,  2.6716792D-10,  1.2390962D-11,
     $  -2.2386781D-12/
C
      DATA (D_COEF(J,1,3,1),J=1,62)/     4.3142073D-04,  1.6582153D-03,
     $  -5.1021829D-04,  5.6164450D-04, -3.5384090D-04,  1.4769752D-04,
     $   2.3494653D-03,  1.5995529D-04, -4.5089358D-04,  5.0751477D-05,
     $   2.0675902D-05,  4.7554312D-04, -5.5344538D-04, -7.8768634D-04,
     $   1.1983665D-04,  1.2547126D-05, -5.8492988D-06,  7.3933651D-06,
     $   6.0189211D-05,  5.3717491D-04, -1.0928352D-03, -7.3488758D-05,
     $  -2.5108573D-06, -2.7853704D-05, -1.3259684D-06,  1.7627528D-07,
     $  -1.0038652D-06, -4.9540019D-06,  2.0709135D-05,  1.6171120D-04,
     $   6.7239886D-04,  3.1738232D-04,  4.4093750D-05,  7.8326051D-06,
     $  -6.1794162D-07, -5.6611359D-07,  1.5939530D-08, -6.6257663D-08,
     $   8.5800671D-07, -1.0003152D-07, -5.7411696D-06,  1.4864391D-04,
     $   1.4018450D-04,  9.2412255D-05, -3.4400035D-05, -1.6239958D-06,
     $  -4.0098438D-07,  3.8623830D-08,  2.8651992D-09, -1.2290905D-09,
     $   2.8838715D-09, -1.1201416D-09, -4.1703153D-08,  4.6716467D-07,
     $  -9.7018446D-06,  5.5167749D-06, -1.6475630D-04, -3.7679145D-05,
     $   1.3328910D-05,  9.1544795D-07,  9.1087276D-08,  4.8457889D-09/
C
      DATA (D_COEF(J,1,3,1),J=63,121)/  -1.9394414D-09, -7.6305047D-10,
     $   5.5120810D-11, -8.6555063D-10, -1.9633563D-09,  2.3147942D-08,
     $   8.1138774D-08, -8.6130127D-07, -4.2443950D-06,  5.7497735D-06,
     $   4.3828959D-04,  7.9502589D-05, -6.6232660D-06, -1.3944519D-06,
     $  -2.6250167D-08, -2.0555195D-09,  1.6692094D-09, -1.7185162D-10,
     $  -1.7360783D-10,  1.5341207D-11,  5.5460453D-11, -3.1191119D-10,
     $  -5.1533645D-09, -7.4480999D-09,  3.8588586D-07,  2.0837859D-06,
     $  -1.0351590D-05,  1.3932599D-04,  1.3279597D-03,  2.2679617D-04,
     $  -3.7164399D-05, -2.1429620D-06,  1.6599351D-07,  4.0707212D-08,
     $   1.8749085D-09, -6.7592078D-10, -8.4385985D-11,  6.0207558D-12,
     $  -6.0200805D-13, -2.6449182D-13,  2.8589312D-11,  2.2895049D-10,
     $  -5.0551746D-10, -2.2556243D-08, -1.0432292D-07,  1.5749815D-06,
     $   1.0006317D-05, -6.2860604D-05,  4.5155597D-04, -2.4472414D-04,
     $  -9.2260657D-06,  1.4446014D-06,  1.6803196D-07,  9.9068485D-12,
     $  -2.1148145D-09, -1.7542901D-10,  1.8113937D-11,  3.7933227D-12,
     $   1.1507970D-13/
C
      DATA (D_COEF(J,2,3,1),J=1,62)/    -1.1859265D-03, -6.1214193D-04,
     $  -4.8372300D-04,  3.8027297D-03, -1.1449854D-04, -4.1932554D-04,
     $   4.1112846D-03, -5.6972870D-04, -1.2196651D-03,  5.5080962D-06,
     $   1.5903184D-04, -1.1867176D-04, -1.8354205D-03, -1.5406416D-03,
     $   1.7024552D-04,  1.9519325D-04, -6.1856798D-06, -3.2463987D-05,
     $   2.3909642D-05, -1.6940312D-04, -2.2671394D-04, -2.5174807D-04,
     $  -1.3894759D-05,  3.4556941D-05, -7.9907637D-06,  6.8089750D-07,
     $  -1.5316006D-06, -5.0979145D-06,  9.3736449D-06,  1.2983449D-04,
     $  -1.4060236D-04,  2.0026115D-04,  2.7106009D-05, -6.9777397D-07,
     $  -3.1038727D-06, -3.6160073D-07,  6.6065645D-09,  1.8980927D-07,
     $  -7.5932114D-08, -1.4040808D-06, -3.2235516D-05, -5.1745171D-05,
     $   6.9203976D-04,  3.5834912D-04, -3.1142338D-05, -3.5719083D-06,
     $   7.7553046D-07,  2.7591762D-07,  1.0825050D-07, -2.7074329D-09,
     $   1.2205965D-09, -1.6237346D-08, -2.5308798D-07,  2.0687116D-07,
     $   1.4288923D-06, -4.0801554D-06, -4.8487299D-04,  4.8319473D-05,
     $   9.6642283D-06, -1.7607650D-06, -2.9499446D-07,  2.7128134D-08/
C
      DATA (D_COEF(J,2,3,1),J=63,121)/   8.4245526D-09, -3.7109533D-09,
     $   2.3257334D-10, -3.1677005D-10, -1.7571563D-09,  3.0105900D-08,
     $   1.8797925D-07, -1.4710311D-06, -1.3980509D-08,  1.4866070D-05,
     $   1.0613115D-03,  1.9147513D-04, -2.0532561D-05, -1.7610535D-06,
     $   3.0167212D-07,  2.6807441D-08, -3.4083900D-09, -7.7208462D-10,
     $   1.1411343D-10,  6.3559707D-13, -1.3312665D-11, -4.1165748D-10,
     $  -2.1175074D-09,  1.7877616D-09,  3.0526291D-07,  1.3179944D-06,
     $  -1.4388323D-05, -1.4580322D-04,  5.2316839D-03,  4.2457617D-04,
     $  -9.7611861D-05, -3.2079304D-06,  5.2799109D-07,  3.4613263D-08,
     $   9.1266056D-10, -1.1528761D-10, -5.4031302D-11, -1.1348133D-11,
     $   2.3559623D-13,  1.5483051D-12,  1.2580443D-11,  1.5760046D-12,
     $  -5.3594007D-10, -1.0244012D-08, -5.4442156D-08,  1.0431647D-06,
     $   9.6552513D-06, -2.4608488D-05,  9.9355069D-04, -6.4930977D-04,
     $  -1.6219544D-05,  4.0978858D-06,  1.9844539D-07, -1.0378414D-08,
     $  -1.3926467D-09, -1.0914727D-10, -3.2163966D-12,  8.1561529D-13,
     $   1.1684778D-13/
C
      DATA (D_COEF(J,3,3,1),J=1,62)/    -2.1477381D-03,  1.0431620D-03,
     $   6.1922486D-03,  1.0716897D-02, -6.5493907D-04, -2.2337980D-04,
     $   8.8364474D-03, -7.6988397D-03, -2.1275159D-03,  2.2910965D-04,
     $   4.9242803D-04, -3.8643732D-05, -1.8569891D-02, -8.6187473D-03,
     $   1.8876644D-03,  7.2526407D-04, -6.9964222D-05, -1.4807266D-04,
     $   1.7389263D-04,  1.3781501D-03, -1.7097370D-02,  7.8970919D-03,
     $   1.3480985D-03, -2.1969401D-04, -4.7169096D-05,  5.5449637D-06,
     $   1.6130016D-05, -3.8137890D-05, -4.7462115D-04, -3.9683722D-04,
     $   2.4084269D-02,  9.0655706D-03, -1.0053077D-03, -2.2918984D-04,
     $   8.4203244D-06,  2.2549720D-06, -4.1908496D-07, -8.7066107D-07,
     $   7.6276839D-06,  7.6209351D-05, -9.5534624D-05, -1.5733562D-03,
     $   2.9986681D-02, -5.3806984D-03, -1.2420725D-03,  4.4144009D-05,
     $   1.7286970D-05,  1.3431537D-06,  2.6153370D-07,  2.1945225D-08,
     $  -1.0249909D-08, -8.5500876D-07, -5.9666797D-06,  1.9282325D-05,
     $   3.7789935D-04,  5.1512351D-04, -2.0346631D-02, -9.5583713D-03,
     $   4.0055659D-04,  1.2656019D-04,  2.1373769D-06, -7.9054885D-07/
C
      DATA (D_COEF(J,3,3,1),J=63,121)/  -1.1712188D-07, -2.6429392D-08,
     $  -5.3670405D-10,  8.0919437D-09,  5.9767084D-08,  1.5837032D-07,
     $  -2.7405008D-06, -4.4008214D-05,  7.0593704D-05,  1.5307703D-03,
     $  -4.0920820D-02,  2.1958506D-03,  9.4024949D-04,  3.9908321D-06,
     $  -7.3395496D-06, -6.2178744D-07, -6.5645268D-09,  7.0101139D-09,
     $   2.4060515D-09, -4.0184406D-11, -5.6058367D-10, -1.9373125D-09,
     $   3.7512217D-09,  2.3413257D-07,  2.6348983D-06, -6.9822144D-06,
     $  -2.6489421D-04, -4.5063369D-04,  1.2784600D-02,  7.8811194D-03,
     $  -7.0574666D-05, -6.2092751D-05, -2.3720817D-06,  2.0029350D-07,
     $   3.1628528D-08,  2.5267168D-09, -4.0532604D-11, -4.2509191D-11,
     $   1.8595748D-12,  2.0136466D-11,  1.2008328D-11, -1.5908289D-09,
     $  -1.5154854D-08, -4.9731389D-08,  1.0190282D-06,  2.4811541D-05,
     $  -4.5639153D-05, -1.4327912D-03,  4.0979127D-02,  3.4469633D-04,
     $  -6.0319987D-04, -1.7406393D-05,  2.9541589D-06,  2.8236962D-07,
     $   4.3331545D-09, -1.3672691D-09, -2.3613981D-10, -1.4425578D-11,
     $   2.3806491D-12/
C
      DATA (D_COEF(J,1,1,2),J=1,62)/    -2.1123024D-02,  9.3875968D-01,
     $   5.6701105D-02, -3.0529878D-01, -1.6700179D-04, -3.6118760D-02,
     $  -4.1715139D-02,  6.8219990D-03, -1.3584002D-03, -1.3313390D-04,
     $   4.3453446D-03, -2.2857112D-03,  4.6018351D-02,  3.1696952D-03,
     $   3.3106089D-03, -3.4477708D-05, -3.0751470D-05, -2.8118668D-04,
     $  -1.2909405D-03,  7.6568657D-03, -1.5709882D-02, -3.3859391D-03,
     $  -1.2890154D-03, -2.3373165D-04, -9.0327244D-07,  2.8257449D-06,
     $   1.3396059D-05, -4.4545690D-05, -2.6565296D-04, -3.4819039D-03,
     $  -8.7669891D-03,  5.8660908D-04, -1.7182720D-04,  1.2090647D-04,
     $   6.0827697D-06,  2.6708475D-07, -1.8025381D-07, -9.5115528D-07,
     $  -1.9424003D-06,  1.5954642D-05,  6.0546808D-05,  1.6927972D-03,
     $   4.4414937D-03, -1.5514449D-04,  6.2384597D-07, -1.0050935D-05,
     $  -2.8385291D-06, -1.6514295D-06, -3.4745807D-08,  1.3613391D-08,
     $   3.4613104D-09,  1.3069793D-07, -1.1484520D-06, -9.3199914D-06,
     $  -4.7423647D-05, -4.0120230D-04, -2.4772771D-03,  9.0819469D-05,
     $  -7.0055456D-05,  1.3556954D-05,  7.0746823D-08,  2.8923938D-07/
C
      DATA (D_COEF(J,1,1,2),J=63,121)/   3.2821530D-08,  7.5941184D-09,
     $  -2.8819423D-10, -1.0644711D-09,  4.0793941D-09, -8.2598669D-09,
     $   1.3410132D-06, -7.1280800D-07,  7.9321911D-05, -3.9819831D-05,
     $   5.5319096D-03, -1.5434468D-06,  1.0483514D-04,  1.7447320D-06,
     $   1.1680839D-07, -2.6363121D-09, -1.3641397D-08, -1.2510912D-09,
     $  -3.4853961D-10,  7.6443764D-12,  3.8164595D-11,  2.7524259D-12,
     $  -1.9946451D-09, -2.0459969D-08, -3.7404186D-07,  1.7677748D-06,
     $  -2.4131142D-05,  6.6513040D-04, -1.2805783D-03, -2.2946535D-04,
     $  -1.9760317D-05, -5.2505302D-06,  6.6465454D-08, -2.9459711D-08,
     $   3.9523632D-09,  2.4911735D-10,  7.6262687D-11,  1.3093054D-11,
     $  -2.0822419D-13, -7.2965050D-13, -6.4810788D-12,  1.6080620D-10,
     $   4.4396152D-10,  1.4444185D-08, -6.0465003D-08, -9.6796515D-07,
     $  -1.6418007D-05, -3.0992250D-04, -1.2799228D-03,  1.5058443D-04,
     $  -1.7312275D-05,  3.2854991D-06, -6.6423628D-09,  1.7785022D-08,
     $  -3.9677693D-10, -3.4499726D-11, -1.6151746D-11, -2.0209755D-12,
     $  -2.9090389D-13/
C
      DATA (D_COEF(J,2,1,2),J=1,62)/     1.3295816D-02,  3.1844751D-01,
     $  -1.7553523D-01,  9.4989506D-01,  3.4514630D-03,  1.1059391D-03,
     $   3.6522779D-02,  2.1479833D-02,  1.6632014D-03, -2.9216515D-04,
     $   7.3627785D-04,  7.7922017D-03, -1.1375407D-02,  6.1634813D-03,
     $  -8.8867088D-04, -5.9348531D-04, -9.6959334D-06, -3.8328422D-04,
     $   1.7811714D-04, -1.0892060D-03, -1.0093380D-02,  1.6079936D-03,
     $  -2.4237772D-04,  6.0560634D-05,  6.6356639D-05,  2.2790708D-06,
     $   3.3146438D-06, -3.2638110D-05, -2.9474169D-04,  3.1550219D-04,
     $   1.9339373D-03,  1.0479171D-03,  3.9782123D-04, -2.9763021D-05,
     $   1.0250179D-05, -4.2024740D-06, -2.7927048D-07, -1.6073837D-07,
     $  -2.2746712D-06,  6.7022498D-06,  9.6513985D-05, -1.8103695D-04,
     $  -4.1101609D-03,  2.0347667D-04, -1.3242111D-04,  2.5631285D-06,
     $  -3.9045386D-06, -7.5766264D-07,  7.7804658D-08,  9.9591999D-09,
     $   8.9359958D-09, -8.4784362D-08,  6.2591157D-07, -1.3294300D-05,
     $   7.4670349D-05, -1.8060819D-04,  1.7062984D-03,  3.0142732D-05,
     $   6.2185423D-05, -8.0071725D-06, -8.5294356D-07,  1.7167817D-08/
C
      DATA (D_COEF(J,2,1,2),J=63,121)/  -1.8891001D-08,  3.3994624D-09,
     $  -1.0844472D-10,  1.1247859D-09,  3.0894338D-09,  2.6845470D-08,
     $  -2.4080762D-07,  1.7770389D-06, -1.5208063D-05,  4.6845393D-04,
     $  -7.5389082D-04, -1.7798162D-04, -1.4872858D-06, -3.8551244D-06,
     $   5.0207350D-07, -2.8399385D-08,  2.7480451D-09, -1.2673243D-11,
     $  -1.4915471D-10, -1.6996382D-13,  2.5748192D-11,  1.6247677D-10,
     $   2.2129947D-09,  2.0615024D-08, -9.4309320D-08,  2.6454360D-07,
     $  -1.7859117D-05, -1.7566274D-04, -1.3242667D-03,  1.7737104D-04,
     $  -8.6222717D-06,  2.2010124D-06,  7.4409627D-08,  3.8661841D-09,
     $   3.9165949D-10, -1.2013351D-12, -2.9321070D-11,  1.2746365D-11,
     $  -2.4581793D-14, -2.9226050D-12,  4.9534580D-13, -1.1874769D-10,
     $   1.4587347D-10,  5.7650428D-09,  5.3820291D-08,  7.5355441D-07,
     $   1.6497946D-05, -9.0080478D-05,  8.3392122D-04,  3.8635853D-05,
     $   6.6984729D-06,  5.0976629D-07, -8.2661820D-08, -1.0282318D-08,
     $  -5.0773270D-10, -9.3529381D-11,  5.2664763D-12,  1.3431411D-12,
     $  -2.1100892D-14/
C
      DATA (D_COEF(J,3,1,2),J=1,62)/     3.0528473D-02,  6.2934558D-02,
     $   9.6920233D-01,  1.5038045D-01, -1.2881683D-02,  3.1886884D-03,
     $  -2.8839373D-02, -1.6703269D-02, -2.1370839D-02, -9.7500145D-04,
     $  -2.4100366D-03, -1.4899085D-02,  4.9808741D-03,  1.1252311D-02,
     $  -8.3355316D-04,  2.7180608D-03,  1.1013956D-04, -6.6391039D-06,
     $   2.9452767D-04,  2.3844738D-03,  5.7140105D-03,  1.1664442D-04,
     $  -3.1281137D-04,  2.2095935D-05, -1.9548785D-04, -3.2305566D-07,
     $   9.1727988D-06,  6.4934625D-06,  1.0905869D-04, -8.1077548D-04,
     $   4.8882802D-03, -1.4973808D-05,  4.5685866D-05, -3.6614551D-05,
     $  -2.8466214D-06,  3.9683549D-06,  6.1034878D-07, -2.6462926D-07,
     $   4.6856064D-06,  2.9840053D-05,  1.8340079D-04,  3.3721632D-03,
     $   6.6886192D-03, -7.9216693D-04,  2.1635147D-04, -6.4051164D-05,
     $   1.3840697D-06, -1.3008400D-06, -3.2894798D-07, -2.5964240D-09,
     $  -1.5325575D-08, -6.2416359D-08, -4.2791620D-06, -4.4073446D-07,
     $  -2.7668655D-04,  5.1560335D-04, -1.3662886D-02, -1.0475643D-04,
     $  -2.7630946D-04, -8.0454972D-06,  1.1821776D-06, -2.3544152D-08/
C
      DATA (D_COEF(J,3,1,2),J=63,121)/   9.7605908D-08,  6.1518169D-09,
     $  -3.8460648D-10,  1.6951632D-09,  1.8290589D-09,  1.2881464D-07,
     $   3.4921713D-07, -8.6869971D-06,  1.5690063D-05, -2.2463484D-03,
     $   1.1538832D-03,  9.8067541D-04,  2.0556590D-05,  2.5507378D-05,
     $  -3.4824343D-08,  1.2781601D-07, -3.4383990D-09, -2.5682337D-09,
     $  -3.3531204D-10, -5.4766462D-12, -1.0229775D-10, -6.7111291D-10,
     $  -1.3656195D-09, -3.1669250D-08,  9.5633236D-07,  1.4347586D-06,
     $   1.1327474D-04,  5.9319225D-04,  6.7754915D-03, -1.4604343D-04,
     $   9.2431460D-05, -5.7386424D-06, -3.6443170D-07, -3.6322136D-08,
     $  -5.0462705D-09,  3.1478780D-10,  1.3578631D-10,  2.0058190D-11,
     $   1.0240480D-12,  2.6875423D-12,  2.9439717D-11,  4.7300286D-11,
     $  -4.7165200D-10, -1.6462705D-08, -3.6112649D-07,  2.3313504D-07,
     $  -4.1302861D-05,  7.0376760D-04, -3.2603839D-03, -3.8256471D-04,
     $  -3.4001409D-05, -5.4054393D-06,  1.1966121D-07, -6.5115238D-09,
     $   2.7884040D-09,  1.1868725D-10,  3.9965815D-12, -3.7485489D-12,
     $  -7.8310332D-13/
C
      DATA (D_COEF(J,1,2,2),J=1,62)/     1.3779654D-03, -3.1507661D-03,
     $  -5.2593826D-04, -1.4111662D-03,  7.1362528D-04,  4.9066043D-03,
     $   1.2707273D-02, -4.0804805D-04,  1.6881230D-03,  2.9581466D-04,
     $  -1.1116281D-03,  3.8808152D-03, -1.1670557D-02, -2.0225636D-03,
     $  -7.5385237D-04, -2.6362074D-04, -2.0985069D-05,  1.4332489D-04,
     $   8.8132460D-05, -5.7299709D-04,  2.8421868D-03,  4.5516806D-04,
     $   2.0226050D-04,  7.6077387D-05, -4.5496856D-06, -1.0284187D-06,
     $  -1.0231997D-05, -9.4997244D-07, -1.5654090D-04,  3.1686076D-04,
     $  -3.4596088D-03,  3.6909086D-04, -1.3925747D-04, -1.2390101D-05,
     $  -1.9166752D-06,  1.4437737D-06,  1.5544666D-07,  8.4179622D-07,
     $   5.1025384D-06, -1.5393053D-05,  1.1531986D-04, -3.8408736D-03,
     $   3.6347141D-03,  1.3875785D-03,  1.5379267D-04,  5.2026648D-05,
     $   2.4360748D-06,  1.3018274D-06,  6.0748234D-08, -9.3958915D-09,
     $  -2.0372812D-08, -1.5358412D-07,  2.5238512D-06,  1.0988541D-05,
     $   2.6676351D-04,  7.1715057D-04,  1.2108561D-02, -2.1216126D-04,
     $   2.3251274D-04, -1.8295809D-05, -8.6242052D-07, -3.5141100D-07/
C
      DATA (D_COEF(J,1,2,2),J=63,121)/  -1.8565454D-08, -8.1668503D-09,
     $   1.9123442D-11,  3.7955990D-09, -1.1652154D-08,  3.7926020D-08,
     $  -4.0570429D-07,  3.2710733D-06,  7.8344695D-05,  2.4049387D-03,
     $   6.2170647D-03, -1.0224371D-03,  4.5580936D-05, -1.4054575D-05,
     $  -4.4491639D-07,  7.7642316D-08,  8.7829590D-09,  1.2481822D-09,
     $   3.2127417D-10, -1.5487812D-11, -4.0911190D-11,  4.5534147D-10,
     $  -6.8273949D-10,  7.1504887D-08,  8.3324489D-08, -3.0100603D-07,
     $  -2.6488205D-05,  1.9092517D-04,  4.2121135D-04, -1.4232784D-04,
     $   2.4379292D-05,  6.6224745D-06,  1.4110609D-07,  7.0976747D-08,
     $  -3.1176131D-09, -4.4298343D-10, -6.5965670D-11, -8.0144242D-12,
     $   5.4024692D-13, -1.3955008D-12,  6.8198393D-12, -1.6524990D-10,
     $  -4.3387295D-10,  5.2115802D-09,  2.4334184D-08,  1.6673858D-06,
     $  -1.4824850D-05,  3.0906053D-04, -4.6245318D-03, -3.1423728D-05,
     $  -4.4243826D-05, -6.1421922D-07, -3.7399220D-07, -4.5449318D-09,
     $  -2.0890808D-09,  2.0101894D-11,  2.1970383D-11,  4.6316214D-12,
     $   6.7032212D-14/
C
      DATA (D_COEF(J,2,2,2),J=1,62)/    -1.7957280D-03,  1.7066697D-03,
     $  -1.1367924D-03, -8.6970805D-03, -1.8821502D-03, -1.8120878D-03,
     $  -8.7607219D-03, -3.0261837D-03,  1.6769880D-04,  3.0594189D-04,
     $   1.0751787D-04, -3.5242167D-03,  7.7225036D-03, -9.7824631D-04,
     $   5.0525236D-04,  3.3885677D-04,  1.0335260D-05,  1.3825367D-04,
     $   2.6468581D-04,  1.8850538D-03,  6.6040732D-03, -2.5036388D-03,
     $  -1.6785902D-04, -1.2179399D-04, -6.9124506D-05, -3.7378244D-06,
     $  -4.0256598D-06, -2.3178405D-05, -1.3113956D-04,  5.2889119D-04,
     $  -7.7845565D-03, -1.8255838D-03, -2.6986697D-04, -1.3189791D-05,
     $   2.2015206D-06,  4.1371353D-06,  3.6844547D-07,  6.5040607D-07,
     $   2.0975970D-07,  5.6891411D-06, -2.2399826D-04, -1.7812974D-03,
     $  -6.9834244D-04,  8.7721018D-04,  4.6701100D-05,  2.2860842D-05,
     $   7.1247570D-06,  5.5615396D-09,  2.2062778D-08, -1.4290224D-08,
     $  -3.8083091D-08,  2.5627851D-07,  5.3201171D-07,  1.6987074D-05,
     $   1.4186444D-04, -6.0866048D-04,  8.8562850D-03,  4.1419417D-04,
     $   6.7741122D-05,  1.7777770D-05, -3.5997208D-07, -9.0328121D-08/
C
      DATA (D_COEF(J,2,2,2),J=63,121)/   2.0066555D-08, -1.3703456D-08,
     $  -5.3262881D-12,  6.1844613D-10, -5.9995939D-09, -3.4561130D-08,
     $   5.0174373D-07,  4.2888001D-06,  6.3013975D-05,  1.6116345D-03,
     $   2.9343474D-03, -8.9171658D-04,  1.5788363D-05, -1.3463076D-05,
     $  -8.6153105D-07,  6.0231055D-09, -6.7788604D-09,  3.3494927D-10,
     $   5.3078783D-10,  5.6419997D-12, -1.4117214D-11, -5.7032495D-10,
     $   5.8627238D-09, -3.7378689D-08, -7.0370969D-07,  4.2733008D-06,
     $  -1.6330540D-04,  4.5042199D-04, -9.7392183D-03,  1.9284518D-04,
     $  -9.3475831D-05, -4.4000148D-06,  7.8918153D-07, -1.2927404D-08,
     $   6.3074825D-09,  4.9697761D-10, -9.5730959D-11, -1.7781146D-11,
     $  -1.5066777D-13,  4.8060220D-13,  3.0784096D-11, -8.9061049D-11,
     $  -6.4105315D-10,  6.2225245D-08, -2.1608473D-07,  4.1057399D-06,
     $   2.0515749D-05, -1.2904962D-03, -1.8296825D-03,  7.9461555D-04,
     $  -7.5468022D-05,  9.8738415D-06, -5.5551801D-07, -3.4208926D-08,
     $   8.6223550D-10, -5.3555949D-10, -1.7458045D-11,  1.9741260D-12,
     $  -1.0494641D-13/
C
      DATA (D_COEF(J,3,2,2),J=1,62)/    -8.2341683D-03, -2.5272379D-02,
     $   1.2376626D-02,  1.4595159D-02,  3.2618470D-03,  1.1421251D-02,
     $   1.1320354D-02, -9.2060301D-05,  2.7481924D-03,  3.4909080D-04,
     $  -1.0234980D-03,  7.3007955D-03, -2.5584835D-02, -5.3536176D-03,
     $  -1.6306596D-03, -1.2956419D-03, -6.7714442D-05, -9.0007099D-05,
     $  -4.1099321D-04, -1.1514135D-02, -4.3338449D-03,  4.2914901D-03,
     $   2.7085115D-04,  2.8771048D-04,  1.3308161D-04,  4.2634415D-06,
     $   1.6353461D-05, -4.7719147D-06,  1.2461936D-03, -2.0956168D-03,
     $   3.8640240D-02,  1.2519431D-03,  1.1518616D-03,  9.8543573D-05,
     $   2.5411917D-06, -1.5275336D-06, -5.9602309D-07, -2.3314249D-07,
     $   3.4832121D-06,  3.0622963D-05,  2.4889155D-04,  1.0304877D-02,
     $   4.1366701D-03, -5.0078854D-03,  3.1357235D-05, -1.3315830D-04,
     $  -6.1454572D-06,  9.7219726D-07, -7.3858503D-08, -1.4483454D-09,
     $   7.4597793D-08, -5.7817424D-07, -5.1227415D-06,  9.8464227D-07,
     $  -8.7519638D-04,  2.8412199D-03, -5.1536653D-02, -1.4342534D-03,
     $  -6.8818196D-04, -4.0228516D-05,  2.1476502D-07, -1.7853362D-07/
C
      DATA (D_COEF(J,3,2,2),J=63,121)/  -9.8304723D-08,  2.2299738D-08,
     $   1.8211837D-09, -1.1093658D-08,  1.6340232D-08, -3.4129086D-08,
     $  -2.6833055D-06, -1.5309555D-05, -3.4630845D-04, -9.2664097D-03,
     $  -1.7834053D-02,  4.2570712D-03, -2.1006490D-04,  6.9784104D-05,
     $   2.4357896D-06, -1.8613788D-07,  3.6404913D-08,  5.2879768D-10,
     $  -3.0729055D-10, -1.5273898D-11,  3.0091847D-10,  1.0044048D-09,
     $  -7.3349632D-09,  1.6777035D-07,  3.4052191D-06,  4.0874911D-06,
     $   7.0925826D-04, -2.1052232D-03,  5.9805159D-02,  1.3793714D-03,
     $   5.7236314D-04,  2.1163176D-05, -6.7969282D-07,  3.1564248D-08,
     $   4.0032499D-09, -2.2755770D-09,  2.3868737D-10, -1.6537715D-11,
     $  -1.4333357D-12, -7.2001622D-12, -5.2047256D-11,  1.0819406D-09,
     $  -7.4485923D-09, -1.6613207D-08,  6.1771389D-08,  6.3193172D-06,
     $   1.7674134D-04,  6.5300269D-03,  1.4204553D-02, -3.1977263D-03,
     $   7.5942751D-05, -3.4775988D-05, -8.4735747D-07,  1.6297179D-08,
     $   1.3193131D-09, -7.9428169D-11,  8.7359989D-11, -6.3748854D-12,
     $   1.3755085D-12/
C
      DATA (D_COEF(J,1,3,2),J=1,62)/     1.1013883D-03,  3.8962803D-03,
     $   7.6177692D-04, -7.5267052D-04, -7.2972519D-04,  3.1892858D-04,
     $  -4.8514624D-03, -4.1887231D-04, -1.0472941D-03, -1.5805848D-04,
     $   1.2523637D-05, -2.4804323D-03,  2.1264085D-04,  9.7808400D-04,
     $  -1.4376837D-05,  1.9825512D-04,  1.9870308D-05, -4.3025710D-05,
     $   1.4999369D-04, -1.0110545D-03,  2.0136490D-03,  3.9322516D-04,
     $   1.2469719D-04,  1.1010576D-05,  3.5521748D-06,  6.8996076D-08,
     $   4.2828904D-06,  1.1648134D-05,  1.7977186D-04,  7.7563618D-04,
     $   4.3433416D-03, -4.4162313D-04,  1.3361210D-04, -1.8050047D-05,
     $  -2.4063276D-07, -1.2420996D-06, -7.8196888D-08, -3.3507901D-07,
     $  -2.8941794D-06,  5.4667723D-06, -9.1192352D-05,  2.3253687D-03,
     $  -2.9736147D-03, -9.8344703D-04, -9.7409305D-05, -3.1857448D-05,
     $  -1.1062812D-06, -4.1336986D-07, -4.9383791D-08,  3.8060756D-09,
     $   1.2477701D-08,  7.1359607D-08, -1.3706805D-06, -5.3354177D-06,
     $  -1.7823537D-04, -3.7421020D-04, -7.8788475D-03,  1.1066643D-04,
     $  -1.3850506D-04,  9.4270609D-06,  5.8097507D-07,  1.8696763D-07/
C
      DATA (D_COEF(J,1,3,2),J=63,121)/   4.8993695D-10,  4.3438520D-09,
     $   1.2570854D-10, -2.4087434D-09,  9.1111259D-09, -3.0822202D-08,
     $  -7.7493882D-08, -1.6761387D-06, -8.8650842D-05, -1.6525646D-03,
     $  -6.5643101D-03,  7.2655863D-04, -7.0518122D-05,  8.9788076D-06,
     $   2.8888375D-07, -5.6998064D-08, -2.8826396D-09, -2.1831339D-10,
     $  -1.8295172D-10,  5.4777731D-12,  1.7370619D-11, -4.0786593D-10,
     $   9.7531203D-10, -4.7482317D-08,  9.0354872D-08, -2.9579999D-07,
     $   2.9042666D-05, -4.0331263D-04,  4.3069584D-04,  2.0597850D-04,
     $  -7.8689767D-06, -3.1039300D-06, -1.3612861D-07, -4.5591729D-08,
     $   1.2252155D-09,  1.8081867D-10,  3.2799193D-11,  1.8195042D-12,
     $  -2.4775176D-13,  1.1224100D-12, -2.9982739D-12,  6.1616407D-11,
     $   7.5391956D-11, -1.4153254D-08, -3.8201614D-08, -6.1933642D-07,
     $   1.4837421D-05,  4.8697658D-05,  3.5311098D-03, -9.9360840D-05,
     $   3.1406450D-05, -1.8947911D-06,  2.7018384D-07, -8.1011602D-09,
     $   1.7593425D-09,  1.2057331D-11, -9.4158351D-12, -2.9179101D-12,
     $  -3.7394824D-14/
C
      DATA (D_COEF(J,2,3,2),J=1,62)/    -3.2464597D-04, -1.6765495D-03,
     $   7.0925541D-04,  3.2370222D-03,  1.1581050D-03,  9.8152103D-04,
     $   2.8305594D-03, -7.2358600D-05, -7.9249732D-05, -1.7546988D-04,
     $  -2.1970053D-04,  1.4841870D-03, -3.5153509D-03, -6.5014660D-04,
     $  -1.6392829D-04, -1.8782341D-04, -7.7078799D-06, -1.8262483D-05,
     $  -2.8795882D-04, -1.1999059D-03, -3.3028448D-03,  1.1843382D-03,
     $   6.0666167D-05,  5.9324618D-05,  4.0502165D-05,  2.5596654D-06,
     $   1.5886318D-06,  2.4833268D-05,  1.7580599D-04, -6.8124960D-04,
     $   5.4170890D-03,  1.0891375D-03,  1.1459107D-04,  2.5543342D-05,
     $  -3.7386297D-06, -2.1118588D-06, -2.2501108D-07, -3.6785581D-07,
     $   5.7691090D-07, -7.1939979D-06,  1.6017837D-04,  1.3486305D-03,
     $   1.9824505D-03, -7.9746742D-04, -6.0885779D-06, -1.8368522D-05,
     $  -4.8901208D-06,  2.4821080D-07, -3.1859231D-08,  8.0706920D-09,
     $   2.7282141D-08, -1.9574991D-07, -5.3049528D-07, -9.0563262D-06,
     $  -1.3793181D-04,  5.5900075D-04, -7.1010404D-03, -3.8115192D-04,
     $  -7.0754273D-05, -1.2043782D-05,  8.4782103D-07,  9.0919726D-08/
C
      DATA (D_COEF(J,2,3,2),J=63,121)/  -6.8975148D-09,  1.0107138D-08,
     $   3.5870228D-11, -7.5949953D-10,  6.1819873D-09,  3.0048133D-08,
     $  -2.3085012D-07, -2.4442435D-06, -4.2998365D-05, -1.3238515D-03,
     $  -1.9458929D-03,  6.9292995D-04, -1.1544405D-05,  1.1183042D-05,
     $   5.4628952D-07, -1.0401795D-08,  2.2769644D-09, -4.2846275D-10,
     $  -4.6826582D-10, -1.6977941D-12,  1.7495800D-11,  3.8093980D-10,
     $  -5.3912574D-09,  3.4728058D-08,  5.4259353D-07, -2.7409137D-06,
     $   1.3262451D-04, -2.9078727D-04,  8.0966883D-03, -1.7818588D-04,
     $   6.8026837D-05,  2.8213160D-06, -7.1405840D-07,  6.3550953D-09,
     $  -4.5372448D-09, -4.5153978D-10,  9.4339623D-11,  9.6204235D-12,
     $   7.3839307D-14,  2.0775852D-13, -2.4103782D-11,  1.4160312D-10,
     $  -3.2091424D-10, -5.1461455D-08,  1.5137162D-07, -4.5547744D-06,
     $  -1.9717549D-05,  9.2958754D-04,  8.4188869D-04, -6.4360713D-04,
     $   5.1370694D-05, -7.4259585D-06,  3.7801866D-07,  3.8199157D-08,
     $  -9.0983544D-10,  3.9658006D-10,  1.4404631D-11, -2.5793883D-12,
     $   2.6145436D-13/
C
      DATA (D_COEF(J,3,3,2),J=1,62)/     2.5319098D-03,  9.3805531D-03,
     $  -6.2217927D-03, -5.1601624D-03, -1.8165119D-03, -6.9122206D-03,
     $  -4.7697970D-03,  1.9971441D-03, -5.9488631D-04, -5.9733842D-05,
     $   1.1786245D-03, -3.9008107D-03,  1.7404168D-02,  2.3733491D-03,
     $   1.1984592D-03,  6.0430254D-04,  3.4658261D-05,  5.3897689D-05,
     $   4.7050445D-04,  7.8511861D-03,  4.0499334D-03, -3.3505989D-03,
     $   5.4381292D-05, -2.1339195D-04, -6.2155417D-05, -4.3358559D-06,
     $  -1.2736887D-05, -1.8698093D-06, -9.8041921D-04,  2.3944746D-03,
     $  -3.0421617D-02, -1.2727042D-03, -8.4042401D-04, -8.9216667D-05,
     $   1.2151333D-06,  2.8145894D-07,  3.7715415D-07,  1.1733539D-07,
     $  -4.1753602D-06, -2.2917365D-05, -2.9017258D-04, -8.3132945D-03,
     $  -5.9529795D-03,  3.9574091D-03, -1.0869776D-04,  1.0989758D-04,
     $   5.6769675D-06, -6.3300238D-07,  1.1064353D-07,  1.6147107D-09,
     $  -4.8002745D-08,  5.3606586D-07,  4.5731935D-06,  1.7591166D-06,
     $   7.5098646D-04, -2.4084090D-03,  4.2170476D-02,  1.2862028D-03,
     $   5.8158723D-04,  3.6647514D-05, -1.1969776D-06,  7.2231246D-08/
C
      DATA (D_COEF(J,3,3,2),J=63,121)/   4.7584065D-08, -2.3398800D-08,
     $  -1.2154108D-09,  7.6608771D-09, -2.4096573D-08,  1.6142808D-08,
     $   1.5102698D-06,  8.7031438D-06,  2.5808150D-04,  7.1111246D-03,
     $   1.2320928D-02, -3.4653406D-03,  1.3788058D-04, -5.4947267D-05,
     $  -2.1009645D-06,  1.6621788D-07, -1.6906316D-08, -5.3728041D-10,
     $   8.2605948D-10, -5.0015875D-12, -1.7835702D-10, -7.1884443D-10,
     $   5.4401424D-09, -1.2722541D-07, -2.7153665D-06, -4.5419248D-06,
     $  -5.7576393D-04,  1.3853414D-03, -4.7118355D-02, -1.0557495D-03,
     $  -4.4146002D-04, -1.4772190D-05,  9.2174260D-07, -2.1147381D-09,
     $  -1.4166116D-09,  1.6296997D-09, -2.0420518D-10,  1.4014690D-11,
     $   1.3966039D-12,  4.5492395D-12,  2.4834311D-11, -7.5354511D-10,
     $   6.5658197D-09,  1.6130000D-08,  2.2967961D-07, -2.2379537D-06,
     $  -1.1123939D-04, -4.7324749D-03, -8.6193793D-03,  2.4858682D-03,
     $  -3.1700902D-05,  2.5742128D-05,  6.2853779D-07, -2.1079462D-08,
     $  -3.5249985D-09,  1.4025098D-10, -6.8375998D-11,  5.5879333D-12,
     $  -1.6864359D-12/
C
      END
