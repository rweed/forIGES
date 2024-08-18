! This file is part of the forIGES libraries
! https://github.com/rweed/forIGES

! Copyright (C) 2024 Richard Weed.
! All rights reserved.

! Redistribution and use in source and binary forms, with or without 
! modification, are permitted provided that the following conditions are met:

! 1. Redistributions of source code, in whole or in part, must retain the  
! above copyright notice, this list of conditions and the following 
! disclaimer.

! 2. Redistributions in binary form, in whole or in part, must reproduce the 
! above copyright notice, this list of conditions and the following disclaimer 
! in the documentation and/or other materials provided with the distribution.

! 3. The names of the contributors may not be used to endorse or promote from 
! products derived from this software without specific prior written 
! permission.

! 4. Redistribution of this source code, including any modifications, may 
! not be intentionally obfuscated.

! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
! IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND
! CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
! EXEMPLARARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
! OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
! WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
! OTHERWISE), ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
! ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Module IGES_Psection 

!! Classes and methods for processing IGES P section entity types read from
!! IGES CAD files including converting file input records to entity type 
!! components, writing user model entities to an IGES format file, and printing
!! components of entity types to a user defined output unit

!! Author  : Richard Weed

!! Version : 0.1 (Beta release 1)
!! Date    : August 18, 2024

  USE IGES_params
  USE IGES_utils
 
  Implicit NONE

  Type, Abstract :: entity_t
  !! Abstract type for a P section entity. The following components are shared
  !! by all types extending entity_t

    Integer      :: entity_type  = -1      
    !! Entity type number
    Integer      :: DEP          = 0
    !! Dictionary section array index corresponing to this entity
    Integer      :: form         = 0
    !! Alternative form for this entity
    Integer      :: tmat         = 0
    !! Dictionary section entry for a transformation matrix for this entity
    Integer      :: thisSeqNo    = 0
    !! Corresponding starting sequence number in IGES file section for this
    !! entity
    Character(8) :: entity_label = "        "
    !! Eight character ASCII label for this entity
    Logical      :: has_DEpointers = .FALSE.
    !! Logical flag to signal entity contains Directory Entry indicies/pointers

  Contains

    Procedure(gPdata), deferred :: getEntity
    !! Deferred method for translating IGES file records into an entity type
    Procedure(sPdata), deferred :: makeEntity
    !! Deferred method for translating an entity type into IGES file records 
    Procedure(oPdata), deferred :: outputEntity
    !! Deferred method for printing out components of an entity type
    Procedure(dPdata), deferred :: dealloc
    !! Deferred method for deallocating an entity type

  End Type

  Abstract Interface

    Subroutine gPdata(this, Precords, form) 
    !! Deferred method for translating IGES file records into an entity type
      IMPORT :: entity_t
      Class(entity_t),           Intent(INOUT) :: this
      Character(80),             Intent(IN)    :: Precords(:)
      Integer,         Optional, Intent(IN)    :: form 
    End Subroutine gPdata

    Subroutine sPdata(this, Precords) 
    !! Abstract interface for deferred method for translating an entity type
    !! into IGES file records 
      IMPORT :: entity_t
      Class(entity_t),              Intent(IN)    :: this
      Character(80),   Allocatable, Intent(INOUT) :: Precords(:)
    End Subroutine sPdata

    Subroutine dPdata(this) 
    !! Abstarct interface for deferred method for deallocating an entity type
      IMPORT :: entity_t
      Class(entity_t), Intent(INOUT) :: this
    End Subroutine dPdata

    Subroutine oPdata(this,ounit) 
    !! Abstract interface for deferred method for printing out contents of an
    !!  entity type
      IMPORT :: entity_t
      Class(entity_t), Intent(IN) :: this
      Integer,         Intent(IN) :: ounit
    End Subroutine oPdata

  End Interface

  Type :: PE_t
  !! Container for a polymorphic entity
    Class(entity_t), Allocatable :: entity

  End Type

  Type :: Psection_t
  !! Container for an array of polymorphic entities
    Type(PE_t), Allocatable :: PE(:)
  End Type

! Begin definition of individual entity types

  Type, Extends(entity_t) :: unsupported_t
  !! Unsupported entity 

  Contains

    Procedure :: getEntity    => getUnsupported
    !! Translate IGES file records into an unsupported_t entity type 
    Procedure :: makeEntity   => makeUnsupported 
    !! Translate an unsupported_t entity type into a P record array 
    Procedure :: outputEntity => outputUnsupported
    !! Output the components of an unsupproted_t entity type
    Procedure :: copyTo       => copyToUnsupported
    !! Copy this unsupported entity type to an output unsupported entity 
    Procedure :: initType     => initTypeUnsupported
    !! Copy an input unsupported entity to this unsupported entity
    Procedure :: dealloc      => deallocUnsupported
    !! Deallocate an unsupported entity

  End Type

  Type, Extends(entity_t) :: entity0_t
  !! Null entity

  Contains

    Procedure :: getEntity    => getE0 
    !! Translate IGES file records into an entity0_t entity type 
    Procedure :: makeEntity   => makeE0 
    !! Translate entity0_t entity type into IGES P records 
    Procedure :: outputEntity => outputE0
    !! Output the components of an entity0_t entity type 
    Procedure :: copyTo       => copyToE0
    !! Copy this entity0_t type to an output entity0_t 
    Procedure :: initType     => initTypeE0
    !! Copy an input entity0_t type to this entity0_t 
    Procedure :: dealloc      => deallocE0
    !! Deallocate an entity0_t entity
    Procedure :: init         => initE0
    !! Initialize an entity0_t entity with user data

  End Type
 
  Type, Extends(entity_t) :: entity100_t
  !! Circular Arc Entity
 
    Real(WP) :: ZT = 0.0_WP !! Parallel Zt displacement of arc from Xt,Yt plane
    Real(WP) :: X1 = 0.0_WP !! Arc center abscissa
    Real(WP) :: Y1 = 0.0_WP !! Arc center ordinate
    Real(WP) :: X2 = 0.0_WP !! Start point abscissa
    Real(WP) :: Y2 = 0.0_WP !! Start point ordinate
    Real(WP) :: X3 = 0.0_WP !! Terminate point abscissa
    Real(WP) :: Y3 = 0.0_WP !! Terminate point ordinate

  Contains

    Procedure :: getEntity    => getE100 
    !! Translate IGES file records into an entity100_t entity type 
    Procedure :: makeEntity   => makeE100 
    !! Translate entity100_t entity type into IGES P records 
    Procedure :: outputEntity => outputE100
    !! Output the components of an entity100_t entity type 
    Procedure :: copyTo       => copyToE100
    !! Copy this entity100_t type to an output entity100_t 
    Procedure :: initType     => initTypeE100
    !! Copy an input entity100_t type to this entity100_t 
    Procedure :: dealloc      => deallocE100
    !! Deallocate an entity100_t entity
    Procedure :: init         => initE100
    !! Initialize an entity100_t entity with user data

  End Type
 
  Type, Extends(entity_t) :: entity102_t
  !! Composite Curve Entity
 
    Integer              :: N = 0 !! Number of Entities
    Integer, Allocatable :: DE(:) !! Array of DE indexes of constituent curves

  Contains

    Procedure :: getEntity    => getE102 
    !! Translate IGES file records into an entity102_t entity type 
    Procedure :: makeEntity   => makeE102 
    !! Translate entity102_t entity type into IGES P records 
    Procedure :: outputEntity => outputE102
    !! Output the components of an entity102_t entity type 
    Procedure :: copyTo       => copyToE102
    !! Copy this entity102_t type to an output entity102_t 
    Procedure :: initType     => initTypeE102
    !! Copy an input entity102_t type to this entity102_t 
    Procedure :: dealloc      => deallocE102
    !! Deallocate an entity102_t entity
    Procedure :: init         => initE102
    !! Initialize an entity102_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity104_t
  !! Conic Arc Entity 
 
    Real(WP) :: A  = 0.0_WP !! Conic Coefficient
    Real(WP) :: B  = 0.0_WP !! Conic Coefficient
    Real(WP) :: C  = 0.0_WP !! Conic Coefficient
    Real(WP) :: D  = 0.0_WP !! Conic Coefficient
    Real(WP) :: E  = 0.0_WP !! Conic Cofficient
    Real(WP) :: F  = 0.0_WP !! Conic Coefficient
    Real(WP) :: ZT = 0.0_WP !! Zt Coordinate of plane of definition
    Real(WP) :: X1 = 0.0_WP !! Start Point Abcissa
    Real(WP) :: Y1 = 0.0_WP !! Start Point Ordinate
    Real(WP) :: X2 = 0.0_WP !! Terminate Point Abscissa
    Real(WP) :: Y2 = 0.0_WP !! Terminate Point Ordinate

  Contains

    Procedure :: getEntity    => getE104 
    !! Translate IGES file records into an entity104_t entity type 
    Procedure :: makeEntity   => makeE104 
    !! Translate entity104_t entity type into IGES P records 
    Procedure :: outputEntity => outputE104
    !! Output the components of an entity104_t entity type 
    Procedure :: copyTo       => copyToE104
    !! Copy this entity104_t type to an output entity104_t 
    Procedure :: initType     => initTypeE104
    !! Copy an input entity104_t type to this entity104_t 
    Procedure :: dealloc      => deallocE104
    !! Deallocate an entity104_t entity
    Procedure :: init         => initE104
    !! Initialize an entity104_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity106_t
  !! Copious data entity (forms 1-3, 11-13, 63) 
 
    Integer               :: IP = 0 !! Integer Interpretation flag
                                    !! 1 = x,y pairs common Z
                                    !! 2 = x,y,z coordinates
                                    !! 3 = x,y,z coordinates and i,j,k vectors
    Integer               :: N  = 0 !! Integer number of n-tuples (array size) 
    Real(WP)              :: ZT = 0.0_WP !! Common Z displacement for IP=1
    Real(WP), Allocatable :: X(:)   !! N X values/abscissas (IP=1,2,3)
    Real(WP), Allocatable :: Y(:)   !! N Y values/ordinates (IP=1,2,3)
    Real(WP), Allocatable :: Z(:)   !! N Z values (IP = 2,3)
    Real(WP), Allocatable :: IR(:)  !! N I vector values (IP = 3) 
    Real(WP), Allocatable :: JR(:)  !! N J vector values (IP = 3)
    Real(WP), Allocatable :: KR(:)  !! N K vector values (IP = 3)
 
  Contains

    Procedure :: getEntity    => getE106 
    !! Translate IGES file records into an entity106_t entity type 
    Procedure :: makeEntity   => makeE106 
    !! Translate entity106_t entity type into IGES P records 
    Procedure :: outputEntity => outputE106
    !! Output the components of an entity106_t entity type 
    Procedure :: copyTo       => copyToE106
    !! Copy this entity106_t type to an output entity106_t 
    Procedure :: initType     => initTypeE106
    !! Copy an input entity106_t type to this entity106_t 
    Procedure :: dealloc      => deallocE106
    !! Deallocate an entity106_t entity
    Procedure :: init         => initE106
    !! Initialize an entity106_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity108_t
  !! Plane entity 
 
    Real(WP) :: A    = 0.0_WP !! Coefficients of Plane
    Real(WP) :: B    = 0.0_WP !! Coefficients of Plane
    Real(WP) :: C    = 0.0_WP !! Coefficients of Plane
    Real(WP) :: D    = 0.0_WP !! Coefficients of plane
    Integer  :: PTR  = 0      !! Zero for form 0
                              !! Index in DE of the closed curve entity
    Real(WP) :: X    = 0.0_WP !! XT coordinate of a display symbol location
    Real(WP) :: Y    = 0.0_WP !! YT coordinate of a display symbol location
    Real(WP) :: Z    = 0.0_WP !! ZT coordinate of a display symbol location
    Real(WP) :: SIZE = 0.0_WP !! Size parameter for a display symbol
                              !! XT, YT, ZT, and SIZE are only valid for
                              !! forms 1 and -1
 
  Contains

    Procedure :: getEntity    => getE108 
    !! Translate IGES file records into an entity108_t entity type 
    Procedure :: makeEntity   => makeE108 
    !! Translate entity108_t entity type into IGES P records 
    Procedure :: outputEntity => outputE108
    !! Output the components of an entity108_t entity type 
    Procedure :: copyTo       => copyToE108
    !! Copy this entity108_t type to an output entity108_t 
    Procedure :: initType     => initTypeE108
    !! Copy an input entity108_t type to this entity108_t 
    Procedure :: dealloc      => deallocE108
    !! Deallocate an entity108_t entity
    Procedure :: init         => initE108
    !! Initialize an entity108_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity110_t
  !! Line entity 
 
    Real(WP) :: X1 = 0.0_WP !! Start/arbitrary point P1.x
    Real(WP) :: Y1 = 0.0_WP !! Start/arbitrary point P1.y
    Real(WP) :: Z1 = 0.0_WP !! Start/arbitrary point P1.z
    Real(WP) :: X2 = 0.0_WP !! Terminate/arbitrary point P2.x
    Real(WP) :: Y2 = 0.0_WP !! Terminate/arbitrary point P2.y
    Real(WP) :: Z2 = 0.0_WP !! Terminate/arbitrary Point P2.z

  Contains

    Procedure :: getEntity    => getE110 
    !! Translate IGES file records into an entity110_t entity type 
    Procedure :: makeEntity   => makeE110 
    !! Translate entity110_t entity type into IGES P records 
    Procedure :: outputEntity => outputE110
    !! Output the components of an entity110_t entity type 
    Procedure :: copyTo       => copyToE110
    !! Copy this entity110_t type to an output entity110_t 
    Procedure :: initType     => initTypeE110
    !! Copy an input entity110_t type to this entity110_t 
    Procedure :: dealloc      => deallocE110
    !! Deallocate an entity110_t entity
    Procedure :: init         => initE110
    !! Initialize an entity110_t entity with user data

  End Type

 
  Type, Extends(entity_t) :: entity112_t
  !! Parametric spline curve entity
 
    Integer               :: CTYPE = 0 !! Spline type
                                       !! 1 = Linear
                                       !! 2 = Quadratic
                                       !! 3 = Cubic
                                       !! 4 = Wilson-Fowler
                                       !! 5 = Modified Wilson-Fowler
                                       !! 6 = B-spline
    Integer               :: H     = 0 !! Degree of continuity wrt arc length
    Integer               :: NDIM  = 0 !! Number of dimensions
                                       !! 2 = planar
                                       !! 3 = non-planar
    Integer               :: N     = 0 !! Number of curve segments
    Real(WP), Allocatable :: T(:)      !! N+1 curve segment breakpoints
    Real(WP), Allocatable :: Coefs(:,:) !! 12xN array of polynomial coefficients
    Real(WP)              :: TPX0  = 0.0_WP !! X value at end of Nth segment
    Real(WP)              :: TPX1  = 0.0_WP !! X first derivative  
    Real(WP)              :: TPX2  = 0.0_WP !! X second derivative/2!
    Real(WP)              :: TPX3  = 0.0_WP !! X third derivative/3!
    Real(WP)              :: TPY0  = 0.0_WP !! Y value at end of Nth segment
    Real(WP)              :: TPY1  = 0.0_WP !! Y first derivative
    Real(WP)              :: TPY2  = 0.0_WP !! Y second derivative/2!
    Real(WP)              :: TPY3  = 0.0_WP !! Y third derivative/3!
    Real(WP)              :: TPZ0  = 0.0_WP !! Z value at end of Nth segment
    Real(WP)              :: TPZ1  = 0.0_WP !! Z first derivative
    Real(WP)              :: TPZ2  = 0.0_WP !! Z second derivative/2!
    Real(WP)              :: TPZ3  = 0.0_WP !! Z third derivative/3!

  Contains

    Procedure :: getEntity    => getE112 
    !! Translate IGES file records into an entity112_t entity type 
    Procedure :: makeEntity   => makeE112 
    !! Translate entity112_t entity type into IGES P records 
    Procedure :: outputEntity => outputE112
    !! Output the components of an entity112_t entity type 
    Procedure :: copyTo       => copyToE112
    !! Copy this entity112_t type to an output entity112_t 
    Procedure :: initType     => initTypeE112
    !! Copy an input entity112_t type to this entity112_t 
    Procedure :: dealloc      => deallocE112
    !! Deallocate an entity112_t entity
    Procedure :: init         => initE112
    !! Initialize an entity112_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity114_t
  !! Parametric spline surface entity 
 
    Integer               :: CTYPE = 0 !! Spline Boundary Type
                                       !! 1 = Linear
                                       !! 2 = Quadratic
                                       !! 3 = Cubic
                                       !! 4 = Wilson-Fowler
                                       !! 5 = Modified Wilson-Fowler
                                       !! 6 = B-spline 
    Integer               :: PTYPE = 0 !! Patch type
                                       !! 1 = Cartesian Product
                                       !! 2 = Unspecifie
    Integer               :: M     = 0 !! Number of u segments
    Integer               :: N     = 0 !! Number of v segments
    Real(WP), Allocatable :: TU(:)     !! M+1 U breakpoints
    Real(WP), Allocatable :: TV(:)     !! N+1 V breakpoints
    Real(WP), Allocatable :: Coefs(:,:,:) !! 48xMXN array of patch coefficients
                                          !! There are 16 coefficients for
                                          !! each coordinate direction
  Contains

    Procedure :: getEntity    => getE114 
    !! Translate IGES file records into an entity114_t entity type 
    Procedure :: makeEntity   => makeE114 
    !! Translate entity114_t entity type into IGES P records 
    Procedure :: outputEntity => outputE114
    !! Output the components of an entity114_t entity type 
    Procedure :: copyTo       => copyToE114
    !! Copy this entity114_t type to an output entity114_t 
    Procedure :: initType     => initTypeE114
    !! Copy an input entity114_t type to this entity114_t 
    Procedure :: dealloc      => deallocE114
    !! Deallocate an entity114_t entity
    Procedure :: init         => initE114
    !! Initialize an entity114_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity116_t
  !! Point entity 
 
    Real(WP) :: X   = 0.0_WP !! X coordinate of point
    Real(WP) :: Y   = 0.0_WP !! Y coordinate or point
    Real(WP) :: Z   = 0.0_WP !! Z coordinate of point
    Integer  :: PTR = 0 !! DE index of a display figure Subfigure Definition 
                        !! Entity or zero

  Contains

    Procedure :: getEntity    => getE116 
    !! Translate IGES file records into an entity116_t entity type 
    Procedure :: makeEntity   => makeE116 
    !! Translate entity116_t entity type into IGES P records 
    Procedure :: outputEntity => outputE116
    !! Output the components of an entity116_t entity type 
    Procedure :: copyTo       => copyToE116
    !! Copy this entity116_t type to an output entity116_t 
    Procedure :: initType     => initTypeE116
    !! Copy an input entity116_t type to this entity116_t 
    Procedure :: dealloc      => deallocE116
    !! Deallocate an entity116_t entity
    Procedure :: init         => initE116
    !! Initialize an entity116_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity118_t
  !! Ruled surface entity 
 
    Integer :: DE1     = 0 !! DE index of the first curve entity
    Integer :: DE2     = 0 !! DE index of the second curve entity
    Integer :: DIRFLAG = 0 !! Dirction flag
                           !! 0 = join first to first, last to last 
                           !! 1 = join first to last, last to first
    Integer :: DEVFLG  = 0 !! Developable surface flag
                           !! 1 = Developable
                           !! 0 = Maybe not
  Contains

    Procedure :: getEntity    => getE118 
    !! Translate IGES file records into an entity118_t entity type 
    Procedure :: makeEntity   => makeE118 
    !! Translate entity118_t entity type into IGES P records 
    Procedure :: outputEntity => outputE118
    !! Output the components of an entity118_t entity type 
    Procedure :: copyTo       => copyToE118
    !! Copy this entity118_t type to an output entity118_t 
    Procedure :: initType     => initTypeE118
    !! Copy an input entity118_t type to this entity118_t 
    Procedure :: dealloc      => deallocE118
    !! Deallocate an entity118_t entity
    Procedure :: init         => initE118
    !! Initialize an entity118_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity120_t
  !! Surface of revolution entity 
 
    Integer  :: L  = 0 !! DE index of the axis of revolution Line entity
    Integer  :: C  = 0 !! DE index of the generatrix entity
    Real(WP) :: SA = 0.0_WP !! Start angle in radians
    Real(WP) :: TA = 0.0_WP !! Terminate angle in radians

  Contains

    Procedure :: getEntity    => getE120 
    !! Translate IGES file records into an entity120_t entity type 
    Procedure :: makeEntity   => makeE120 
    !! Translate entity120_t entity type into IGES P records 
    Procedure :: outputEntity => outputE120
    !! Output the components of an entity120_t entity type 
    Procedure :: copyTo       => copyToE120
    !! Copy this entity120_t type to an output entity120_t 
    Procedure :: initType     => initTypeE120
    !! Copy an input entity120_t type to this entity120_t 
    Procedure :: dealloc      => deallocE120
    !! Deallocate an entity120_t entity
    Procedure :: init         => initE120
    !! Initialize an entity120_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity122_t
  !! Tabulated cylinder entity 
 
    Integer  :: DE = 0 !! DE index of a directrix curve entity
    Real(WP) :: LX = 0.0_WP !! X coordinate of generatrix terminate point
    Real(WP) :: LY = 0.0_WP !! Y coordinate of generatrix terminate point
    Real(WP) :: LZ = 0.0_WP !! Z coordinate of generatrix terminate point

  Contains

    Procedure :: getEntity    => getE122 
    !! Translate IGES file records into an entity122_t entity type 
    Procedure :: makeEntity   => makeE122 
    !! Translate entity122_t entity type into IGES P records 
    Procedure :: outputEntity => outputE122
    !! Output the components of this entity122_t entity type
    Procedure :: copyTo       => copyToE122
    !! Copy this entity122_t type to an output entity122_t 
    Procedure :: initType     => initTypeE122
    !! Copy an input entity122_t type to this entity122_t 
    Procedure :: dealloc      => deallocE122
    !! Deallocate an entity122_t entity
    Procedure :: init         => initE122
    !! Initialize an entity122_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity123_t
  !! Direction entity 
    Real(WP) :: X = 0.0_WP !! Direction ratio wrt X axis
    Real(WP) :: Y = 0.0_WP !! Direction ratio wrt Y axis 
    Real(WP) :: Z = 0.0_WP !! Direction ratio wrt Z axis

  Contains

    Procedure :: getEntity    => getE123 
    !! Translate IGES file records into an entity123_t entity type 
    Procedure :: makeEntity   => makeE123 
    !! Translate entity123_t entity type into IGES P records 
    Procedure :: outputEntity => outputE123
    !! Output the components of this entity123_t entity type
    Procedure :: copyTo       => copyToE123
    !! Copy this entity123_t type to an output entity123_t 
    Procedure :: initType     => initTypeE123
    !! Copy an input entity123_t type to this entity123_t 
    Procedure :: dealloc      => deallocE123
    !! Deallocate an entity123_t entity
    Procedure :: init         => initE123
    !! Initialize an entity123_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity124_t
  !! Transformation matrix entity 

    Integer  :: tmat_owner = 0 !! DE index of entity that uses this T mat
    Real(WP) :: R(3,3)     = 0.0_WP !! Rotation matrix
    Real(WP) :: T(3)       = 0.0_WP !! Translation Vector

  Contains

    Procedure :: getEntity    => getE124 
    !! Translate IGES file records into an entity124_t entity type 
    Procedure :: makeEntity   => makeE124 
    !! Translate entity124_t entity type into IGES P records 
    Procedure :: outputEntity => outputE124
    !! Output the components of this entity124_t entity type
    Procedure :: copyTo       => copyToE124
    !! Copy this entity124_t type to an output entity124_t 
    Procedure :: initType     => initTypeE124
    !! Copy an input entity124_t type to this entity124_t 
    Procedure :: dealloc      => deallocE124
    !! Deallocate an entity124_t entity
    Procedure :: init         => initE124
    !! Initialize an entity124_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity126_t
  !! Rational B-spline curve entity 
 
    Integer               :: K     = 0 !! Upper index of sum
    Integer               :: M     = 0 !! Degree of basis funcions
    Integer               :: PROP1 = 0 !! 0=nonplanar, 1=planar
    Integer               :: PROP2 = 0 !! 0=open curve, 1=closed curve
    Integer               :: PROP3 = 0 !! 0=rational, 1=polynomial (b-spline)
    Integer               :: PROP4 = 0 !! 0=nonperiodic, 1= periodic
    Real(WP), Allocatable :: T(:) !! Knot sequence (bounds = -M:K+1) 
    Real(WP), Allocatable :: W(:) !! Weights (bounds = 0:K) 
    Real(WP), Allocatable :: X(:) !! X control points (bounds=0:K) 
    Real(WP), Allocatable :: Y(:) !! Y control points (bounds=0:K)
    Real(WP), Allocatable :: Z(:) !! Z control points (bounds=0:K)
    Real(WP)              :: V(0:1) = 0.0_WP !! Starting/ending parameter values
    Real(WP)              :: XNORM  = 0.0_WP !! Planar curve unit normal X
    Real(WP)              :: YNORM  = 0.0_WP !! Planar curve unit normal Y
    Real(WP)              :: ZNORM  = 0.0_WP !! Planar curve unit normal Z

  Contains

    Procedure :: getEntity    => getE126 
    !! Translate IGES file records into an entity126_t entity type 
    Procedure :: makeEntity   => makeE126 
    !! Translate entity126_t entity type into IGES P records 
    Procedure :: outputEntity => outputE126
    !! Output the components of this entity126_t entity type
    Procedure :: copyTo       => copyToE126
    !! Copy this entity126_t type to an output entity126_t 
    Procedure :: initType     => initTypeE126
    !! Copy an input entity126_t type to this entity126_t 
    Procedure :: dealloc      => deallocE126
    !! Deallocate an entity126_t entity
    Procedure :: init         => initE126
    !! Initialize an entity126_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity128_t
  !! Rational B-spline surface entity 
 
    Integer               :: K1    = 0 !! Upper index of first (U) sum
    Integer               :: K2    = 0 !! Upper index of second (V) sum
    Integer               :: M1    = 0 !! Degree of U direction basis functions
    Integer               :: M2    = 0 !! Degree of V direction basis functions
    Integer               :: PROP1 = 0 !! 1=closed in U direction, 0=not closed 
    Integer               :: PROP2 = 0 !! 1=closed in V direction, 0=not closed
    Integer               :: PROP3 = 0 !! 0=rational, 1=polynomial (B-spline)
    Integer               :: PROP4 = 0 !! 0=non-periodic in U direction
                                       !! 1=periodic in U direction
    Integer               :: PROP5 = 0 !! 0=non-periodic in V direction
                                       !! 1=periodic in V direction
    Real(WP), Allocatable :: S(:) !! U knot values (bounds: -M:K1+1)) 
    Real(WP), Allocatable :: T(:) !! V knot values (bounds: -N:K2+1))  
    Real(WP), Allocatable :: W(:,:) !! Weight array (bounds: (0:K1,0:K2)) 
    Real(WP), Allocatable :: X(:,:) !! X control points (bounds: (0:K1,0:K2))
    Real(WP), Allocatable :: Y(:,:) !! Y control points (bounds: (0:K1,0:K2))
    Real(WP), Allocatable :: Z(:,:) !! Z control points (bounds: (0:K1,0:k2))
    Real(WP)              :: U(0:1) = 0.0_WP !! Starting/ending U params
    Real(WP)              :: V(0:1) = 0.0_WP !! Starting/ending V params

  Contains

    Procedure :: getEntity    => getE128 
    !! Translate IGES file records into an entity128_t entity type 
    Procedure :: makeEntity   => makeE128 
    !! Translate entity128_t entity type into IGES P records 
    Procedure :: outputEntity => outputE128
    !! Output the components of this entity128_t entity type
    Procedure :: copyTo       => copyToE128
    !! Copy this entity128_t type to an output entity128_t 
    Procedure :: initType     => initTypeE128
    !! Copy an input entity128_t type to this entity128_t 
    Procedure :: dealloc      => deallocE128
    !! Deallocate an entity128_t entity
    Procedure :: init         => initE128
    !! Initialize an entity128_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity130_t
  !! Offset curve entity 
 
    Integer  :: DE1   = 0 !! DE index of curve entity to be offset
    Integer  :: FLAG  = 0 !! Offset distance flag
                          !! 1=Single value offset, uniform distance
                          !! 2=Offset distance varies linearly
                          !! 3=Offset distance as a specified function
    Integer  :: DE2   = 0 !! DE index of a curve entity where one coordinate
                          !! describes the offset as a function of its
                          !! parameter. 0 unless FLAG=3
    Integer  :: NDIM  = 0 !! DE index of a coordinate of DE2 which describes
                          !! offset as a function of its parameter (FLAG=3) 
    Integer  :: PTYPE = 0 !! Tapered offset flag
                          !! 1=Function of arc length
                          !! 2=Function of parameter (FLAG=2 or 3 only)
    Real(WP) :: D1    = 0.0_WP !! First offset distance (FLAG=1 or 2 only)
    Real(WP) :: TD1   = 0.0_WP !! Arc length or parameter value of first 
                               !! offset distance 
    Real(WP) :: D2    = 0.0_WP !! Second offset distance
    Real(WP) :: TD2   = 0.0_WP !! Arc length or parameter of second offset
                               !! distance (FLAG = 2 only) 
    Real(WP) :: VX    = 0.0_WP !! X component of unit vector normal to plane
                               !! containing offset curve 
    Real(WP) :: VY    = 0.0_WP !! Y component of unit vector normal to plane
                               !! containing offset curve 
    Real(WP) :: VZ    = 0.0_WP !! Z component of unit vector normal to plane
                               !! containing offset curve 
    Real(WP) :: TT1   = 0.0_WP !! Offset curve starting parameter value 
    Real(WP) :: TT2   = 0.0_WP !! Offset curve ending parameter value

  Contains

    Procedure :: getEntity    => getE130 
    !! Translate IGES file records into an entity130_t entity type 
    Procedure :: makeEntity   => makeE130 
    !! Translate entity130_t entity type into IGES P records 
    Procedure :: outputEntity => outputE130
    !! Output the components of this entity130_t entity type
    Procedure :: copyTo       => copyToE130
    !! Copy this entity130_t type to an output entity130_t 
    Procedure :: initType     => initTypeE130
    !! Copy an input entity130_t type to this entity130_t 
    Procedure :: dealloc      => deallocE130
    !! Deallocate an entity130_t entity
    Procedure :: init         => initE130
    !! Initialize an entity130_t entity with user data

  End Type
 
  Type, Extends(entity_t) :: entity140_t
  !! Offset surface entity 
 
    Real(WP) :: NX = 0.0_WP !! X coordinate of end of offset indicator
    Real(WP) :: NY = 0.0_WP !! Y coordinate of end of offset indicator
    Real(WP) :: NZ = 0.0_WP !! Z coordinate of end of offset indicator
    Real(WP) :: D  = 0.0_WP !! Distance to offset surface normal to the side
                            !! of the offset indicator
    Integer  :: DE = 0      !! DE index of surface entity to offset

  Contains

    Procedure :: getEntity    => getE140 
    !! Translate IGES file records into an entity140_t entity type 
    Procedure :: makeEntity   => makeE140 
    !! Translate entity140_t entity type into IGES P records 
    Procedure :: outputEntity => outputE140
    !! Output the components of this entity140_t entity type
    Procedure :: copyTo       => copyToE140
    !! Copy this entity140_t type to an output entity140_t 
    Procedure :: initType     => initTypeE140
    !! Copy an input entity140_t type to this entity140_t 
    Procedure :: dealloc      => deallocE140
    !! Deallocate an entity140_t entity
    Procedure :: init         => initE140
    !! Initialize an entity140_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity141_t
  !! Boundary entity 
 
    Integer                       :: TYPE = 0
    !! Boundary representation type
    !! 0=Boundary entities use model space trimming curves and associated
    !! surface may be parameteric
    !! 1=Boundary entities use model space curves and associated 
    !! parametric space curve collections
    Integer                       :: PREF = 0
    !! Preferred representation of triming curves
    !! 0=Unspecified
    !! 1=Model space
    !! 2=Parameter space
    !! 3=Model or parameter space 
    Integer                       :: SPTR = 0
    !! DE index of untrimmed surface entiry to be bounded
    Integer                       :: N    = 0
    !! Number of curves included in this boundary entity
    Integer,          Allocatable :: CRVPT(:)
    !! Array of DE index values for curves of this boundary entity
    Integer,          Allocatable :: SENSE(:)
    !! Orientation flag indicating if model space curves should be reversed
    Integer,          Allocatable :: K(:)
    !! Number of space curve in the collection of each trimming curve
    Type(intArray_t), Allocatable :: PSCPT(:)
    !! An array of N integer arrays containing DE index values for each
    !! Trimming curve 
  Contains

    Procedure :: getEntity    => getE141 
    !! Translate IGES file records into an entity141_t entity type 
    Procedure :: makeEntity   => makeE141 
    !! Translate entity141_t entity type into IGES P records 
    Procedure :: outputEntity => outputE141
    !! Output the components of this entity141_t entity type
    Procedure :: copyTo       => copyToE141
    !! Copy this entity141_t type to an output entity141_t 
    Procedure :: initType     => initTypeE141
    !! Copy an input entity141_t type to this entity141_t 
    Procedure :: dealloc      => deallocE141
    !! Deallocate an entity141_t entity
    Procedure :: init         => initE141
    !! Initialize an entity141_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity142_t
  !! Curve on parametric surface entity
 
    Integer :: CRTN = 0 !! How a curve on a surface was created
                        !! 0=Unspecified
                        !! 1=Projection of a curve to surface
                        !! 2=Intersection of two surfaces
                        !! 3=Isoparametric curve
    Integer :: SPTR = 0 !! DE index of the surface on which the curve lies
    Integer :: BPTR = 0 !! DE index of the entity containing the B curve
    Integer :: CPTR = 0 !! DE index of C curve entity 
    Integer :: PREF = 0 !! Preferred curve representation
                        !! 0=Unspecified
                        !! B on S is preferred 
                        !! C is preferred
                        !! C and B on S preferred equally
  Contains

    Procedure :: getEntity    => getE142 
    !! Translate IGES file records into an entity142_t entity type 
    Procedure :: makeEntity   => makeE142 
    !! Translate entity142_t entity type into IGES P records 
    Procedure :: outputEntity => outputE142
    !! Output the components of this entity142_t entity type
    Procedure :: copyTo       => copyToE142
    !! Copy this entity142_t type to an output entity142_t 
    Procedure :: initType     => initTypeE142
    !! Copy an input entity142_t type to this entity142_t 
    Procedure :: dealloc      => deallocE142
    !! Deallocate an entity142_t entity
    Procedure :: init         => initE142
    !! Initialize an entity142_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity143_t
  !! Bounded surface entity
 
    Integer              :: TYPE = 0 !! Type of bounded surface
                                     !! 0=model space curves only
                                     !! 1=model space and assciated parameter
                                     !! space curves
    Integer              :: SPTR = 0 !! DE index of untrimmed surface to be
                                     !! bounded
    Integer              :: N    = 0 !! Number of boundary entities
    Integer, Allocatable :: BDPT(:)  !! Array of N DE indicies to a boundary
                                     !! entity (Type 141) 

  Contains

    Procedure :: getEntity    => getE143 
    !! Translate IGES file records into an entity143_t entity type 
    Procedure :: makeEntity   => makeE143 
    !! Translate entity143_t entity type into IGES P records 
    Procedure :: outputEntity => outputE143
    !! Output the components of this entity143_t entity type
    Procedure :: copyTo       => copyToE143
    !! Copy this entity143_t type to an output entity143_t 
    Procedure :: initType     => initTypeE143
    !! Copy an input entity143_t type to this entity143_t 
    Procedure :: dealloc      => deallocE143
    !! Deallocate an entity143_t entity
    Procedure :: init         => initE143
    !! Initialize an entity143_t entity with user data

  End Type


  Type, Extends(entity_t) :: entity144_t
  !! Trimmed parametric surface entity
 
    Integer              :: PTS   = 0 !! DE index of surface to be trimmed 
    Integer              :: N1    = 0 !! 0=outer boundary is boundary of D
                                      !! 1=otherwise
    Integer              :: N2    = 0 !! Number of simple curves that make up
                                      !! the inner boundary of the trimmed
                                      !! surface
    Integer              :: PTO   = 0 !! DE index of curve on parametric surface
                                      !! entity that constitutes the outer
                                      !! boundary of the trimmed surface
    Integer, Allocatable :: PTI(:)    !! Array of N2 DE indicies of a simple
                                      !! closed boundary curve entity 

  Contains

    Procedure :: getEntity    => getE144 
    !! Translate IGES file records into an entity144_t entity type 
    Procedure :: makeEntity   => makeE144 
    !! Translate entity144_t entity type into IGES P records 
    Procedure :: outputEntity => outputE144
    !! Output the components of this entity144_t entity type
    Procedure :: copyTo       => copyToE144
    !! Copy this entity144_t type to an output entity144_t 
    Procedure :: initType     => initTypeE144
    !! Copy an input entity144_t type to this entity144_t 
    Procedure :: dealloc      => deallocE144
    !! Deallocate an entity144_t entity
    Procedure :: init         => initE144
    !! Initialize an entity144_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity190_t
  !! Plane surface entity 
 
    Integer :: DELOC  = 0 !! DE index of a point on surface
    Integer :: DENRML = 0 !! DE index of a surface normal direction
    Integer :: DEREFD = 0 !! DE index to a reference direction (Form 1 only)

  Contains

    Procedure :: getEntity    => getE190 
    !! Translate IGES file records into an entity190_t entity type 
    Procedure :: makeEntity   => makeE190 
    !! Translate entity190_t entity type into IGES P records 
    Procedure :: outputEntity => outputE190
    !! Output the components of this entity190_t entity type
    Procedure :: copyTo       => copyToE190
    !! Copy this entity190_t type to an output entity190_t 
    Procedure :: initType     => initTypeE190
    !! Copy an input entity190_t type to this entity190_t 
    Procedure :: dealloc      => deallocE190
    !! Deallocate an entity190_t entity
    Procedure :: init         => initE190
    !! Initialize an entity190_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity192_t
  !! Right circular cylinder surface entity
 
    Integer  :: DELOC  = 0 !! DE index of a point on axis
    Integer  :: DEAXIS = 0 !! DE index of an axis direction
    Real(WP) :: RADIUS = 0.0_WP !! Radius of cylinder
    Integer  :: DEREFD = 0 !! DE index of a reference direction (Form 1 only)

  Contains

    Procedure :: getEntity    => getE192 
    !! Translate IGES file records into an entity192_t entity type 
    Procedure :: makeEntity   => makeE192 
    !! Translate entity192_t entity type into IGES P records 
    Procedure :: outputEntity => outputE192
    !! Output the components of this entity192_t entity type
    Procedure :: copyTo       => copyToE192
    !! Copy this entity192_t type to an output entity192_t 
    Procedure :: initType     => initTypeE192
    !! Copy an input entity192_t type to this entity192_t 
    Procedure :: dealloc      => deallocE192
    !! Deallocate an entity192_t entity
    Procedure :: init         => initE192
    !! Initialize an entity192_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity194_t
  !! Right circular conical surface entity
 
    Integer  :: DELOC  = 0 !! DE index of a point on axis
    Integer  :: DEAXIS = 0 !! DE index to an axis direction
    Real(WP) :: RADIUS = 0.0_WP !! Radius at axis point
    Real(WP) :: SANGLE = 0.0_WP !! Semi-angle in degrees (0.0 to 90.0)
    Integer  :: DEREFD = 0 !! DE index of a reference direction (Form 1 only)
   
  Contains

    Procedure :: getEntity    => getE194 
    !! Translate IGES file records into an entity194_t entity type 
    Procedure :: makeEntity   => makeE194 
    !! Translate entity194_t entity type into IGES P records 
    Procedure :: outputEntity => outputE194
    !! Output the components of this entity194_t entity type
    Procedure :: copyTo       => copyToE194
    !! Copy this entity194_t type to an output entity194_t 
    Procedure :: initType     => initTypeE194
    !! Copy an input entity194_t type to this entity194_t 
    Procedure :: dealloc      => deallocE194
    !! Deallocate an entity194_t entity
    Procedure :: init         => initE194
    !! Initialize an entity194_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity196_t
  !! Spherical surface entity
 
    Integer  :: DELOC  = 0 !! DE index of a center point 
    Real(WP) :: RADIUS = 0.0_WP !! Radius
    Integer  :: DEAXIS = 0 !! DE index of axis direction (Form 1 only)
    Integer  :: DEREFD = 0 !! DE index of a reference direction (Form 1 only)
   
  Contains

    Procedure :: getEntity    => getE196 
    !! Translate IGES file records into an entity196_t entity type 
    Procedure :: makeEntity   => makeE196 
    !! Translate entity196_t entity type into IGES P records 
    Procedure :: outputEntity => outputE196
    !! Output the components of this entity196_t entity type
    Procedure :: copyTo       => copyToE196
    !! Copy this entity196_t type to an output entity196_t 
    Procedure :: initType     => initTypeE196
    !! Copy an input entity196_t type to this entity196_t 
    Procedure :: dealloc      => deallocE196
    !! Deallocate an entity196_t entity
    Procedure :: init         => initE196
    !! Initialize an entity196_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity198_t
  !! Toroidal surface entity
 
    Integer  :: DELOC  = 0 !! DE index of a center point
    Integer  :: DEAXIS = 0 !! DE index of an axis direction
    Real(WP) :: MAJRAD = 0.0_WP !! Major radius
    Real(WP) :: MINRAD = 0.0_WP !! Minor radius
    Integer  :: DEREFD = 0 !! DE index of a reference direction (Form 1 only)
 
  Contains

    Procedure :: getEntity    => getE198 
    !! Translate IGES file records into an entity198_t entity type 
    Procedure :: makeEntity   => makeE198 
    !! Translate entity198_t entity type into IGES P records 
    Procedure :: outputEntity => outputE198
    !! Output the components of this entity198_t entity type
    Procedure :: copyTo       => copyToE198
    !! Copy this entity198_t type to an output entity198_t 
    Procedure :: initType     => initTypeE198
    !! Copy an input entity198_t type to this entity198_t 
    Procedure :: dealloc      => deallocE198
    !! Deallocate an entity198_t entity
    Procedure :: init         => initE198
    !! Initialize an entity198_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity212_t
  !! General note entity
 
    Integer                     :: NS = 0 !! Number of text strings in note
    Integer,        Allocatable :: NC(:)  !! Number of characters in each string
    Real(WP),       Allocatable :: WT(:)  !! Box widths of each string
    Real(WP),       Allocatable :: HT(:)  !! Box height of each string
    Integer,        Allocatable :: FC(:)  !! Font code or DE index
                                          !! of a font entity (not-supported)
    Real(WP),       Allocatable :: SL(:)  !! Slant angles of text
    Real(WP),       Allocatable :: A(:)   !! Rotation angles in radians
    Integer,        Allocatable :: M(:)   !! Mirror flag
                                          !! 0=no mirroring
                                          !! 1=mirror axis perpendicular to text
                                          !! 2=mirror axis in text direction
    Integer,        Allocatable :: VH(:)  !! Rotate internal text flag
                                          !! 0=text horizontal
                                          !! 1=text vertical
    Real(WP),       Allocatable :: XS(:)  !! Text starting X point
    Real(WP),       Allocatable :: YS(:)  !! Text starting Y point
    Real(WP),       Allocatable :: ZS(:)  !! Text Z depth in X,Y plane
    Type(string_t), Allocatable :: TEXT(:) !! Array of NS text strings
 
  Contains

    Procedure :: getEntity    => getE212 
    !! Translate IGES file records into an entity212_t entity type 
    Procedure :: makeEntity   => makeE212 
    !! Translate entity212_t entity type into IGES P records 
    Procedure :: outputEntity => outputE212
    !! Output the components of this entity212_t entity type
    Procedure :: copyTo       => copyToE212
    !! Copy this entity212_t type to an output entity212_t 
    Procedure :: initType     => initTypeE212
    !! Copy an input entity212_t type to this entity212_t 
    Procedure :: dealloc      => deallocE212
    !! Deallocate an entity212_t entity
    Procedure :: init         => initE212
    !! Initialize an entity212_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity308_t
  !! Subfigure definition entity

    Integer                   :: DEPTH = 0 !! Depth of subfigure
    Character(:), Allocatable :: NAME !! Subfigure name
    Integer                   :: N     = 0 !! Number of subfigure entities
    Integer,      Allocatable :: DE(:) !! Array of N DE indicies for subfigure
                                       !! entities

  Contains

    Procedure :: getEntity    => getE308 
    !! Translate IGES file records into an entity308_t entity type 
    Procedure :: makeEntity   => makeE308 
    !! Translate entity308_t entity type into IGES P records 
    Procedure :: outputEntity => outputE308
    !! Output the components of this entity308_t entity type
    Procedure :: copyTo       => copyToE308
    !! Copy this entity308_t type to an output entity308_t 
    Procedure :: initType     => initTypeE308
    !! Copy an input entity308_t type to this entity308_t 
    Procedure :: dealloc      => deallocE308
    !! Deallocate an entity308_t entity
    Procedure :: init         => initE308
    !! Initialize an entity308_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity314_t
  !! Color definition entity 
 
    Real(WP)                  :: CC1 = 0.0_WP !! Red color coordinate
    Real(WP)                  :: CC2 = 0.0_WP !! Green color coordinate
    Real(WP)                  :: CC3 = 0.0_WP !! Blue color coordinate
    Character(:), Allocatable :: CNAME !! Optional color name

  Contains

    Procedure :: getEntity    => getE314 
    !! Translate IGES file records into an entity314_t entity type 
    Procedure :: makeEntity   => makeE314 
    !! Translate entity314_t entity type into IGES P records 
    Procedure :: outputEntity => outputE314
    !! Output the components of this entity314_t entity type
    Procedure :: copyTo       => copyToE314
    !! Copy this entity314_t type to an output entity314_t 
    Procedure :: initType     => initTypeE314
    !! Copy an input entity314_t type to this entity314_t 
    Procedure :: dealloc      => deallocE314
    !! Deallocate an entity314_t entity
    Procedure :: init         => initE314
    !! Initialize an entity314_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity402_t
  !! Group associativity instance (forms 1, 7, 14, 15)
 
    Integer              :: N = 0 !! number of associated entities in group
    Integer, Allocatable :: DE(:) !! Array of N DE indicies of associated entities

  Contains

    Procedure :: getEntity    => getE402 
    !! Translate IGES file records into an entity402_t entity type 
    Procedure :: makeEntity   => makeE402 
    !! Translate entity402_t entity type into IGES P records 
    Procedure :: outputEntity => outputE402
    !! Output the components of this entity402_t entity type
    Procedure :: copyTo       => copyToE402
    !! Copy this entity402_t type to an output entity402_t 
    Procedure :: initType     => initTypeE402
    !! Copy an input entity402_t type to this entity402_t 
    Procedure :: dealloc      => deallocE402
    !! Deallocate an entity402_t entity
    Procedure :: init         => initE402
    !! Initialize an entity402_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity406_t
  !! Property entity (forms 1 (levels) and 15 (name) only
 
    Integer                       :: NP = 1 !! Number of levels for Form 1
    Integer,          Allocatable :: L(:)   !! Array of NP level numbers Form 1
    Character(LEN=:), Allocatable :: NAME   !! Name property (Form 15 only)

  Contains

    Procedure :: getEntity    => getE406 
    !! Translate IGES file records into an entity406_t entity type 
    Procedure :: makeEntity   => makeE406 
    !! Translate entity406_t entity type into IGES P records 
    Procedure :: outputEntity => outputE406
    !! Output the components of this entity406_t entity type
    Procedure :: copyTo       => copyToE406
    !! Copy this entity406_t type to an output entity406_t 
    Procedure :: initType     => initTypeE406
    !! Copy an input entity406_t type to this entity406_t 
    Procedure :: dealloc      => deallocE406
    !! Deallocate an entity406_t entity
    Procedure :: init         => initE406
    !! Initialize an entity406_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity408_t
  !! Singular subfigure instance entity
 
    Integer  ::  DE = 0 !! DE index of Subfigure Definition Entity
    Real(WP) ::  X  = 0.0_WP !! X translation value
    Real(WP) ::  Y  = 0.0_WP !! Y translation value
    Real(WP) ::  Z  = 0.0_WP !! Z translation value
    Real(WP) ::  S  = 1.0_WP !! Scale factor

  Contains

    Procedure :: getEntity    => getE408 
    !! Translate IGES file records into an entity408_t entity type 
    Procedure :: makeEntity   => makeE408 
    !! Translate entity408_t entity type into IGES P records 
    Procedure :: outputEntity => outputE408
    !! Output the components of this entity408_t entity type
    Procedure :: copyTo       => copyToE408
    !! Copy this entity408_t type to an output entity408_t 
    Procedure :: initType     => initTypeE408
    !! Copy an input entity408_t type to this entity408_t 
    Procedure :: dealloc      => deallocE408
    !! Deallocate an entity408_t entity
    Procedure :: init         => initE408
    !! Initialize an entity408_t entity with user data

  End Type


  Type, Extends(entity_t) :: entity502_t
  !! Vertex entity 
    Integer               :: N = 0 !! Number of vertex values
    Real(WP), Allocatable :: X(:)  !! Array of N vertex X coordinates
    Real(WP), Allocatable :: Y(:)  !! Array of N vertex Y coordinates
    Real(WP), Allocatable :: Z(:)  !! Array of N vertex Z coordinates
    
  Contains

    Procedure :: getEntity    => getE502 
    !! Translate IGES file records into an entity502_t entity type 
    Procedure :: makeEntity   => makeE502 
    !! Translate entity502_t entity type into IGES P records 
    Procedure :: outputEntity => outputE502
    !! Output the components of this entity502_t entity type
    Procedure :: copyTo       => copyToE502
    !! Copy this entity502_t type to an output entity502_t 
    Procedure :: initType     => initTypeE502
    !! Copy an input entity502_t type to this entity502_t 
    Procedure :: dealloc      => deallocE502
    !! Deallocate an entity502_t entity
    Procedure :: init         => initE502
    !! Initialize an entity502_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity504_t
  !! Edge entity

    Integer              :: N = 0 !! Number of edges
    Integer, Allocatable :: CURV(:) !! Array of N DE indices of start values
                                    !! of space curves
    Integer, Allocatable :: SVP(:)  !! Array of N DE indicies of Vertex Lists 
    Integer, Allocatable :: SV(:)   !! Array of N vertex start values 
    Integer, Allocatable :: TVP(:)  !! Array of N DE indicies of end values of
                                    !! space curves 
    Integer, Allocatable :: TV(:)   !! Array of list indicies of terminate
                                    !! verticies
  
  Contains

    Procedure :: getEntity    => getE504 
    !! Translate IGES file records into an entity504_t entity type 
    Procedure :: makeEntity   => makeE504 
    !! Translate entity504_t entity type into IGES P records 
    Procedure :: outputEntity => outputE504
    !! Output the components of this entity504_t entity type
    Procedure :: copyTo       => copyToE504
    !! Copy this entity504_t type to an output entity504_t 
    Procedure :: initType     => initTypeE504
    !! Copy an input entity504_t type to this entity504_t 
    Procedure :: dealloc      => deallocE504
    !! Deallocate an entity504_t entity
    Procedure :: init         => initE504
    !! Initialize an entity504_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity508_t
  !! Loop entity

    Integer                       :: N = 0 !! Number of edge values
    Integer,          Allocatable :: TYPE(:) !! Type of first edge
                                             !! 0=Edge
                                             !! 1=Vertex
    Integer,          Allocatable :: EDGE(:) !! Array of N DE indicies of a
                                             !! Vertex or Edge list 
    Integer,          Allocatable :: NDX(:)  !! list indicies in Vertex or
                                             !! Edge lists
    Logical,          Allocatable :: OF(:)   !! Orientation wrt space curve
                                             !! directions (TRUE = agrees) 
    Integer,          Allocatable :: K(:)    !! Number of space curves for each
                                             !! Edge or Vertex

    Type(intArray_t), Allocatable :: ISOP(:) !! Array of Isoparametric flags
    Type(intArray_t), Allocatable :: CURV(:) !! Array of DE indicies for space
                                             !! curves in each edge
 
  Contains

    Procedure :: getEntity    => getE508 
    !! Translate IGES file records into an entity508_t entity type 
    Procedure :: makeEntity   => makeE508 
    !! Translate entity508_t entity type into IGES P records 
    Procedure :: outputEntity => outputE508
    !! Output the components of this entity508_t entity type
    Procedure :: copyTo       => copyToE508
    !! Copy this entity508_t type to an output entity508_t 
    Procedure :: initType     => initTypeE508
    !! Copy an input entity508_t type to this entity508_t 
    Procedure :: dealloc      => deallocE508
    !! Deallocate an entity508_t entity
    Procedure :: init         => initE508
    !! Initialize an entity508_t entity with user data

  End Type

  Type, Extends(entity_t) :: entity510_t
  !! Face entity
    Integer               :: SURF = 0 !! DE index of underlying surface
    Integer               :: N    = 0 !! Number of loops
    Logical               :: OF   = .FALSE. !! Outer loop flag
    Integer, Allocatable  :: LOOP(:)        !! N DE indicies defining loops of
                                            !! a face
     
  Contains

    Procedure :: getEntity    => getE510
    !! Translate IGES file records into an entity510_t entity type 
    Procedure :: makeEntity   => makeE510 
    !! Translate entity510_t entity type into IGES P records 
    Procedure :: outputEntity => outputE510
    !! Output the components of this entity510_t entity type
    Procedure :: copyTo       => copyToE510
    !! Copy this entity510_t type to an output entity510_t 
    Procedure :: initType     => initTypeE510
    !! Copy an input entity510_t type to this entity510_t 
    Procedure :: dealloc      => deallocE510
    !! Deallocate an entity510_t entity
    Procedure :: init         => initE510
    !! Initialize an entity510_t entity with user data

  End Type

Contains

! some utilities needed to parse and construct P section entities

  Subroutine PentityToString(Precords, string, Dentry)
  !! Forms a long string of concatenated fields 1-65 of the given
  !! arry of Precords. Returns the string modified for Fortran list
  !! directed IO as well as the corresponding DE index for this entity

    Character(80),              Intent(IN)    :: Precords(:)
    Character(:),  Allocatable, Intent(INOUT) :: string
    Integer,                    Intent(INOUT) :: Dentry

    Integer :: i, nrecs, ls


    nrecs  = SIZE(Precords)
    string = TRIM(ADJUSTL(Precords(1)(1:65)))

!! Get array index in Directory for this Entity

    Read(Precords(1)(66:72),*) Dentry
    Dentry = (Dentry+1)/2

    If (nrecs > 1) Then
      Do i = 2, nrecs
        string = string // TRIM(ADJUSTL(Precords(i)(1:65)))
      End Do
    End If
  
!! Change terminator character to list directed input EOR (slash)

    ls = LEN(STRING)
    Do i = 1,ls
      If (string(i:i) == record_terminator) string(i:i) = "/"
    End Do  

!! Change field separator to comma

    If (field_separator /= ",") Then
      Do i = 1, ls
        If (string(i:i) == field_separator) string(i:i) = ","
      End Do
    End If

  End Subroutine PentityToString
       
  Subroutine getField(string, nfields, fs, fe)
  !! Determines the number of comma separated fields in a string and 
  !! returns arrays of the starting and ending parts of the field in
  !! the string
 
    Character(*),              Intent(INOUT) :: string
    Integer,                   Intent(INOUT) :: nfields
    Integer,      Allocatable, Intent(INOUT) :: fs(:)
    Integer,      Allocatable, Intent(INOUT) :: fe(:)

    Integer :: i, ls, ifield

! Change terminator character to list directed input EOR (slash)

    ls = LEN(STRING)
    Do i = 1,ls
      If (string(i:i) == record_terminator) string(i:i) = "/"
    End Do  

! Change field separator to comma

    If (field_separator /= ",") Then
      Do i = 1, ls
        If (string(i:i) == field_separator) string(i:i) = ","
      End Do
    End If

! get number of fields in this string
    
    nfields = 0 
    Do i = 1, ls
      If (string(i:i) == "," .OR. string(i:i) == "/") Then
        nfields = nfields+1
      End If
    End Do

    If (ALLOCATED(fs)) DEALLOCATE(fs)
    If (ALLOCATED(fe)) DEALLOCATE(fe)
    ALLOCATE(fs(nfields), SOURCE=0)
    ALLOCATE(fe(nfields), SOURCE=0)

! form fs and fe arrays

    fs(1) = 1
    ifield = 0
    Do i=1,ls
      If (string(i:i) == "," .OR. string(i:i) == "/") Then
        ifield = ifield+1
        fe(ifield) = i-1
      End If
    End Do
    Do i=2,nfields
      fs(i) = fe(i-1)+2
    End Do

  End Subroutine getField

  Subroutine filterHstrings(string)
!!  Searches for embedded commas in string fields and replaces them
!!  with an @. Ie  ,8H A, B, C, becomes ,8H A@ B@ C@,
!!  Primary use is in Entity 212 - General Notes

    Character(*), Intent(INOUT) :: string

    Character(:), Allocatable :: stemp

    Integer :: i, j, is, nfields, ih, ihs, lf, nc
    Integer, Allocatable :: fs(:), fe(:)
    
    stemp = string
    Call getField(stemp, nfields,fs,fe)

    Do i=1,nfields
      ih = INDEX(stemp(fs(i):fe(i)),"H")
      ihs = fs(i) + ih-1
      If (ih > 0) Then
        is = SCAN(stemp(ihs-1:ihs-1),"0123456789")
        If (is > 0) Then
          Read(stemp(fs(i):ihs-1), *) nc
          lf = LEN(stemp(fs(i):fe(i)))
          If (lf < nc) Then
            Do j=fs(i),fs(i)+nc-1
              If (stemp(j:j) == ",") stemp(j:j) = "@"
            End Do
          End If
        End If
      End If
    End Do

    string = stemp

  End Subroutine filterHstrings

  Subroutine genPrecords(recbuf, Precords)
  !! Given a recbuf array of type string_t, return a P records array

    Type(string_t),              Intent(IN)    :: recbuf(:)
    Character(80),  Allocatable, Intent(INOUT) :: Precords(:)

    Integer :: i, nlen, nrecs

    nrecs = SIZE(recbuf)

    ALLOCATE(Precords(nrecs))
    Do i=1,nrecs
      nlen = LEN(recbuf(i)%str)
      Precords(i) = REPEAT(" ",80)
      Precords(i)(1:nlen) = recbuf(i)%str(1:nlen)
      Precords(i)(73:73) = "P"
    End Do

  End Subroutine genPrecords
     
  Subroutine getEntityFields(string, fields)
  !! Given a string of comma separated fields, return the individual field
  !! strings in the fields array

    Character(*),                Intent(INOUT) :: string
    Type(string_t), Allocatable, Intent(INOUT) :: fields(:)

    Integer                   :: i, nfields
    Integer,      Allocatable :: fs(:)
    Integer,      Allocatable :: fe(:)

    Call getField(string, nfields, fs, fe)

    If (ALLOCATED(fields)) DEALLOCATE(fields)

    ALLOCATE(fields(nfields))

    Do i=1, nfields
      If (string(fs(i-1):fe(i+1)) == ",," .OR.                               &  
          string(fs(i-1):fe(i+1)) == ",/") Then
        fields(i)%str = "0"
      Else
        fields(i)%str = string(fs(i):fe(i))
      End If
    End Do

  End Subroutine getEntityFields    

! Begin entity methods
 
  Subroutine deallocUnsupported(this)
  !! Deallocates and resets unsupported entity
    Class(unsupported_t), Intent(INOUT) :: this
    !! unsupported class argument
 
    this%entity_type = -1
    this%form        = 0

  End Subroutine deallocUnsupported

  Subroutine getUnsupported(this, Precords, form)
  !! Creates unsupported entity type from IGES file records
 
    Class(unsupported_t),       Intent(INOUT) :: this
    !! Unsupported entity argument
    Character(80),              Intent(IN)    :: Precords(:)
    !! Array of P section records for this entity
    Integer,          Optional, Intent(IN)    :: form 
    !! Optional form number for this entity (not required)
 
    Character(:), Allocatable :: string
    Integer      :: ET

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET
 
    this%entity_type = -ET
    
    this%form = 0
    If (PRESENT(form)) this%form = form

  End Subroutine getUnsupported

  Subroutine makeUnsupported(this, Precords)
  !! Creates IGES file P section NULL records from unsupported entity type
 
    Class(unsupported_t),             Intent(IN)    :: this
    !! Unsupported entity type argument

    Character(80),       Allocatable, Intent(INOUT) :: Precords(:)
    !! IGES file P section records for this entity 

! Create NULL records for unsupported types
    If (this%entity_type < 0) Then
      ALLOCATE(Precords(1))
      Precords(1) = REPEAT(" ", 80)
      Precords(1)(1:2)   = "0;"
      Precords(1)(73:73) = "P"
    Else
      Write(stderr, *) ''
      Write(stderr, *) ' forIGES WARNING'
      Write(stderr,'(" makeUnsupported: unsupported type not < 0")')
      ALLOCATE(Precords(1))
      Precords(1) = REPEAT(" ", 80)
      Precords(1)(1:2)   = "0;"
      Precords(1)(73:73) = "P"
    End If
 
  End Subroutine makeUnsupported

  Subroutine initUnsupported(this, DEP)
  !! Initializes an unsupported entity type from user input

    Class(unsupported_t),   Intent(INOUT) :: this
    
    Integer,                Intent(IN)    :: DEP

    this%entity_type = 0
    this%DEP         = DEP

  End Subroutine initUnsupported

  Subroutine outputUnsupported(this, ounit)
!! Outputs an unsupported entity message to user output unit

    Class(unsupported_t), Intent(IN) :: this
    Integer,              Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Unsupported Entity *****'
    Write(ounit,*) ''

    Write(ounit,'(" This entity is a placeholder for unsupported entity : ", i0)')            ABS(this%entity_type)
 
  End Subroutine outputUnsupported

  Subroutine copyToUnsupported(this, that)
!! Copies this entity to that entity

    Class(unsupported_t), Intent(IN)  :: this
    Type(unsupported_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToUnsupported

  Subroutine initTypeUnsupported(this, that)
  !! Copies that entity to this entity 

    Class(unsupported_t), Intent(OUT) :: this
    Type(unsupported_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeUnsupported
 
  Subroutine deallocE0(this)
  !! Deallocates and resets this entity0_t type

    Class(entity0_t), Intent(INOUT) :: this
  
    this%entity_type = -1
    this%form        = 0

  End Subroutine deallocE0

  Subroutine getE0(this, Precords, form)
  !! Translate IGES P records into this entity0_t entity type components
  
    Class(entity0_t),           Intent(INOUT) :: this
    Character(80),              Intent(IN)    :: Precords(:)
    Integer,          Optional, Intent(IN)    :: form 

    Character(:), Allocatable :: string
    Integer      :: ET

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET
 
    this%entity_type = ET
    
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE0 : Invalid form - must be  0")') 
      End If
    End If

  End Subroutine getE0

  Subroutine makeE0(this, Precords)
  !! Translate this entity0_t type components into IGES P records

    Class(entity0_t),              Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    If (this%entity_type == 0) Then
      ALLOCATE(Precords(1))
      Precords(1) = REPEAT(" ", 80)
      Precords(1)(1:2)   = "0;"
      Precords(1)(73:73) = "P"
    Else
      Write(stderr,*) '' 
      Write(stderr,*) ' forIGES WARNING'
      Write(stderr,'(" makeE0 : entity type is not 0")')
      ALLOCATE(Precords(1))
      Precords(1) = REPEAT(" ", 80)
      Precords(1)(1:2)   = "0;"
      Precords(1)(73:73) = "P"
    End If 
  End Subroutine makeE0

  Subroutine initE0(this, DEP) 
  !! Initialize this entity0_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity0_t), Intent(INOUT) :: this
    Integer,          Intent(IN)    :: DEP

    this%entity_type = 0
    this%DEP         = DEP

  End Subroutine initE0

  Subroutine outputE0(this, ounit)
  !! Output this entity0_t entity type components to a user output unit

    Class(entity0_t), Intent(IN) :: this
    Integer,          Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 0 - Null *****'
    Write(ounit,*) ''
    Write(ounit,'(" entity type : ", i0)') this%entity_type

    Write(ounit,*) ''
    Write(ounit,*) ' NULL entities are not processed'
 
  End Subroutine outputE0

  Subroutine copyToE0(this, that)
  !! Copy this entity to that entity

    Class(entity0_t), Intent(IN)  :: this
    Type(entity0_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE0

  Subroutine initTypeE0(this, that)
  !! Copy that entity to this entity

    Class(entity0_t), Intent(OUT) :: this
    Type(entity0_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE0
 
  Subroutine deallocE100(this)
  !! Deallocate and reset this entity100_t entity

    Class(entity100_t), Intent(INOUT) :: this

    this%entity_type = -1
    this%form        = 0 
    this%ZT          = 0.0_WP 
    this%X1          = 0.0_WP 
    this%Y1          = 0.0_WP 
    this%X2          = 0.0_WP 
    this%Y2          = 0.0_WP 
    this%X3          = 0.0_WP 
    this%Y3          = 0.0_WP 

  End Subroutine deallocE100

  Subroutine getE100(this, Precords, form)
  !! Translate IGES P records into this entity100_t entity type components

    Class(entity100_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string
    Integer  :: ET
    Real(WP) :: ZT
    Real(WP) :: X1
    Real(WP) :: Y1
    Real(WP) :: X2
    Real(WP) :: Y2
    Real(WP) :: X3
    Real(WP) :: Y3

    ZT = 0.0_WP
    X1 = 0.0_WP
    Y1 = 0.0_WP
    X2 = 0.0_WP
    Y2 = 0.0_WP
    X3 = 0.0_WP
    Y3 = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, ZT, X1, Y1, X2, Y2, X3, Y3

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE100 : Invalid form - must be 0")') 
      End If
    End If

    this%ZT = ZT
    this%X1 = X1 
    this%Y1 = Y1 
    this%X2 = X2 
    this%Y2 = Y2 
    this%X3 = X3 
    this%Y3 = Y3 

  End Subroutine getE100

  Subroutine makeE100(this, Precords)
  !! Translate this entity100_t type components into IGES P records

    Class(entity100_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields
    Character(:),   Allocatable :: fmt
    Character(60)               :: buf
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 8
    ALLOCATE(fields(8)) 
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%ZT
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X1
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y1
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X2
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y2
    fields(6)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X3
    fields(7)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y3
    fields(8)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf,Precords)

  End Subroutine makeE100

  Subroutine initE100(this, DEP, ZT, X1, Y1, X2, Y2, X3, Y3, tmat ) 
  !! Initialize this entity100_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity100_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Real(WP),           Intent(IN)    :: ZT 
    Real(WP),           Intent(IN)    :: X1 
    Real(WP),           Intent(IN)    :: Y1 
    Real(WP),           Intent(IN)    :: X2 
    Real(WP),           Intent(IN)    :: Y2 
    Real(WP),           Intent(IN)    :: X3 
    Real(WP),           Intent(IN)    :: Y3

    Integer,  Optional, Intent(IN)    :: tmat


    this%entity_type = 100
 
    this%DEP = DEP
    this%ZT  = ZT
    this%X1  = X1 
    this%Y1  = Y1 
    this%X2  = X2 
    this%Y2  = Y2 
    this%X3  = X3 
    this%Y3  = Y3 

    If (PRESENT(tmat)) this%tmat = tmat

  End Subroutine initE100

  Subroutine outputE100(this, ounit)
  !! Output this entity100_t entity type components to a user output unit

    Class(entity100_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 100 - Circular Arc *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" ZT           : ", ES15.6)') this%ZT
    Write(ounit,'(" X1           : ", ES15.6)') this%X1
    Write(ounit,'(" Y1           : ", ES15.6)') this%Y1
    Write(ounit,'(" X2           : ", ES15.6)') this%X2
    Write(ounit,'(" Y2           : ", ES15.6)') this%Y2
    Write(ounit,'(" X3           : ", ES15.6)') this%X3
    Write(ounit,'(" Y3           : ", ES15.6)') this%Y3

  End Subroutine outputE100

  Subroutine copyToE100(this, that)
  !! Copy this entity to that entity

    Class(entity100_t), Intent(IN)  :: this
    Type(entity100_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE100

  Subroutine initTypeE100(this, that)
  !! Copy that entity to this entity

    Class(entity100_t), Intent(OUT) :: this
    Type(entity100_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE100
 
  Subroutine deallocE102(this)
  !! Deallocate and reset this entity102_t entity

    Class(entity102_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%N           = 0 
    this%form        = 0
    If (ALLOCATED(this%DE)) DEALLOCATE(this%DE)

  End Subroutine deallocE102

  Subroutine getE102(this, Precords, form)
  !! Translate IGES P records into this entity102_t entity type components

    Class(entity102_t),         Intent(INOUT) :: this
    Character(80),              Intent(IN)    :: Precords(:)
    Integer,          Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string
    
    Integer                   :: dum 
    Integer                   :: ET 
    Integer                   :: N
    Integer,      Allocatable :: DE(:)

    Call PentityToString(Precords, string, this%DEP)

    Read(string, *) ET, N
    ALLOCATE(DE(N), SOURCE=0)
    Read(string, *) dum, dum, DE

    DE = (DE+1)/2

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE102 : Invalid form - must be 0")') 
      End If
    End If

    this%N                  = N
  
    Call MOVE_ALLOC(DE, this%DE)

  End Subroutine getE102

  Subroutine makeE102(this, Precords)
  !! Translate this entity102_t type components into IGES P records

    Class(entity102_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer       :: i, nfields, is
    Integer      :: DE, N
    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    N = this%N

    nfields = 2+N 
    ALLOCATE(fields(nfields)) 
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    is = 2
    Do i=1,N
      is = is+1
      DE = DE2PTR(this%DE(i))
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE
      If (i /= N) Then 
        fields(is)%str = TRIM(ADJUSTL(buf))//","
      Else
        fields(is)%str = TRIM(ADJUSTL(buf))//";"
      End If
    End Do

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)
   
  End Subroutine makeE102

  Subroutine initE102(this, DEP, N, DE, tmat)
  !! Initialize this entity102_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity102_t), Intent(INOUT) :: this
    Integer,            Intent(IN)    :: DEP 
    Integer,            Intent(IN)    :: N 
    Integer,            Intent(IN)    :: DE(N)

    Integer,  Optional, Intent(IN)    :: tmat

    this%entity_type = 102 
    this%DEP         = DEP
    this%N           = N
    this%DE          = DE

    If (PRESENT(tmat)) this%tmat = tmat

  End Subroutine initE102

  Subroutine outputE102(this,ounit)
  !! Output this entity102_t entity type components to a user output unit

    Class(entity102_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 102 - Composite Curve *****'
    Write(ounit,*) ''


    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" N            : ", i0)') this%N 
    Write(ounit,*) ''
    Write(ounit,'(" DE Index     : ")')
    Write(ounit,'(1x, 8i10)') this%DE(:)
    Write(ounit,*) ''
    Write(ounit,'(" DE Seq. No.  : ")')
    Write(ounit,'(1x, 8i10)') DE2PTR(this%DE(:))

  End Subroutine outputE102

  Subroutine copyToE102(this, that)
  !! Copy this entity to that entity

    Class(entity102_t), Intent(IN)  :: this
    Type(entity102_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE102

  Subroutine initTypeE102(this, that)
  !! Copy that entity to this entity

    Class(entity102_t), Intent(OUT) :: this
    Type(entity102_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE102
 
  Subroutine deallocE104(this)
  !! Deallocate and reset this entity104_t entity

    Class(entity104_t), Intent(INOUT) :: this

    this%entity_type        = -1 
    this%form               = 0 
    this%A                  = 0.0_WP 
    this%B                  = 0.0_WP 
    this%C                  = 0.0_WP 
    this%D                  = 0.0_WP 
    this%E                  = 0.0_WP 
    this%F                  = 0.0_WP 
    this%ZT                 = 0.0_WP 
    this%X1                 = 0.0_WP 
    this%Y1                 = 0.0_WP 
    this%X2                 = 0.0_WP 
    this%Y2                 = 0.0_WP
 
  End Subroutine deallocE104

  Subroutine getE104(this, Precords, form)
  !! Translate IGES P records into this entity104_t entity type components

    Class(entity104_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 

    Character(:), Allocatable :: string

    Integer  :: ET
    Real(WP) :: A
    Real(WP) :: B
    Real(WP) :: C
    Real(WP) :: D
    Real(WP) :: E
    Real(WP) :: F
    Real(WP) :: ZT
    Real(WP) :: X1
    Real(WP) :: Y1
    Real(WP) :: X2
    Real(WP) :: Y2

    A  = 0.0_WP
    B  = 0.0_WP
    C  = 0.0_WP
    D  = 0.0_WP
    E  = 0.0_WP
    F  = 0.0_WP
    ZT = 0.0_WP
    X1 = 0.0_WP
    Y1 = 0.0_WP
    X2 = 0.0_WP
    Y2 = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, A, B, C, D, E, F, ZT, X1, Y1, X2, Y2 

    this%entity_type        = ET
    this%form = 0
    
    If (PRESENT(form)) Then
      this%form = form
      If (form < 1 .OR. form >3) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE102 : Invalid form - must be 1, 2, or 3")') 
      End If
    End If


    this%A  = A
    this%B  = B
    this%C  = C
    this%D  = D
    this%E  = E
    this%F  = F
    this%ZT = ZT 
    this%X1 = X1 
    this%Y1 = Y1 
    this%X2 = X2 
    this%Y2 = Y2 

  End Subroutine getE104

  Subroutine makeE104(this,Precords)
  !! Translate this entity104_t type components into IGES P records

    Class(entity104_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer       :: nfields
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 12 
    ALLOCATE(fields(nfields))
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%A
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%B
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%C
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%D
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%E
    fields(6)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%F
    fields(7)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%ZT 
    fields(8)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X1
    fields(9)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y1 
    fields(10)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X2
    fields(11)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y2 
    fields(12)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE104

  Subroutine initE104(this, DEP, form, A, B, C, D, E, F, ZT, X1, Y1, X2, Y2,   &
                      tmat) 
  !! Initialize this entity104_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity104_t), Intent(INOUT) :: this
    Integer,            Intent(IN)    :: DEP 
    Integer,            Intent(IN)    :: form
    Real(WP),           Intent(IN)    :: A
    Real(WP),           Intent(IN)    :: B
    Real(WP),           Intent(IN)    :: C
    Real(WP),           Intent(IN)    :: D
    Real(WP),           Intent(IN)    :: E
    Real(WP),           Intent(IN)    :: F
    Real(WP),           Intent(IN)    :: ZT
    Real(WP),           Intent(IN)    :: X1 
    Real(WP),           Intent(IN)    :: Y1 
    Real(WP),           Intent(IN)    :: X2 
    Real(WP),           Intent(IN)    :: Y2 
    Integer,  Optional, Intent(IN)    :: tmat


    this%entity_type = 104
    this%DEP         = DEP
    this%form        = form
    Select Case(form)
      Case(1,2,3)

        this%A  = A
        this%B  = B
        this%C  = C
        this%D  = D
        this%E  = E
        this%F  = F
        this%ZT = ZT 
        this%X1 = X1 
        this%Y1 = Y1 
        this%X2 = X2 
        this%Y2 = Y2
        If (PRESENT(tmat)) this%tmat = tmat
      Case Default
        Write(stderr,*) ' *** forIGES ERROR ***' 
        ERROR STOP " initE104: Invalid form - must be 1,2, or 3"
    End Select
 
  End Subroutine initE104

  Subroutine outputE104(this, ounit)
  !! Output this entity104_t entity type components to a user output unit

    Class(entity104_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer  :: iform

    iform = this%form

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 104 - Conic Arc *****'
    Write(ounit,*) ''
    Select Case(iform)
      Case(1) 
        Write(ounit,*) ' ** Parent Curve is an Ellipse'
      Case(2) 
        Write(ounit,*) ' ** Parent Curve is a Hyperbola'
      Case(3) 
        Write(ounit,*) ' ** Parent Curve is a Parabola'
    End Select
    
    Write(ounit,*) ''
    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat


    Write(ounit,'(" A            : ", ES15.6)') this%A
    Write(ounit,'(" B            : ", ES15.6)') this%B
    Write(ounit,'(" C            : ", ES15.6)') this%C
    Write(ounit,'(" D            : ", ES15.6)') this%D
    Write(ounit,'(" E            : ", ES15.6)') this%E
    Write(ounit,'(" F            : ", ES15.6)') this%F
    Write(ounit,'(" ZT           : ", ES15.6)') this%ZT
    Write(ounit,'(" X1           : ", ES15.6)') this%X1
    Write(ounit,'(" Y1           : ", ES15.6)') this%Y1
    Write(ounit,'(" X2           : ", ES15.6)') this%X2
    Write(ounit,'(" Y2           : ", ES15.6)') this%Y2

  End Subroutine outputE104

  Subroutine copyToE104(this, that)
  !! Copy this entity to that entity

    Class(entity104_t), Intent(IN)  :: this
    Type(entity104_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE104

  Subroutine initTypeE104(this, that)
  !! Copy that entity to this entity

    Class(entity104_t), Intent(OUT) :: this
    Type(entity104_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE104
 
  Subroutine deallocE106(this)
  !! Deallocate and reset this entity106_t entity

    Class(entity106_t), Intent(INOUT) :: this

    this%entity_type = -1
    this%form        = 0
    this%N           = 0
    If (this%IP == 1) Then
      this%ZT = 0.0_WP 
    End If
    If (ALLOCATED(this%X))   DEALLOCATE(this%X) 
    If (ALLOCATED(this%Y))   DEALLOCATE(this%Y)
    If (this%IP > 1) Then
      If(ALLOCATED(this%Z))  DEALLOCATE(this%Z)
    End If
    If (this%IP == 3) Then
      If(ALLOCATED(this%IR)) DEALLOCATE(this%IR)
      If(ALLOCATED(this%JR)) DEALLOCATE(this%JR)
      If(ALLOCATED(this%KR)) DEALLOCATE(this%KR)
    End If 
    this%IP = 0

  End Subroutine deallocE106

  Subroutine getE106(this, Precords, form)
  !! Translate IGES P records into this entity106_t entity type components

    Class(entity106_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string
    Integer               :: ET
    Integer               :: IP
    Integer               :: N
    Real(WP)              :: ZT 
    Real(WP), Allocatable :: X(:) 
    Real(WP), Allocatable :: Y(:) 
    Real(WP), Allocatable :: Z(:) 
    Real(WP), Allocatable :: IR(:) 
    Real(WP), Allocatable :: JR(:) 
    Real(WP), Allocatable :: KR(:)
!!    Real(WP), Allocatable :: temp(:) 

    Integer :: i, iform, dum

    IP = 0
    N  = 0
    ZT = 0.0_WP

    this%form = 1
    If(PRESENT(form)) this%form = form
    Select Case (form)
      Case(1,2,3,11,12,13,63)

        Call PentityToString(Precords, string, this%DEP)
        Read(string,*) ET, IP, N
        ALLOCATE(X(N), SOURCE=0.0_WP)
        ALLOCATE(Y(N), SOURCE=0.0_WP)
        If (IP > 1) Then
          ALLOCATE(Z(N), SOURCE=0.0_WP)
        End If
        If (IP == 3) Then
          ALLOCATE(IR(N), SOURCE=0.0_WP)
          ALLOCATE(JR(N), SOURCE=0.0_WP)
          ALLOCATE(KR(N), SOURCE=0.0_WP)
        End If 

        If (IP == 1 .OR. iform == 63) Then
          Read(string,*) dum, dum, dum, ZT, (X(i),Y(i), i=1,N)
        ElseIf(IP == 2 ) Then
          Read(string,*) dum, dum, dum, (X(i),Y(i),Z(i), i=1,N)
        ElseIf(IP == 3) Then
          Read(string,*) dum, dum, dum, (X(i),Y(i),Z(i),IR(i),JR(i),KR(i), i=1,N)
        End If 

        this%entity_type = ET

        this%IP          = IP 
        this%N           = N
        If (IP == 1 .OR. iform==63) Then
          this%ZT = ZT
        End If
        Call MOVE_ALLOC(X, this%X) 
        Call MOVE_ALLOC(Y, this%Y)
        If (IP > 1) Then
          Call MOVE_ALLOC(Z, this%Z)
        End If
        If (IP == 3) Then
          Call MOVE_ALLOC(IR, this%IR)
          Call MOVE_ALLOC(JR, this%JR)
          Call MOVE_ALLOC(KR, this%KR)
        End If 
      Case Default
        Write(stderr, *) ''
        Write(stderr,*) ' *** forIGES Warning ***'
        Write(stderr,'(" getE106: Invalid form - must be 1-3, 11-13, and 63")')
    End Select 

  End Subroutine getE106

  Subroutine makeE106(this, Precords)
  !! Translate this entity106_t type components into IGES P records

    Class(entity106_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer       :: i, nfields, is
    Integer       :: IP
    Integer       :: N
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    Select case(this%form)
      Case(1,2,3, 11,12,13, 63)
        IP = this%IP
        N  = this%N 
        nfields = 3
        If (IP == 1) Then
          nfields = 4 + 2*N
        ElseIf (IP == 2) Then
          nfields = 3 + 3*N
        ElseIf (IP == 3) Then
          nfields = 3 + 6*N
        End If
      
        ALLOCATE(fields(nfields))
        buf = REPEAT(" ",60)
        Write(buf,'(i0)') this%entity_type
        fields(1)%str = TRIM(ADJUSTL(buf))//","
        buf = REPEAT(" ",60)
        Write(buf,'(i0)') IP 
        fields(2)%str = TRIM(ADJUSTL(buf))//","
        buf = REPEAT(" ",60)
        Write(buf,'(i0)') N 
        fields(3)%str = TRIM(ADJUSTL(buf))//","
        is = 3
        If (IP == 1) Then
          buf = REPEAT(" ",60)
          Write(buf,fmt) this%ZT 
          fields(4)%str = TRIM(ADJUSTL(buf))//","
          is = 4
          Do i=1,n
            is = is+1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%X(i) 
            fields(is)%str = TRIM(ADJUSTL(buf))//","
            is = is + 1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%Y(i)
            If (i /= N) Then 
              fields(is)%str = TRIM(ADJUSTL(buf))//","
            Else
              fields(is)%str = TRIM(ADJUSTL(buf))//";"
            End If 
          End Do
        ElseIf (IP == 2) Then
          Do i=1,N
            is = is+1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%X(i) 
            fields(is)%str = TRIM(ADJUSTL(buf))//","
            is = is + 1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%Y(i) 
            fields(is)%str = TRIM(ADJUSTL(buf))//","
            is = is + 1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%Z(i) 
            If (i /= N) Then 
              fields(is)%str = TRIM(ADJUSTL(buf))//","
            Else
              fields(is)%str = TRIM(ADJUSTL(buf))//";"
            End If
          End Do
        ElseIf (IP==3) Then
          Do i=1,N 
            is = is+1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%X(i) 
            fields(is)%str = TRIM(ADJUSTL(buf))//","
            is = is + 1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%Y(i) 
            fields(is)%str = TRIM(ADJUSTL(buf))//","
            is = is + 1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%Z(i) 
            fields(is)%str = TRIM(ADJUSTL(buf))//","
            is = is+1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%IR(i) 
            fields(is)%str = TRIM(ADJUSTL(buf))//","
            is = is + 1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%JR(i) 
            fields(is)%str = TRIM(ADJUSTL(buf))//","
            is = is + 1
            buf = REPEAT(" ",60)
            Write(buf,fmt) this%KR(i) 
            If (i /= N) Then 
              fields(is)%str = TRIM(ADJUSTL(buf))//","
            Else
              fields(is)%str = TRIM(ADJUSTL(buf))//";"
            End If
          End Do
        End If
        
        Call fieldsToRecs(fields, recbuf, 64)
        Call genPrecords(recbuf, Precords)

      Case Default
        Write(stderr,*) ' *** forIGES WARNING ***'
        Write(stderr,'(" makeE106: form must be 1-3, 11-13, or 63")')
    End Select
 
  End Subroutine makeE106

  Subroutine initE106(this, DEP, form, IP, N, X, Y, ZT, Z, IR, JR, KR, tmat) 
  !! Initialize this entity106_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity106_t), Intent(INOUT) :: this
    
    Integer,            Intent(IN)  :: DEP 
    Integer,            Intent(IN)  :: form
    Integer,            Intent(IN)  :: IP
    Integer,            Intent(IN)  :: N
    Real(WP),           Intent(IN)  :: X(N) 
    Real(WP),           Intent(IN)  :: Y(N) 
    Real(WP), Optional, Intent(IN)  :: ZT
    Real(WP), Optional, Intent(IN)  :: Z(N)
    Real(WP), Optional, Intent(IN)  :: IR(N)
    Real(WP), Optional, Intent(IN)  :: JR(N)
    Real(WP), Optional, Intent(IN)  :: KR(N)
    Integer,  Optional, Intent(IN)  :: tmat 

    this%entity_type = 106 
    this%DEP         = DEP 
    this%form        = form
    
    Select Case(this%form)
      Case(1,2,3, 11,12,13, 63)
        this%IP          = IP 
        this%N           = N
        If (IP == 1 .OR. this%form==63) Then
          If (PRESENT(ZT)) Then
            this%ZT = ZT
          Else
            Write(stderr,*) ' *** forIGES ERROR ***'
            ERROR STOP " InitE106 : IP = 1 but ZT is not present"
          End If
        End If

        this%X = X 
        this%Y = Y

        If (IP > 1) Then
          If (PRESENT(Z)) Then
            this%Z = Z
          Else
            Write(stderr,*) ' *** forIGES ERROR ***'
            ERROR STOP "InitE106 : IP > 1 but Z is not present"
          End If
        End If

        If (IP == 3) Then
          If (PRESENT(IR)) Then 
            this%IR = IR
          Else
            Write(stderr,*) ' *** forIGES ERROR ***'
            ERROR STOP " InitE106 : IP == 3 but IR is not present"
          End If
          If (PRESENT(JR)) Then 
            this%JR = JR
          Else
            Write(stderr,*) ' *** forIGES ERROR ***'
            ERROR STOP " InitE106 : IP == 3 but JR is not present"
          End If
          If (PRESENT(KR)) Then 
            this%KR = KR
          Else
            Write(stderr,*) ' *** forIGES ERROR ***'
            ERROR STOP " InitE106 : IP == 3 but KR is not present"
          End If
        End If
        If (PRESENT(tmat)) this%tmat = tmat
 
      Case Default
        Write(stderr,*) ' *** forIGES ERROR ***'
        ERROR STOP " initE106: Invalid form - must be 1-3, 11-13, or 63"
    End Select

  End Subroutine initE106

  Subroutine outputE106(this, ounit)
  !! Output this entity106_t entity type components to a user output unit

    Class(entity106_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    If (this%form <=3) Then
      Write(ounit,*) ''
      Write(ounit,*) ' *****  Entity 106 - Copious Data Form 1-3 *****'
      Write(ounit,*) ''
      Write(ounit,*) ' +++ General Coordinate Data ***'
      Write(ounit,*) ''
    ElseIf (this%form>=11 .AND. this%form<=13) Then
      Write(ounit,*) ''
      Write(ounit,*) ' *****  Entity 106 - Copious Data  Form 11-13 *****'
      Write(ounit,*) ''
      Write(ounit,*) ' +++ Linear Path ***'
      Write(ounit,*) ''
    ElseIf (this%form==63) Then
      Write(ounit,*) ''
      Write(ounit,*) ' *****  Entity 106 - Copious Data Form 63 *****'
      Write(ounit,*) ''
      Write(ounit,*) ' +++ Simple Closed Planar Curve ***'
      Write(ounit,*) ''
    Else
      Write(ounit,*) ''
      Write(ounit,*) ' *****  Entity 106 - Copius Data Form is not supported *****'
      Write(ounit,'(" Entity Form  : ", i0)') this%form
      Write(ounit,*) ''
      RETURN
    End If
    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" IP           : ",i0)') this%IP
    Write(ounit,'(" N            : ",i0)') this%N
    If (this%IP == 1 .OR. this%form == 63) Then
    Write(ounit,*) ''
      Write(ounit,'(" ZT         : ",ES15.6)') this%ZT
    End If
    Write(ounit,*) ''
    Write(ounit,'(" X            : ")')
    Write(ounit,'(1x,5ES15.6)') this%X(:)
    Write(ounit,*) ''
    Write(ounit,'(" Y            : ")')
    Write(ounit,'(1x,5ES15.6)') this%Y(:)
    If (this%IP > 1) Then
      Write(ounit,*) ''
      Write(ounit,'(" Z          : ")')
      Write(ounit,'(1x,5ES15.6)') this%Z(:)
    End If
    If (this%IP == 3) Then
      Write(ounit,*) ''
      Write(ounit,'(" IR         : ")')
      Write(ounit,'(1x,5ES15.6)') this%IR(:)
      Write(ounit,*) ''
      Write(ounit,'(" JR         : ")')
      Write(ounit,'(1x,5ES15.6)') this%JR(:)
      Write(ounit,*) ''
      Write(ounit,'(" KR         : ")')
      Write(ounit,'(1x,5ES15.6)') this%KR(:)
    End If
 
  End Subroutine outputE106

  Subroutine copyToE106(this, that)
  !! Copy this entity to that entity

    Class(entity106_t), Intent(IN)  :: this
    Type(entity106_t), Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE106

  Subroutine initTypeE106(this, that)
  !! Copy that entity to this entity

    Class(entity106_t), Intent(OUT) :: this
    Type(entity106_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE106
 
  Subroutine deallocE108(this)
  !! Deallocate and reset this entity108_t entity

    Class(entity108_t), Intent(INOUT) :: this

    this%entity_type = -1
    this%form        = 0 
    this%A           = 0.0_WP 
    this%B           = 0.0_WP 
    this%C           = 0.0_WP 
    this%D           = 0.0_WP 
    this%PTR         = 0 
    this%X           = 0.0_WP 
    this%Y           = 0.0_WP 
    this%Z           = 0.0_WP 
    this%SIZE        = 0.0_WP
 
  End Subroutine deallocE108

  Subroutine getE108(this, Precords, form)
  !! Translate IGES P records into this entity108_t entity type components

    Class(entity108_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET
    Real(WP) :: A
    Real(WP) :: B
    Real(WP) :: C
    Real(WP) :: D
    Integer  :: PTR 
    Real(WP) :: X
    Real(WP) :: Y
    Real(WP) :: Z
    Real(WP) :: SIZE
 

    Call PentityToString(Precords, string, this%DEP)

    A    = 0.0_WP
    B    = 0.0_WP
    C    = 0.0_WP
    D    = 0.0_WP
    X    = 0.0_WP
    Y    = 0.0_WP
    Z    = 0.0_WP
    SIZE = 0.0_WP
    PTR  = 0

    Read(string,*) ET, A, B, C, D, PTR, X, Y, Z, SIZE

    this%form                    = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form < -1 .OR. form > 1) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE108 : Invalid form - must be -1, 0, 0 1")')
      End If
   End If
 
   If (this%form /= 0) Then
      PTR = (PTR+1)/2
    Else
      PTR = 0
    End If 

    this%entity_type        = ET

    this%A                  = A
    this%B                  = B
    this%C                  = C
    this%D                  = D
    this%PTR                = PTR
    this%X                  = X
    this%Y                  = Y
    this%Z                  = Z
    this%SIZE               = SIZE 

  End Subroutine getE108

  Subroutine makeE108(this, Precords)
  !! Translate this entity108_t type components into IGES P records

    Class(entity108_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:)

    Integer :: nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 10  
    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%A
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%B
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%C
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%D
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%PTR)
    fields(6)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X
    fields(7)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y
    fields(8)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Z
    fields(9)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%SIZE
    fields(10)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)
 
  End Subroutine makeE108

  Subroutine initE108(this, DEP, form, A, B, C, D, PTR, X, Y, Z, SIZE, tmat ) 
  !! Initialize this entity108_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity108_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: form 
    Real(WP),           Intent(IN) :: A
    Real(WP),           Intent(IN) :: B
    Real(WP),           Intent(IN) :: C
    Real(WP),           Intent(IN) :: D
    Integer,            Intent(IN) :: PTR 
    Real(WP),           Intent(IN) :: X
    Real(WP),           Intent(IN) :: Y
    Real(WP),           Intent(IN) :: Z
    Real(WP),           Intent(IN) :: SIZE 

    Integer,  Optional, Intent(IN) :: tmat 

    this%entity_type = 108
    this%DEP         = DEP
    this%form        = form
    If (form >=-1 .AND. form <=1) Then
      this%A           = A
      this%B           = B
      this%C           = C
      this%D           = D
      this%PTR         = PTR
      this%X           = X 
      this%Y           = Y 
      this%Z           = Z 
      this%SIZE        = SIZE 
      If (PRESENT(tmat)) this%tmat = tmat
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE108 : Invalid form - must be -1, 0, or 1"
    End if

  End Subroutine initE108

  Subroutine outputE108(this, ounit)
  !! Output this entity108_t entity type components to a user output unit

    Class(entity108_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: iform

    iform = this%form
    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 108 - Plane *****'
    Write(ounit,*) ''

    Select Case(iform)

      Case(-1)
        Write(ounit,*) ' **  Negative Bounded Plane **'
      Case(0) 
        Write(ounit,*) ' **  Unbounded Plane **'
      Case(1) 
        Write(ounit,*) ' **  Bounded Plane **'
    End Select

    Write(ounit,*) ''
    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" A            : ", ES15.6)') this%A
    Write(ounit,'(" B            : ", ES15.6)') this%B
    Write(ounit,'(" C            : ", ES15.6)') this%C
    Write(ounit,'(" D            : ", ES15.6)') this%D
    Write(ounit,'(" PTR Index    : ", i0)') this%PTR
    Write(ounit,'(" PTR Seq. No. : ", i0)') DE2PTR(this%PTR)
    Write(ounit,'(" X            : ", ES15.6)') this%X
    Write(ounit,'(" Y            : ", ES15.6)') this%Y
    Write(ounit,'(" Z            : ", ES15.6)') this%Z
    Write(ounit,'(" SIZE         : ", ES15.6)') this%SIZE

  End Subroutine outputE108

  Subroutine copyToE108(this, that)
  !! Copy this entity to that entity

    Class(entity108_t), Intent(IN)  :: this
    Type(entity108_t),  Intent(OUT) :: that
    
    that = this

  End Subroutine copyToE108

  Subroutine initTypeE108(this, that)
  !! Copy that entity to this entity

    Class(entity108_t), Intent(OUT) :: this
    Type(entity108_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE108
 
  Subroutine deallocE110(this)
  !! Deallocate and reset this entity110_t entity

    Class(entity110_t), Intent(INOUT) :: this

    this%entity_type = -1
    this%form        = 0 
    this%X1          = 0.0_WP 
    this%Y1          = 0.0_WP 
    this%Z1          = 0.0_WP 
    this%X2          = 0.0_WP 
    this%Y2          = 0.0_WP 
    this%Z2          = 0.0_WP
 
  End Subroutine deallocE110

  Subroutine getE110(this, Precords, form)
  !! Translate IGES P records into this entity110_t entity type components

    Class(entity110_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET
    Real(WP) :: X1
    Real(WP) :: Y1
    Real(WP) :: Z1
    Real(WP) :: X2
    Real(WP) :: Y2
    Real(WP) :: Z2

    X1 = 0.0_WP
    Y1 = 0.0_WP
    Z1 = 0.0_WP
    X2 = 0.0_WP
    Y2 = 0.0_WP
    Z2 = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, X1, Y1, Z1, X2, Y2, Z2

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form < 0 .OR. form > 2) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE110 : Invalid form - must be 0, 1, or 2")')
      End If
    End If
 

    this%X1 = X1
    this%Y1 = Y1
    this%Z1 = Z1
    this%X2 = X2
    this%Y2 = Y2
    this%Z2 = Z2

  End Subroutine getE110

  Subroutine makeE110(this, Precords)
  !! Translate this entity110_t type components into IGES P records

    Class(entity110_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 7  
    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X1
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y1
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Z1
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X2
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y2
    fields(6)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Z2
    fields(7)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE110

  Subroutine initE110(this, DEP, form, X1, Y1, Z1, X2, Y2, Z2, tmat ) 
  !! Initialize this entity110_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity110_t), Intent(INOUT) :: this
    
    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: form
    Real(WP),           Intent(IN) :: X1
    Real(WP),           Intent(IN) :: Y1
    Real(WP),           Intent(IN) :: Z1
    Real(WP),           Intent(IN) :: X2
    Real(WP),           Intent(IN) :: Y2
    Real(WP),           Intent(IN) :: Z2
    Integer,  Optional, Intent(IN) :: tmat
 
    this%entity_type  = 110 
    this%DEP          = DEP
    If (form >=0 .AND. form <=2) Then 
      this%form         = form
      this%X1           = X1
      this%Y1           = Y1
      this%Z1           = Z1
      this%X2           = X2
      this%Y2           = Y2
      this%Z2           = Z2
      If (PRESENT(tmat)) this%tmat = tmat
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE110 : Invalid form - must be 0, 1, or 2"
    End If

  End Subroutine initE110

  Subroutine outputE110(this, ounit)
  !! Output this entity110_t entity type components to a user output unit

    Class(entity110_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit


    Integer :: iform

    iform = this%form
    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 110 - Line *****'
    Write(ounit,*) ''

    If (iform == 1) Then
      Write(ounit,*) ' ** Semibounded Line **'
    Else If (iform == 2) Then
      Write(ounit,*) ' ** Unbounded Line **'
    End If
    Write(ounit,*) ''
    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" X1           : ", ES15.6)') this%X1
    Write(ounit,'(" Y1           : ", ES15.6)') this%Y1
    Write(ounit,'(" Z1           : ", ES15.6)') this%Z1
    Write(ounit,'(" X2           : ", ES15.6)') this%X2
    Write(ounit,'(" Y2           : ", ES15.6)') this%Y2
    Write(ounit,'(" Z2           : ", ES15.6)') this%Z2

  End Subroutine outputE110

  Subroutine copyToE110(this, that)
  !! Copy this entity to that entity

    Class(entity110_t), Intent(IN)  :: this
    Type(entity110_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE110

  Subroutine initTypeE110(this, that)
  !! Copy that entity to this entity

    Class(entity110_t), Intent(OUT) :: this
    Type(entity110_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE110
 
  Subroutine deallocE112(this)
  !! Deallocate and reset this entity112_t entity

    Class(entity112_t), Intent(INOUT) :: this

    this%entity_type        = -1 
    this%CTYPE              = 0
    this%H                  = 0
    this%NDIM               = 0 
    this%N                  = 0 
    this%TPX0               = 0.0_WP 
    this%TPX1               = 0.0_WP 
    this%TPX2               = 0.0_WP 
    this%TPX3               = 0.0_WP 
    this%TPY0               = 0.0_WP 
    this%TPY1               = 0.0_WP 
    this%TPY2               = 0.0_WP 
    this%TPY3               = 0.0_WP 
    this%TPZ0               = 0.0_WP 
    this%TPZ1               = 0.0_WP 
    this%TPZ2               = 0.0_WP 
    this%TPZ3               = 0.0_WP 

    If (ALLOCATED(this%T))     DEALLOCATE(this%T)
    If (ALLOCATED(this%Coefs)) DEALLOCATE(this%Coefs)

  End Subroutine deallocE112

  Subroutine getE112(this, Precords, form)
  !! Translate IGES P records into this entity112_t entity type components

    Class(entity112_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer                   :: dum
    Integer                   :: ET 
    Integer                   :: CTYPE
    Integer                   :: H 
    Integer                   :: NDIM 
    Integer                   :: N
    Real(WP),     Allocatable :: T(:)
    Real(WP),     Allocatable :: Coefs(:,:)
    Real(WP)                  :: TPX0
    Real(WP)                  :: TPX1
    Real(WP)                  :: TPX2
    Real(WP)                  :: TPX3
    Real(WP)                  :: TPY0
    Real(WP)                  :: TPY1
    Real(WP)                  :: TPY2
    Real(WP)                  :: TPY3
    Real(WP)                  :: TPZ0
    Real(WP)                  :: TPZ1
    Real(WP)                  :: TPZ2
    Real(WP)                  :: TPZ3


    CTYPE = 0
    H     = 0
    NDIM  = 0
    N     = 0
    TPX0 = 0.0_WP
    TPX1 = 0.0_WP
    TPX2 = 0.0_WP
    TPX3 = 0.0_WP
    TPY0 = 0.0_WP
    TPY1 = 0.0_WP
    TPY2 = 0.0_WP
    TPY3 = 0.0_WP
    TPZ0 = 0.0_WP
    TPZ1 = 0.0_WP
    TPZ2 = 0.0_WP
    TPZ3 = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string, *) ET, CTYPE, H, NDIM, N

    ALLOCATE(T(N+1),      Source = 0.0_WP)
    ALLOCATE(Coefs(12,N), Source = 0.0_WP)

    Read(string, *) dum, dum, dum, dum, dum, T, Coefs,                        &
                    TPX0, TPX1, TPX2, TPX3, TPY0, TPY1, TPY2, TPY3,           & 
                    TPZ0, TPZ1, TPZ2, TPZ3

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE112 : Invalid form - must be 0")')
      End If
    End If
 

    this%CTYPE              = CTYPE
    this%H                  = H
    this%NDIM               = NDIM
    this%N                  = N
    this%TPX0               = TPX0
    this%TPX1               = TPX1
    this%TPX2               = TPX2
    this%TPX3               = TPX3
    this%TPY0               = TPY0
    this%TPY1               = TPY1
    this%TPY2               = TPY2
    this%TPY3               = TPY3
    this%TPZ0               = TPZ0
    this%TPZ1               = TPZ1
    this%TPZ2               = TPZ2
    this%TPZ3               = TPZ3

    Call MOVE_ALLOC(T,     this%T)
    Call MOVE_ALLOC(Coefs, this%Coefs)

  End Subroutine getE112

  Subroutine makeE112(this, Precords)
  !! Translate this entity112_t type components into IGES P records

    Class(entity112_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, j, is, N, nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    N = this%N

    nfields = 18 + 13*N  
    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%CTYPE
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%H
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%NDIM
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    is = 5
    Do i=1,N+1
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%T(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
    End Do
    Do j=1,N
      Do i=1,12
        is = is+1
        buf = REPEAT(" ",60)
        Write(buf,fmt) this%coefs(i,j)
        fields(is)%str = TRIM(ADJUSTL(buf))//","
      End Do
    End Do
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPX0
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPX1
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPX2
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPX3
    fields(is)%str = TRIM(ADJUSTL(buf))//","
 
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPY0
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPY1
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPY2
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPY3
    fields(is)%str = TRIM(ADJUSTL(buf))//","

    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPZ0
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPZ1
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPZ2
    fields(is)%str = TRIM(ADJUSTL(buf))//","
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TPZ3
    fields(is)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE112

  Subroutine initE112(this, DEP, CTYPE, H, NDIM, N, T, Coefs, TPX0, TPX1,      &
                      TPX2, TPX3, TPY0, TPY1, TPY2, TPY3, TPZ0, TPZ1, TPZ2,    &
                      TPZ3, tmat) 
  !! Initialize this entity112_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity112_t), Intent(INOUT) :: this


    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: CTYPE 
    Integer,            Intent(IN)    :: H
    Integer,            Intent(IN)    :: NDIM 
    Integer,            Intent(IN)    :: N
    Real(WP),           Intent(IN)    :: T(N+1) 
    Real(WP),           Intent(IN)    :: Coefs(12,N) 
    Real(WP),           Intent(IN)    :: TPX0 
    Real(WP),           Intent(IN)    :: TPX1 
    Real(WP),           Intent(IN)    :: TPX2 
    Real(WP),           Intent(IN)    :: TPX3 
    Real(WP),           Intent(IN)    :: TPY0 
    Real(WP),           Intent(IN)    :: TPY1 
    Real(WP),           Intent(IN)    :: TPY2 
    Real(WP),           Intent(IN)    :: TPY3 
    Real(WP),           Intent(IN)    :: TPZ0 
    Real(WP),           Intent(IN)    :: TPZ1 
    Real(WP),           Intent(IN)    :: TPZ2 
    Real(WP),           Intent(IN)    :: TPZ3
    Integer,  Optional, Intent(IN)    :: tmat

    this%entity_type = 112
    this%DEP         = DEP
    this%form        = 0 
    this%CTYPE       = CTYPE
    this%H           = H
    this%NDIM        = NDIM
    this%N           = N
    this%T           = T
    this%Coefs       = Coefs 
    this%TPX0        = TPX0
    this%TPX1        = TPX1
    this%TPX2        = TPX2
    this%TPX3        = TPX3
    this%TPY0        = TPY0
    this%TPY1        = TPY1
    this%TPY2        = TPY2
    this%TPY3        = TPY3
    this%TPZ0        = TPZ0
    this%TPZ1        = TPZ1
    this%TPZ2        = TPZ2
    this%TPZ3        = TPZ3

    If (PRESENT(tmat)) this%tmat = tmat

  End Subroutine initE112

  Subroutine outputE112(this, ounit)
  !! Output this entity112_t entity type components to a user output unit

    Class(entity112_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: i
    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 112 - Parametric Spline Curve *****'
    Write(ounit,*) ''


    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" CTYPE        : ", i0)') this%CTYPE
    Write(ounit,'(" H            : ", i0)') this%H
    Write(ounit,'(" NDIM         : ", i0)') this%NDIM
    Write(ounit,'(" N            : ", i0)') this%N
    Write(ounit,*) ''
    Write(ounit,'(" T            : ")')
    Write(ounit,*) ''
    Write(ounit,'(1x, 5ES15.6)') this%T(:)
    Do i =1, this%N
      Write(ounit,*) ''
      Write(ounit,'(" Coefs(",i0,") : ")') i
      Write(ounit,'(1x, 5ES15.6)') this%Coefs(:,i)  
    End Do
    Write(ounit,*) ''
    Write(ounit,'(" TPX0     : ", ES15.6)') this%TPX0 
    Write(ounit,'(" TPX1     : ", ES15.6)') this%TPX1 
    Write(ounit,'(" TPX2     : ", ES15.6)') this%TPX2 
    Write(ounit,'(" TPX3     : ", ES15.6)') this%TPX3

    Write(ounit,'(" TPY0     : ", ES15.6)') this%TPY0 
    Write(ounit,'(" TPY1     : ", ES15.6)') this%TPY1 
    Write(ounit,'(" TPY2     : ", ES15.6)') this%TPY2 
    Write(ounit,'(" TPY3     : ", ES15.6)') this%TPY3

    Write(ounit,'(" TPZ0     : ", ES15.6)') this%TPZ0 
    Write(ounit,'(" TPZ1     : ", ES15.6)') this%TPZ1 
    Write(ounit,'(" TPZ2     : ", ES15.6)') this%TPZ2 
    Write(ounit,'(" TPZ3     : ", ES15.6)') this%TPZ3

  End Subroutine outputE112

  Subroutine copyToE112(this, that)
  !! Copy this entity to that entity

    Class(entity112_t), Intent(IN)  :: this
    Type(entity112_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE112

  Subroutine initTypeE112(this, that)
  !! Copy that entity to this entity

    Class(entity112_t), Intent(OUT) :: this
    Type(entity112_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE112
 
  Subroutine deallocE114(this)
  !! Deallocate and reset this entity114_t entity

    Class(entity114_t), Intent(INOUT) :: this

    this%entity_type        = -1
    this%form               = 0
    this%CTYPE              = 0 
    this%PTYPE              = 0 
    this%M                  = 0 
    this%N                  = 0 

    If (ALLOCATED(this%TU))    DEALLOCATE(this%TU)
    If (ALLOCATED(this%TV))    DEALLOCATE(this%TV)
    If (ALLOCATED(this%Coefs)) DEALLOCATE(this%Coefs)

  End Subroutine deallocE114

  Subroutine getE114(this, Precords, form)
  !! Translate IGES P records into this entity114_t entity type components

    Class(entity114_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string
    Integer                  :: ET
    Integer                  :: CTYPE
    Integer                  :: PTYPE
    Integer                  :: M
    Integer                  :: N
    Real(WP),    Allocatable :: TU(:)
    Real(WP),    Allocatable :: TV(:)
    Real(WP),    Allocatable :: Coefs(:,:,:)

    Integer :: i, j, k, l, jlast, dum, maxt 

    Real(WP), Allocatable :: temp(:)

    CTYPE = 0
    PTYPE = 0
    M     = 0
    N     = 0

    Call PentityToString(Precords, string, this%DEP)

    Read(string, *) ET, CTYPE, PTYPE, M, N
    maxt = 6+M+N+48*(M+1)*(N+1)

    ALLOCATE(TU(M+1),       SOURCE=0.0_WP)
    ALLOCATE(TV(N+1),       SOURCE=0.0_WP)
    ALLOCATE(Coefs(48,M,N), SOURCE=0.0_WP)
    ALLOCATE(temp(maxt),    SOURCE=0.0_WP)
    Read(string, *) dum, dum, dum, dum, dum, TU, TV 
    Read(string, *) dum, temp

    jlast = 7+M+N-1
    j = 0
    k = 0
    KLOOP: Do k=1,M
      ILOOP: Do i=1,N
        j = jlast+1
        Do l =1,48
          Coefs(l,k,i ) = temp(j+l-1)
        End Do
        jlast = j+48-1
      End Do ILOOP
      jlast = jlast+48
    End Do KLOOP

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE114 : Invalid form - must be 0")')
      End If
    End If
 
    this%CTYPE              = CTYPE
    this%PTYPE              = PTYPE
    this%M                  = M 
    this%N                  = N 

    Call MOVE_ALLOC(TU,    this%TU)
    Call MOVE_ALLOC(TV,    this%TV)
    Call MOVE_ALLOC(Coefs, this%Coefs)

  End Subroutine getE114

  Subroutine makeE114(this, Precords)
  !! Translate this entity114_t type components into IGES P records

    Class(entity114_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, j, k, is, M, N, nfields 
    Character(60) :: buf

    Real(WP) :: tz
    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    tz = 0.0_WP
    N = this%N
    M = this%M

    nfields = 199 + M + N + 48*(M*(N+1)+(N+1))

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%CTYPE
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PTYPE
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%M
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    is = 5
    Do i=1,M+1
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%TU(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
    End Do
    Do i=1,N+1
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%TV(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
    End Do
    Do i=1,M
      Do j=1,N
        Do k=1,48 
          is = is+1
          buf = REPEAT(" ",60)
          Write(buf,fmt) this%coefs(k,i,j)
          fields(is)%str = TRIM(ADJUSTL(buf))//","
        End Do
      End Do
      Do k=1,48
        is = is+1
        buf = REPEAT(" ",60)
        Write(buf,fmt) tz 
        fields(is)%str = TRIM(ADJUSTL(buf))//","
      End Do
    End Do

    Do k=1,192
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,fmt) tz 
      If (k /= 192) Then
        fields(is)%str = TRIM(ADJUSTL(buf))//","
      Else
        fields(is)%str = TRIM(ADJUSTL(buf))//";"
      End If
    End Do

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE114

  Subroutine initE114(this, DEP, CTYPE, PTYPE, M, N, TU, TV, Coefs, tmat) 
  !! Initialize this entity114_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity114_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: CTYPE 
    Integer,            Intent(IN) :: PTYPE 
    Integer,            Intent(IN) :: M 
    Integer,            Intent(IN) :: N 
    Real(WP),           Intent(IN) :: TU(M+1) 
    Real(WP),           Intent(IN) :: TV(N+1)
    Real(WP),           Intent(IN) :: Coefs(48,M,N) 
    Integer,  Optional, Intent(IN) :: tmat

    this%entity_type = 114 
    this%DEP         = DEP
    this%form        = 0
 
    this%CTYPE  = CTYPE
    this%PTYPE  = PTYPE
    this%M      = M 
    this%N      = N 
    this%TU     = TU
    this%TV     = TV
    this%Coefs  = Coefs 
    If (PRESENT(tmat)) this%tmat = tmat
 
  End Subroutine initE114

  Subroutine outputE114(this, ounit)
  !! Output this entity114_t entity type components to a user output unit

    Class(entity114_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: i, j

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 114 - Parametric Spline Surface ***'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" CTYPE              : ", i0)') this%CTYPE
    Write(ounit,'(" PTYPE              : ", i0)') this%PTYPE
    Write(ounit,'(" M                  : ", i0)') this%M
    Write(ounit,'(" N                  : ", i0)') this%N

    Write(ounit,*) ''
    Write(ounit,*) ' TU                :'
    Write(ounit,*) ''
    Write(ounit,'(1x,5ES15.6)') this%TU(:)
    Write(ounit,*) ''
    Write(ounit,*) ' TV                :'
    Write(ounit,*) ''
    Write(ounit,'(1x,5ES15.6)') this%TV(:)
   
    Do i=1,this%M
      Do j=1,this%N
        Write(ounit,*) ''
        Write(ounit,'(" Coefs(" ,i0,",",i0,")  :")') i,j
        Write(ounit,*) ''
        Write(ounit,'(1x,5ES15.6)') this%Coefs(:,i,j)
      End Do
   End Do

  End Subroutine outputE114

  Subroutine copyToE114(this, that)
  !! Copy this entity to that entity

    Class(entity114_t), Intent(IN)  :: this
    Type(entity114_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE114

  Subroutine initTypeE114(this, that)
  !! Copy that entity to this entity

    Class(entity114_t), Intent(OUT) :: this
    Type(entity114_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE114
 
  Subroutine deallocE116(this)
  !! Deallocate and reset this entity116_t entity

    Class(entity116_t), Intent(INOUT) :: this

    this%entity_type        = 0
    this%form               = 0 
    this%X                  = 0.0_WP 
    this%Y                  = 0.0_WP 
    this%Z                  = 0.0_WP 
    this%PTR                = 0 

  End Subroutine deallocE116

  Subroutine getE116(this, Precords, form)
  !! Translate IGES P records into this entity116_t entity type components

    Class(entity116_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET
    Real(WP) :: X
    Real(WP) :: Y
    Real(WP) :: Z
    Integer  :: PTR 

    X   = 0.0_WP
    Y   = 0.0_WP
    Z   = 0.0_WP
    PTR = 0

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, X, Y, Z, PTR 

    PTR = (PTR+1)/2

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE116 : Invalid form - must be 0")')
      End If
    End If
 
    this%X                  = X
    this%Y                  = Y
    this%Z                  = Z
    this%PTR                = PTR 

  End Subroutine getE116

  Subroutine makeE116(this, Precords)
  !! Translate this entity116_t type components into IGES P records
 
    Class(entity116_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 5 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Z
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%PTR) 
    fields(5)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE116

  Subroutine initE116(this, DEP, X, Y, Z, PTR, tmat) 
  !! Initialize this entity116_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity116_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Real(WP),           Intent(IN) :: X 
    Real(WP),           Intent(IN) :: Y 
    Real(WP),           Intent(IN) :: Z 
    Integer,            Intent(IN) :: PTR 
    Integer,  Optional, Intent(IN) :: tmat

    this%entity_type = 116
    this%DEP         = DEP

    this%X   = X
    this%Y   = Y
    this%Z   = Z
    this%PTR = PTR

    If (PRESENT(tmat)) this%tmat = tmat 

  End Subroutine initE116

  Subroutine outputE116(this, ounit)
  !! Output this entity116_t entity type components to a user output unit

    Class(entity116_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 116 - Point *****'
    Write(ounit,*) ''


    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" X            : ",ES15.6)') this%X
    Write(ounit,'(" Y            : ",ES15.6)') this%Y
    Write(ounit,'(" Z            : ",ES15.6)') this%Z
    Write(ounit,'(" PTR Index    : ",i0)') this%PTR
    Write(ounit,'(" PTR Seq. No. : ",i0)') DE2PTR(this%PTR)
 
  End Subroutine outputE116

  Subroutine copyToE116(this, that)
  !! Copy this entity to that entity

    Class(entity116_t), Intent(IN)  :: this
    Type(entity116_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE116

  Subroutine initTypeE116(this, that)
  !! Copy that entity to this entity

    Class(entity116_t), Intent(OUT) :: this
    Type(entity116_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE116
 
  Subroutine deallocE118(this)
  !! Deallocate and reset this entity118_t entity

    Class(entity118_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%DE1         = 0 
    this%DE2         = 0 
    this%DIRFLAG     = 0 
    this%DEVFLG      = 0 
 
  End Subroutine deallocE118

  Subroutine getE118(this, Precords, form)
  !! Translate IGES P records into this entity118_t entity type components

    Class(entity118_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer :: ET
    Integer :: DE1
    Integer :: DE2
    Integer :: DIRFLAG 
    Integer :: DEVFLG 

    DE1     = 0
    DE2     = 0
    DIRFLAG = 0
    DEVFLG  = 0

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, DE1, DE2, DIRFLAG, DEVFLG

    DE1 = (DE1+1)/2
    DE2 = (DE2+1)/2
 
    this%entity_type  = ET
    this%form         = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form >1) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE118 : Invalid form - must be 0 or 1")')
      End If
    End If
 

    this%DE1     = DE1
    this%DE2     = DE2
    this%DIRFLAG = DIRFLAG
    this%DEVFLG  = DEVFLG
 
  End Subroutine getE118

  Subroutine makeE118(this, Precords)
  !! Translate this entity118_t type components into IGES P records

    Class(entity118_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields 
    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    nfields = 5 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DE1) 
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DE2) 
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%DIRFLAG 
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%DEVFLG
    fields(5)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE118

  Subroutine initE118(this, DEP, form,  DE1, DE2, DIRFLAG, DEVFLG, tmat) 
  !! Initialize this entity118_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity118_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: form
    Integer,            Intent(IN) :: DE1
    Integer,            Intent(IN) :: DE2
    Integer,            Intent(IN) :: DIRFLAG 
    Integer,            Intent(IN) :: DEVFLG 
    Integer,  Optional, Intent(IN) :: tmat

    this%entity_type = 118
    this%DEP         = DEP
    this%form        = form
    If (form >=0 .AND. form <=1) Then
      this%DE1         = DE1
      this%DE2         = DE2
      this%DIRFLAG     = DIRFLAG
      this%DEVFLG      = DEVFLG
      If (PRESENT(tmat)) this%tmat = tmat
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE118 : Invalid form - must be 0 or 1"
    End If 

  End Subroutine initE118

  Subroutine outputE118(this, ounit)
  !! Output this entity118_t entity type components to a user output unit

    Class(entity118_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: iform

    iform = this%form
    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 118 - Ruled Surface *****'
    Write(ounit,*) ''

    If (iform == 0) Then
      Write(ounit,*) ' ** Equal Relative Arc Length **'
    Else If (iform == 1) Then
      Write(ounit,*) ' ** Equal Relative Parametric Values **'
    End If
    Write(ounit,*) ''
    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat
 
    Write(ounit,'(" DE1 Index    : ", i0)') this%DE1
    Write(ounit,'(" DE1 Seq. No. : ", i0)') DE2PTR(this%DE1)
    Write(ounit,'(" DE2 Index    : ", i0)') this%DE2
    Write(ounit,'(" DE2 Seq. No. : ", i0)') DE2PTR(this%DE2)
    Write(ounit,'(" DIRFLAG      : ", i0)') this%DIRFLAG
    Write(ounit,'(" DEVFLG       : ", i0)') this%DEVFLG

  End Subroutine outputE118

  Subroutine copyToE118(this, that)
  !! Copy this entity to that entity

    Class(entity118_t), Intent(IN)  :: this
    Type(entity118_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE118

  Subroutine initTypeE118(this, that)
  !! Copy that entity to this entity

    Class(entity118_t), Intent(OUT) :: this
    Type(entity118_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE118
 
  Subroutine deallocE120(this)
  !! Deallocate and reset this entity120_t entity

    Class(entity120_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%L           = 0 
    this%C           = 0 
    this%SA          = 0.0_WP 
    this%TA          = 0.0_WP 
 
  End Subroutine deallocE120

  Subroutine getE120(this, Precords, form)
  !! Translate IGES P records into this entity120_t entity type components

    Class(entity120_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET 
    Integer  :: L
    Integer  :: C
    Real(WP) :: SA
    Real(WP) :: TA

    L   = 0
    C   = 0
    SA  = 0.0_WP
    TA  = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, L, C, SA, TA

    L = (L+1)/2
    C = (C+1)/2

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /=0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE120 : Invalid form - must be 0")')
      End If
    End If
 
    this%L  = L
    this%C  = C
    this%SA = SA 
    this%TA = TA
 
  End Subroutine getE120

  Subroutine makeE120(this, Precords)
  !! Translate this entity120_t type components into IGES P records

    Class(entity120_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 5 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%L)
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%C)
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%SA
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TA
    fields(5)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE120

  Subroutine initE120(this, DEP, L, C, SA, TA, tmat) 
  !! Initialize this entity120_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity120_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: L 
    Integer,            Intent(IN) :: C
    Real(WP),           Intent(IN) :: SA 
    Real(WP),           Intent(IN) :: TA 
    Integer,  Optional, Intent(IN) :: tmat

    this%entity_type = 120
    this%DEP         = DEP 
    this%L           = L
    this%C           = C
    this%SA          = SA 
    this%TA          = TA
    If (PRESENT(tmat)) this%tmat = tmat
 
  End Subroutine initE120

  Subroutine outputE120(this, ounit)
  !! Output this entity120_t entity type components to a user output unit

    Class(entity120_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 120 - Surface of Revolution *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat
 
    Write(ounit,'(" L Index      : ", i0)') this%L
    Write(ounit,'(" L Seq. No.   : ", i0)') DE2PTR(this%L)
    Write(ounit,'(" C Index      : ", i0)') this%C
    Write(ounit,'(" C Seq. No.   : ", i0)') DE2PTR(this%C)
    Write(ounit,'(" SA           : ", ES15.6)') this%SA
    Write(ounit,'(" TA           : ", ES15.6)') this%TA

  End Subroutine outputE120

  Subroutine copyToE120(this, that)
  !! Copy this entity to that entity

    Class(entity120_t), Intent(IN)  :: this
    Type(entity120_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE120

  Subroutine initTypeE120(this, that)
  !! Copy that entity to this entity

    Class(entity120_t), Intent(OUT) :: this
    Type(entity120_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE120
 
  Subroutine deallocE122(this)
  !! Deallocate and reset this entity122_t entity

    Class(entity122_t), Intent(INOUT) :: this

    this%entity_type  = -1
    this%form         = 0 
    this%DE           = 0 
    this%LX           = 0.0_WP 
    this%LY           = 0.0_WP 
    this%LZ           = 0.0_WP 

  End Subroutine deallocE122

  Subroutine getE122(this, Precords, form)
  !! Translate IGES P records into this entity122_t entity type components

    Class(entity122_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET 
    Integer  :: DEpointer
    Real(WP) :: LX 
    Real(WP) :: LY 
    Real(WP) :: LZ 

    DEpointer = 0
    LX        = 0.0_WP
    LY        = 0.0_WP
    LZ        = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, DEPointer, LX, LY, LZ

    DEPointer = (DEPointer+1)/2
   
    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /=0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE122 : Invalid form - must be 0")')
      End If
    End If
 
    this%DE = DEPointer
    this%LX = LX
    this%LY = LY
    this%LZ = LZ

  End Subroutine getE122

  Subroutine makeE122(this, Precords)
  !! Translate this entity122_t type components into IGES P records

    Class(entity122_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer       :: nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 5 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DE)
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%LX
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%LY
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%LZ
    fields(5)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE122

  Subroutine initE122(this, DEP, DE, LX, LY, LZ, tmat)
  !! Initialize this entity122_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity122_t), Intent(INOUT) :: this
    
    Integer,            Intent(IN) :: DEP 
    Integer,            Intent(IN) :: DE
    Real(WP),           Intent(IN) :: LX 
    Real(WP),           Intent(IN) :: LY 
    Real(WP),           Intent(IN) :: LZ
 
    Integer,  Optional, Intent(IN) :: tmat

    this%entity_type = 122
    this%form        = 0
    this%DEP         = DEP
    this%DE          = DE
    this%LX          = LX 
    this%LY          = LY 
    this%LZ          = LZ 

    If (PRESENT(tmat)) this%tmat = tmat 

  End Subroutine initE122

  Subroutine outputE122(this, ounit)
  !! Output this entity122_t entity type components to a user output unit

    Class(entity122_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 122 - Tabulated Cylinder *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DE Index     : ", i0)') this%DE 
    Write(ounit,'(" DEpointer    : ", i0)') DE2PTR(this%DE) 
    Write(ounit,'(" LX           : ", ES15.6)') this%LX 
    Write(ounit,'(" LY           : ", ES15.6)') this%LY 
    Write(ounit,'(" LZ           : ", ES15.6)') this%LZ
 
  End Subroutine outputE122

  Subroutine copyToE122(this, that)
  !! Copy this entity to that entity

    Class(entity122_t), Intent(IN)  :: this
    Type(entity122_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE122

  Subroutine initTypeE122(this, that)
  !! Copy that entity to this entity

    Class(entity122_t), Intent(OUT) :: this
    Type(entity122_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE122
 
  Subroutine deallocE123(this)
  !! Deallocate and reset this entity123_t entity

    Class(entity123_t), Intent(INOUT) :: this

    this%entity_type        = -1 
    this%X                  = 0.0_WP 
    this%Y                  = 0.0_WP 
    this%Z                  = 0.0_WP 

  End Subroutine deallocE123

  Subroutine getE123(this, Precords, form)
  !! Translate IGES P records into this entity123_t entity type components

    Class(entity123_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET
    Real(WP) :: X
    Real(WP) :: Y
    Real(WP) :: Z

    X = 0.0_WP
    Y = 0.0_WP
    Z = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, X, Y, Z

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /=0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE123 : Invalid form - must be 0")')
      End If
    End If
 
    this%X                  = X
    this%Y                  = Y
    this%Z                  = Z

  End Subroutine getE123

  Subroutine makeE123(this, Precords)
  !! Translate this entity123_t type components into IGES P records

    Class(entity123_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 4 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Z
    fields(5)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE123

  Subroutine initE123(this, DEP, X, Y, Z, tmat)
  !! Initialize this entity123_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity123_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Real(WP),           Intent(IN)    :: X
    Real(WP),           Intent(IN)    :: Y
    Real(WP),           Intent(IN)    :: Z
    Integer,  Optional, Intent(IN)    :: tmat 

    this%entity_type = 123 
    this%DEP         = DEP

    this%X           = X
    this%Y           = Y
    this%Z           = Z

    If (PRESENT(tmat)) this%tmat = tmat


  End Subroutine initE123

  Subroutine outputE123(this, ounit)
  !! Output this entity123_t entity type components to a user output unit

    Class(entity123_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 123 - Direction *****'
    Write(ounit,*) ''


    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" X            : ", ES15.6)') this%X 
    Write(ounit,'(" Y            : ", ES15.6)') this%Y 
    Write(ounit,'(" Z            : ", ES15.6)') this%Z
 
  End Subroutine outputE123

  Subroutine copyToE123(this, that)
  !! Copy this entity to that entity

    Class(entity123_t), Intent(IN)  :: this
    Type(entity123_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE123

  Subroutine initTypeE123(this, that)
  !! Copy that entity to this entity

    Class(entity123_t), Intent(OUT) :: this
    Type(entity123_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE123
 
  Subroutine deallocE124(this)
  !! Deallocate and reset this entity124_t entity

    Class(entity124_t), Intent(INOUT) :: this

    this%entity_type = -1
    this%R           = 0.0_WP
    this%T           = 0.0_WP

  End Subroutine deallocE124

  Subroutine getE124(this, Precords, form)
  !! Translate IGES P records into this entity124_t entity type components

    Class(entity124_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET
    Real(WP) :: R11
    Real(WP) :: R12
    Real(WP) :: R13
    Real(WP) :: T1
    Real(WP) :: R21
    Real(WP) :: R22
    Real(WP) :: R23
    Real(WP) :: T2
    Real(WP) :: R31
    Real(WP) :: R32
    Real(WP) :: R33
    Real(WP) :: T3

    R11 = 0.0_WP
    R12 = 0.0_WP
    R13 = 0.0_WP
    R21 = 0.0_WP
    R22 = 0.0_WP
    R23 = 0.0_WP
    R31 = 0.0_WP
    R32 = 0.0_WP
    R33 = 0.0_WP
    T1  = 0.0_WP 
    T2  = 0.0_WP
    T3  = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string, *) ET, R11, R12, R13, T1, R21, R22, R23, T2, R31, R32, R33, T3

    this%entity_type        = ET
    this%form = 0
   
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form > 1) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE124 : Invalid form - must be 0 or 1")')
      End If
    End If 

    this%R(1,:) = [R11, R12, R13] 
    this%R(2,:) = [R21, R22, R23] 
    this%R(3,:) = [R31, R32, R33]
    this%T      = [T1, T2, T3]
 
  End Subroutine getE124

  Subroutine makeE124(this, Precords)
  !! Translate this entity124_t type components into IGES P records

    Class(entity124_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:)

    Integer       :: nfields
    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)
    Character(:),   Allocatable :: fmt

    fmt = REAL_FORMAT

    nfields = 13

    ALLOCATE(fields(13))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(1,1)
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(1,2)
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(1,3)
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%T(1)
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(2,1)
    fields(6)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(2,2)
    fields(7)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(2,3)
    fields(8)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%T(2)
    fields(9)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(3,1)
    fields(10)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(3,2)
    fields(11)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%R(3,3)
    fields(12)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%T(3)
    fields(13)%str = TRIM(ADJUSTL(buf))//";"
 
    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)
 
  End Subroutine makeE124

  Subroutine initE124(this, DEP, form, R, T, tmat) 
  !! Initialize this entity124_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity124_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: form
    Real(WP),           Intent(IN) :: R(3,3) 
    Real(WP),           Intent(IN) :: T(3) 
    Integer,  Optional, Intent(IN) :: tmat

    this%entity_type =  124
    this%DEP         = DEP
    this%form        = form
    Select Case(form)
      Case(0,1, 10,11,12)
        this%R           = R
        this%T           = T
        If (PRESENT(tmat)) this%tmat = tmat 
      Case Default
        Write(stderr,*) ' *** forIGES ERROR ***'
        ERROR STOP " initE124 : Invalid form - must be 0, 1, 10, 11,or 12"
    End Select 
 
  End Subroutine initE124

  Subroutine outputE124(this, ounit)
  !! Output this entity124_t entity type components to a user output unit

    Class(entity124_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 124 - Transformation Matrix ******'
    Write(ounit,*) ''


    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,*)''
    Write(ounit,'(" R            : ")')
    Write(ounit,*)''
    Write(ounit,'(1x,3ES15.6)') this%R(1,:)
    Write(ounit,'(1x,3ES15.6)') this%R(2,:)
    Write(ounit,'(1x,3ES15.6)') this%R(3,:)

    Write(ounit,*)''
    Write(ounit,'(" T            : ")')
    Write(ounit,*)''
    Write(ounit,'(1x,3ES15.6)') this%T(:)

  End Subroutine outputE124

  Subroutine copyToE124(this, that)
  !! Copy this entity to that entity

    Class(entity124_t), Intent(IN)  :: this
    Type(entity124_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE124

  Subroutine initTypeE124(this, that)
  !! Copy that entity to this entity

    Class(entity124_t), Intent(OUT) :: this
    Type(entity124_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE124
 
  Subroutine deallocE126(this)
  !! Deallocate and reset this entity126_t entity

    Class(entity126_t), Intent(INOUT) :: this

    this%entity_type        = -1 
    this%K                  = 0 
    this%M                  = 0 
    this%PROP1              = 0 
    this%PROP2              = 0 
    this%PROP3              = 0 
    this%PROP4              = 0 
    this%V                  = 0.0_WP 
    this%XNORM              = 0.0_WP 
    this%YNORM              = 0.0_WP 
    this%ZNORM              = 0.0_WP 
 
    If (ALLOCATED(this%T)) DEALLOCATE(this%T) 
    If (ALLOCATED(this%W)) DEALLOCATE(this%W) 
    If (ALLOCATED(this%X)) DEALLOCATE(this%X) 
    If (ALLOCATED(this%Y)) DEALLOCATE(this%Y) 
    If (ALLOCATED(this%Z)) DEALLOCATE(this%Z)

  End Subroutine deallocE126

  Subroutine getE126(this, Precords, form)
  !! Translate IGES P records into this entity126_t entity type components

    Class(entity126_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer               :: ET 
    Integer               :: K
    Integer               :: M
    Integer               :: PROP1 
    Integer               :: PROP2 
    Integer               :: PROP3 
    Integer               :: PROP4
    Real(WP), Allocatable :: T(:) 
    Real(WP), Allocatable :: W(:) 
    Real(WP), Allocatable :: X(:) 
    Real(WP), Allocatable :: Y(:) 
    Real(WP), Allocatable :: Z(:)
    Real(WP)              :: V(0:1)
    Real(WP)              :: XNORM 
    Real(WP)              :: YNORM 
    Real(WP)              :: ZNORM 

    Integer :: i,N, dum

    K     = 0
    M     = 0
    PROP1 = 0
    PROP2 = 0
    PROP3 = 0
    PROP4 = 0
    V     = 0.0_WP
    XNORM = 0.0_WP
    YNORM = 0.0_WP
    ZNORM = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string, *) ET, K, M, PROP1, PROP2, PROP3, PROP4
 
    N = 1 + K-M
    ALLOCATE(T(-M:N+M), SOURCE=0.0_WP)
    ALLOCATE(W(0:K),    SOURCE=0.0_WP) 
    ALLOCATE(X(0:K),    SOURCE=0.0_WP)
    ALLOCATE(Y(0:K),    SOURCE=0.0_WP)
    ALLOCATE(Z(0:K),    SOURCE=0.0_WP)

    Read(string, *) dum, dum, dum, dum, dum, dum, dum, T, W,                  &
                    (X(i), Y(i), Z(i), i=0,K), V, XNORM, YNORM, ZNORM 
     
    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form > 5) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE126 : Invalid form - must be 0-5 ")')
      End If
    End If 

    this%K      = K
    this%M      = M
    this%PROP1  = PROP1 
    this%PROP2  = PROP2 
    this%PROP3  = PROP3 
    this%PROP4  = PROP4
    this%V(0:1) = V(0:1)
    this%XNORM  = XNORM 
    this%YNORM  = YNORM 
    this%ZNORM  = ZNORM
 
    Call MOVE_ALLOC(T, this%T) 
    Call MOVE_ALLOC(W, this%W) 
    Call MOVE_ALLOC(X, this%X) 
    Call MOVE_ALLOC(Y, this%Y) 
    Call MOVE_ALLOC(Z, this%Z)

  End Subroutine getE126

  Subroutine makeE126(this, Precords)
  !! Translate this entity126_t type components into IGES P records

    Class(entity126_t),             Intent(IN)    :: this
    Character(80),     Allocatable, Intent(INOUT) :: Precords(:)

    Integer :: i, j, k, m, n, nfields
    Character(60) :: buf
     
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    Character(:), Allocatable :: fmt

    fmt = REAL_FORMAT

    k = this%K
    m = this%M
    nfields = 13 + SIZE(this%T) + 4*(k+1)

    ALLOCATE(fields(nfields))

!! Write type components into fields

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%K
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%M
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP1
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP2
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP3
    fields(6)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP4
    fields(7)%str = TRIM(ADJUSTL(buf))//","
    j = 7
    N = 1+k-m
    Do i=-m,n+m
      j = j+1      
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%T(i)
      fields(j)%str = TRIM(ADJUSTL(buf))//","
    End Do  
    Do i=0,k
      j = j+1      
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%W(i)
      fields(j)%str = TRIM(ADJUSTL(buf))//","
    End Do
    Do i=0,k
      j = j+1      
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%X(i)
      fields(j)%str = TRIM(ADJUSTL(buf))//","
      j = j+1      
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%Y(i)
      fields(j)%str = TRIM(ADJUSTL(buf))//","
      j = j+1      
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%Z(i)
      fields(j)%str = TRIM(ADJUSTL(buf))//","
    EndDo
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%V(0)
    fields(j)%str = TRIM(ADJUSTL(buf))//","
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%V(1)
    fields(j)%str = TRIM(ADJUSTL(buf))//","
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%XNORM
    fields(j)%str = TRIM(ADJUSTL(buf))//","
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%YNORM
    fields(j)%str = TRIM(ADJUSTL(buf))//","
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%ZNORM
    fields(j)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE126

  Subroutine outputE126(this, ounit)
  !! Output this entity126_t entity type components to a user output unit

    Class(entity126_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: iform

    iform = this%form
    
    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 126 - Rational NURBS Curve *****'
    Write(ounit,*) ''

    Select Case(iform)
      Case(0)
        Write(ounit,*) ' ** Defined by parameters **'
      Case(1)
        Write(ounit,*) ' ** Line **'
      Case(2)
        Write(ounit,*) ' ** Circular Arc **'
      Case(3)
        Write(ounit,*) ' ** Elliptical Arc **'
      Case(4)
        Write(ounit,*) ' ** Parabolic Arc **'
      Case(5)
        Write(ounit,*) ' ** Hyperbolic Arc **'
    End Select

    Write(ounit,*) ''
    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" K            : ", i0)') this%K
    Write(ounit,'(" M            : ", i0)') this%M
    Write(ounit,'(" PROP1        : ", i0)') this%PROP1
    Write(ounit,'(" PROP2        : ", i0)') this%PROP2
    Write(ounit,'(" PROP3        : ", i0)') this%PROP3
    Write(ounit,'(" PROP4        : ", i0)') this%PROP4
    Write(ounit,*)''
    Write(ounit,'(" T            : ")')
    Write(ounit,'(1x,5ES15.6)') this%T(:)
    Write(ounit,*)''
    Write(ounit,'(" W            : ")')
    Write(ounit,'(1x,5ES15.6)') this%W(:)
    Write(ounit,*)''
    Write(ounit,'(" X            : ")')
    Write(ounit,'(1x,5ES15.6)') this%X(:)
    Write(ounit,*)''
    Write(ounit,'(" Y            : ")')
    Write(ounit,'(1x,5ES15.6)') this%Y(:)
    Write(ounit,*)''
    Write(ounit,'(" Z            : ")')
    Write(ounit,'(1x,5ES15.6)') this%Z(:)
    Write(ounit,*)''
    Write(ounit,'(" V            : ", 2ES15.6)') this%V(:)
    Write(ounit,*)''
    Write(ounit,'(" XNORM        : ", ES16.6)') this%XNORM 
    Write(ounit,'(" YNORM        : ", ES16.6)') this%YNORM 
    Write(ounit,'(" ZNORM        : ", ES16.6)') this%ZNORM 

  End Subroutine outputE126

  Subroutine initE126(this, DEP, form, K, M, PROP1, PROP2, PROP3, PROP4, &
                      T, W, X, Y, Z, V, XNORM, YNORM, ZNORM, tmat)
  !! Initialize this entity126_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity126_t), Intent(INOUT) :: this
    Integer,            Intent(IN)    :: DEP 
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: K
    Integer,            Intent(IN)    :: M
    Integer,            Intent(IN)    :: PROP1 
    Integer,            Intent(IN)    :: PROP2 
    Integer,            Intent(IN)    :: PROP3 
    Integer,            Intent(IN)    :: PROP4
    Real(WP),           Intent(IN)    :: T(-M:) 
    Real(WP),           Intent(IN)    :: W(0:) 
    Real(WP),           Intent(IN)    :: X(0:) 
    Real(WP),           Intent(IN)    :: Y(0:) 
    Real(WP),           Intent(IN)    :: Z(0:)
    Real(WP),           Intent(IN)    :: V(0:1)
    Real(WP),           Intent(IN)    :: XNORM 
    Real(WP),           Intent(IN)    :: YNORM 
    Real(WP),           Intent(IN)    :: ZNORM

    Integer, Optional,  Intent(IN)    :: tmat 

    Integer :: N 

    this%entity_type = 126
    this%DEP         = DEP
    this%form        = form
    If (form >=0 .AND. form <=5) Then 
      this%K           = K
      this%M           = M
      this%PROP1       = PROP1 
      this%PROP2       = PROP2 
      this%PROP3       = PROP3 
      this%PROP4       = PROP4 
      this%XNORM       = XNORM
      this%YNORM       = YNORM
      this%ZNORM       = ZNORM
      this%V           = V
 
      N = 1 + K - M

      ALLOCATE(this%T(-M:N+K), SOURCE = T) 
      ALLOCATE(this%W(0:K),    SOURCE = W) 
      ALLOCATE(this%X(0:K),    SOURCE = X) 
      ALLOCATE(this%Y(0:K),    SOURCE = Y) 
      ALLOCATE(this%Z(0:K),    SOURCE = Z)
 
      If (PRESENT(tmat)) this%tmat = tmat 
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE126 : Invalid form must be 0-5"
    End If

  End Subroutine initE126

  Subroutine copyToE126(this, that)
  !! Copy this entity to that entity

    Class(entity126_t), Intent(IN)  :: this
    Type(entity126_t),  Intent(OUT) :: that

    that = this
 
  End Subroutine copyToE126

  Subroutine initTypeE126(this, that)
  !! Copy that entity to this entity

    Class(entity126_t), Intent(OUT) :: this
    Type(entity126_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE126
 
  Subroutine deallocE128(this)
  !! Deallocate and reset this entity128_t entity

    Class(entity128_t), Intent(INOUT) :: this

    this%entity_type        = -1 
    this%K1                 = 0 
    this%K2                 = 0 
    this%M1                 = 0 
    this%M2                 = 0 
    this%PROP1              = 0 
    this%PROP2              = 0 
    this%PROP3              = 0 
    this%PROP4              = 0 
    this%PROP5              = 0 
    this%U                  = 0.0_WP 
    this%V                  = 0.0_WP 
 
    If (ALLOCATED(this%S)) DEALLOCATE(this%S) 
    If (ALLOCATED(this%T)) DEALLOCATE(this%T) 
    If (ALLOCATED(this%W)) DEALLOCATE(this%W) 
    If (ALLOCATED(this%X)) DEALLOCATE(this%X) 
    If (ALLOCATED(this%Y)) DEALLOCATE(this%Y) 
    If (ALLOCATED(this%Z)) DEALLOCATE(this%Z)

  End Subroutine deallocE128

  Subroutine getE128(this, Precords, form)
  !! Translate IGES P records into this entity128_t entity type components

    Class(entity128_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer               :: ET 
    Integer               :: K1, K2
    Integer               :: M1, M2
    Integer               :: PROP1 
    Integer               :: PROP2 
    Integer               :: PROP3 
    Integer               :: PROP4
    Integer               :: PROP5
    Real(WP), Allocatable :: S(:) 
    Real(WP), Allocatable :: T(:) 
    Real(WP), Allocatable :: W(:,:) 
    Real(WP), Allocatable :: X(:,:) 
    Real(WP), Allocatable :: Y(:,:) 
    Real(WP), Allocatable :: Z(:,:)
    Real(WP)              :: U(0:1)
    Real(WP)              :: V(0:1)

    Integer :: i, j, N1, N2, dum

    K1    = 0
    K2    = 0
    M1    = 0
    M2    = 0
    PROP1 = 0
    PROP2 = 0
    PROP3 = 0
    PROP4 = 0
    PROP5 = 0
    U     = 0.0_WP
    V     = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string, *) ET, K1, K2, M1, M2, PROP1, PROP2, PROP3, PROP4, PROP5
 
    N1 = 1 + K1-M1
    N2 = 1 + K2-M2
    ALLOCATE(S(-M1:N1+M1), SOURCE=0.0_WP)
    ALLOCATE(T(-M2:N2+M2), SOURCE=0.0_WP)
    ALLOCATE(W(0:K1,0:K2), SOURCE=0.0_WP) 
    ALLOCATE(X(0:K1,0:K2), SOURCE=0.0_WP)
    ALLOCATE(Y(0:K1,0:K2), SOURCE=0.0_WP)
    ALLOCATE(Z(0:K1,0:K2), SOURCE=0.0_WP)

    Read(string, *) dum, dum, dum, dum, dum, dum, dum, dum, dum, dum, S,  T,   &
                    W, ((X(i,j), Y(i,j), Z(i,j), i=0,K1), j=0,K2), U, V
     
    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form > 9) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE128 : Invalid form - must be 0-9 ")')
      End If
    End If 

    this%K1      = K1
    this%K2      = K2
    this%M1      = M1
    this%M2      = M2
    this%PROP1   = PROP1 
    this%PROP2   = PROP2 
    this%PROP3   = PROP3 
    this%PROP4   = PROP4
    this%PROP5   = PROP5
    this%U(0:1)  = U(0:1)
    this%V(0:1)  = V(0:1)
 
    Call MOVE_ALLOC(S, this%S) 
    Call MOVE_ALLOC(T, this%T) 
    Call MOVE_ALLOC(W, this%W) 
    Call MOVE_ALLOC(X, this%X) 
    Call MOVE_ALLOC(Y, this%Y) 
    Call MOVE_ALLOC(Z, this%Z)

  End Subroutine getE128

  Subroutine makeE128(this, Precords)
  !! Translate this entity128_t type components into IGES P records

    Class(entity128_t),             Intent(IN)    :: this
    Character(80),     Allocatable, Intent(INOUT) :: Precords(:)

    Integer :: i, j, k1, k2, l, m1, m2, n1, n2, nfields 
    Character(60) :: buf
     
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)
    Character(:),   Allocatable :: fmt

    fmt = REAL_FORMAT

    k1 = this%K1
    k2 = this%K2
    m1 = this%M1
    m2 = this%M2

    nfields = 14 + SIZE(this%S) + SIZE(this%T) + 4*(k1+1)*(k2+1) 

    ALLOCATE(fields(nfields))

!! Write type components into fields
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%K1
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%K2
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%M1
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%M2
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP1
    fields(6)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP2
    fields(7)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP3
    fields(8)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP4
    fields(9)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PROP5
    fields(10)%str = TRIM(ADJUSTL(buf))//","
    j = 10
    N1 = 1+k1-m1
    N2 = 1+k2-m2
    Do i=-m1,n1+m1
      j = j+1      
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%S(i)
      fields(j)%str = TRIM(ADJUSTL(buf))//","
    End Do  
    Do i=-m2,n2+m2
      j = j+1      
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%T(i)
      fields(j)%str = TRIM(ADJUSTL(buf))//","
    End Do
    Do l=0,k2  
      Do i=0,k1
        j = j+1      
        buf = REPEAT(" ",60)
        Write(buf,fmt) this%W(i,l)
        fields(j)%str = TRIM(ADJUSTL(buf))//","
      End Do
    End Do
    Do l=0,k2
      Do i=0,k1
        j = j+1      
        buf = REPEAT(" ",60)
        Write(buf,fmt) this%X(i,l)
        fields(j)%str = TRIM(ADJUSTL(buf))//","
        j = j+1      
        buf = REPEAT(" ",60)
        Write(buf,fmt) this%Y(i,l)
        fields(j)%str = TRIM(ADJUSTL(buf))//","
        j = j+1      
        buf = REPEAT(" ",60)
        Write(buf,fmt) this%Z(i,l)
       fields(j)%str = TRIM(ADJUSTL(buf))//","
      EndDo
    End Do
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%U(0)
    fields(j)%str = TRIM(ADJUSTL(buf))//","
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%U(1)
    fields(j)%str = TRIM(ADJUSTL(buf))//","
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%V(0)
    fields(j)%str = TRIM(ADJUSTL(buf))//","
    j = j+1 
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%V(1)
    fields(j)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE128

  Subroutine initE128(this, DEP, form, K1, K2, M1, M2, PROP1, PROP2, PROP3,   & 
                      PROP4, PROP5, S, T, W, X, Y, Z, U, V, tmat) 
  !! Initialize this entity128_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity128_t), Intent(INOUT) :: this
    Integer,            Intent(IN)    :: DEP 
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: K1
    Integer,            Intent(IN)    :: K2
    Integer,            Intent(IN)    :: M1
    Integer,            Intent(IN)    :: M2
    Integer,            Intent(IN)    :: PROP1 
    Integer,            Intent(IN)    :: PROP2 
    Integer,            Intent(IN)    :: PROP3 
    Integer,            Intent(IN)    :: PROP4
    Integer,            Intent(IN)    :: PROP5
    Real(WP),           Intent(IN)    :: S(-M1:) 
    Real(WP),           Intent(IN)    :: T(-M2:) 
    Real(WP),           Intent(IN)    :: W(0:,0:) 
    Real(WP),           Intent(IN)    :: X(0:,0:) 
    Real(WP),           Intent(IN)    :: Y(0:,0:) 
    Real(WP),           Intent(IN)    :: Z(0:,0:)
    Real(WP),           Intent(IN)    :: U(0:1)
    Real(WP),           Intent(IN)    :: V(0:1)

    Integer, Optional,  Intent(IN)    :: tmat 
 
    Integer :: N1, N2

    this%entity_type = 128
    this%DEP         = DEP
    this%form        = form
    If (form>=0 .AND. form <=9) Then
      this%K1          = K1
      this%K2          = K2
      this%M1          = M1
      this%M2          = M2
      this%PROP1       = PROP1 
      this%PROP2       = PROP2 
      this%PROP3       = PROP3 
      this%PROP4       = PROP4 
      this%PROP5       = PROP5 
      this%U           = U
      this%V           = V
 
      N1 = 1 + K1 - M1
      N2 = 1 + K2 - M2

      ALLOCATE(this%S(-M1:N1+K1), SOURCE = S) 
      ALLOCATE(this%T(-M2:N2+K2), SOURCE = T) 
      ALLOCATE(this%W(0:K1,0:K2), SOURCE = W) 
      ALLOCATE(this%X(0:K1,0:K2), SOURCE = X) 
      ALLOCATE(this%Y(0:K1,0:K2), SOURCE = Y) 
      ALLOCATE(this%Z(0:K1,0:K2), SOURCE = Z)
 
      If (PRESENT(tmat)) this%tmat = tmat
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE128 : Invalid form - must be 0-9"
    End If
 
  End Subroutine initE128

  Subroutine outputE128(this, ounit)
  !! Output this entity128_t entity type components to a user output unit

    Class(entity128_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: i, iform

    iform = this%form
    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 128 - Rational NURBS Surface ******'
    Write(ounit,*) ''
  
    Select Case(iform)
      Case(0)
        Write(ounit,*) ' ** Defined by parameters **' 
      Case(1)
        Write(ounit,*) ' ** Plane ' 
      Case(2)
        Write(ounit,*) ' ** Right Circular Cylinder **' 
      Case(3)
        Write(ounit,*) ' ** Cone **' 
      Case(4)
        Write(ounit,*) ' ** Sphere **' 
      Case(5)
        Write(ounit,*) ' ** Torus **' 
      Case(6)
        Write(ounit,*) ' ** Surface of Revolution **' 
      Case(7)
        Write(ounit,*) ' ** Tabulated Cylinder **' 
      Case(8)
        Write(ounit,*) ' ** Ruled Surface **' 
      Case(9)
        Write(ounit,*) ' ** General Quadric Surface **' 
    End Select

    Write(ounit,*) ''
    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" K1           : ", i0)') this%K1
    Write(ounit,'(" K2           : ", i0)') this%K2
    Write(ounit,'(" M1           : ", i0)') this%M1
    Write(ounit,'(" M2           : ", i0)') this%M2
    Write(ounit,'(" PROP1        : ", i0)') this%PROP1
    Write(ounit,'(" PROP2        : ", i0)') this%PROP2
    Write(ounit,'(" PROP3        : ", i0)') this%PROP3
    Write(ounit,'(" PROP4        : ", i0)') this%PROP4
    Write(ounit,'(" PROP5        : ", i0)') this%PROP5
    Write(ounit,*)''
    Write(ounit,'(" S              : ")')
    Write(ounit,'(1x,5ES15.6)') this%S(:)
    Write(ounit,*)''
    Write(ounit,'(" T              : ")')
    Write(ounit,'(1x,5ES15.6)') this%T(:)
    Do i =0,this%K2
      Write(ounit,*)''
      Write(ounit,'(" W(:,",i0,")  : ")') i
      Write(ounit,'(1x,5ES15.6)') this%W(:,i)
    End Do
      Write(ounit,*)''
    Do i =0,this%K2
      Write(ounit,'(" X(:,",i0,")  : ")') i
      Write(ounit,'(1x,5ES15.6)') this%X(:,i)
    End Do
      Write(ounit,*)''
    Do i =0,this%K2
      Write(ounit,*)''
      Write(ounit,'(" Y(:,",i0,")  : ")') i
      Write(ounit,'(1x,5ES15.6)') this%Y(:,i)
    End Do
      Write(ounit,*)''
    Do i =0,this%K2
      Write(ounit,*)''
      Write(ounit,'(" Z(:,",i0,")  : ")') i
      Write(ounit,'(1x,5ES15.6)') this%Z(:,i)
    End Do
    Write(ounit,*)''
    Write(ounit,'(" U            : ", 2ES15.6)') this%U(:)
    Write(ounit,*)''
    Write(ounit,'(" V            : ", 2ES15.6)') this%V(:)

  End Subroutine outputE128

  Subroutine copyToE128(this, that)
  !! Copy this entity to that entity

    Class(entity128_t), Intent(IN)  :: this
    Type(entity128_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE128

  Subroutine initTypeE128(this, that)
  !! Copy that entity to this entity

    Class(entity128_t), Intent(OUT):: this
    Type(entity128_t),  Intent(IN) :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE128 

  Subroutine deallocE130(this)
  !! Deallocate and reset this entity130_t entity

    Class(entity130_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%DE1         = 0 
    this%FLAG        = 0 
    this%DE2         = 0 
    this%NDIM        = 0 
    this%PTYPE       = 0 
    this%D1          = 0.0_WP 
    this%TD1         = 0.0_WP 
    this%D2          = 0.0_WP 
    this%TD2         = 0.0_WP 
    this%VX          = 0.0_WP 
    this%VY          = 0.0_WP 
    this%VZ          = 0.0_WP 
    this%TT1         = 0.0_WP 
    this%TT2         = 0.0_WP
 
  End Subroutine deallocE130

  Subroutine getE130(this, Precords, form)
  !! Translate IGES P records into this entity130_t entity type components

    Class(entity130_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET 
    Integer  :: DE1
    Integer  :: FLAG 
    Integer  :: DE2
    Integer  :: NDIM 
    Integer  :: PTYPE
    Real(WP) :: D1 
    Real(WP) :: TD1 
    Real(WP) :: D2 
    Real(WP) :: TD2 
    Real(WP) :: VX 
    Real(WP) :: VY 
    Real(WP) :: VZ 
    Real(WP) :: TT1 
    Real(WP) :: TT2 

    DE1   = 0
    FLAG  = 0
    DE2   = 0
    NDIM  = 0
    PTYPE = 0
    D1    = 0.0_WP
    TD1   = 0.0_WP
    D2    = 0.0_WP
    TD2   = 0.0_WP
    VX    = 0.0_WP
    VY    = 0.0_WP
    VZ    = 0.0_WP
    TT1   = 0.0_WP
    TT2   = 0.0_WP
  
    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, DE1, FLAG, DE2, NDIM, PTYPE, D1, TD1, D2, TD2, VX,   &
                   VY, VZ, TT1, TT2

    DE1 = (DE1+1)/2
    DE2 = (DE2+1)/2

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE130 : Invalid form - must be 0 ")')
      End If
    End If 

    this%DE1   = DE1
    this%FLAG  = FLAG
    If (FLAG == 3) Then
      this%DE2 = DE2
    Else
      this%DE2 = 0
    End If 
    this%NDIM  = NDIM 
    this%PTYPE = PTYPE
    this%D1    = D1 
    this%TD1   = TD1 
    this%D2    = D2 
    this%TD2   = TD2 
    this%VX    = VX 
    this%VY    = VY 
    this%VZ    = VZ 
    this%TT1   = TT1 
    this%TT2   = TT2 

  End Subroutine getE130

  Subroutine makeE130(this, Precords)
  !! Translate this entity130_t type components into IGES P records

    Class(entity130_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 15 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DE1)
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%FLAG
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    If (this%FLAG == 3) Then
      Write(buf,'(i0)') DE2PTR(this%DE2)
    Else
      Write(buf,'(i0)') this%DE2
    End If
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%NDIM
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PTYPE
    fields(6)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%D1
    fields(7)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TD1
    fields(8)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%D2
    fields(9)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TD2
    fields(10)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%VX
    fields(11)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%VY
    fields(12)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%VZ
    fields(13)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TT1
    fields(14)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%TT2
    fields(15)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE130

  Subroutine initE130(this, DEP, DE1, FLAG, DE2, NDIM, PTYPE, D1, TD1, D2,    &
                      TD2, VX, VY, VZ, TT1, TT2, tmat) 
  !! Initialize this entity130_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity130_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: DE1 
    Integer,            Intent(IN) :: FLAG 
    Integer,            Intent(IN) :: DE2 
    Integer,            Intent(IN) :: NDIM 
    Integer,            Intent(IN) :: PTYPE
    Real(WP),           Intent(IN) :: D1 
    Real(WP),           Intent(IN) :: TD1 
    Real(WP),           Intent(IN) :: D2 
    Real(WP),           Intent(IN) :: TD2 
    Real(WP),           Intent(IN) :: VX 
    Real(WP),           Intent(IN) :: VY 
    Real(WP),           Intent(IN) :: VZ 
    Real(WP),           Intent(IN) :: TT1 
    Real(WP),           Intent(IN) :: TT2 
    Integer, Optional,  Intent(IN) :: tmat 

    this%entity_type = 128 
    this%DEP         = DEP
    this%form        = 0 

    this%DE1   = DE1
    this%FLAG  = FLAG
    If (FLAG == 3) Then
      this%DE2 = DE2
    Else
      this%DE2 = 0
    End If 
    this%NDIM  = NDIM 
    this%PTYPE = PTYPE
    this%D1    = D1 
    this%TD1   = TD1 
    this%D2    = D2 
    this%TD2   = TD2 
    this%VX    = VX 
    this%VY    = VY 
    this%VZ    = VZ 
    this%TT1   = TT1 
    this%TT2   = TT2

    If (PRESENT(tmat)) this%tmat = tmat
 
  End Subroutine initE130

  Subroutine outputE130(this, ounit)
  !! Output this entity130_t entity type components to a user output unit

    Class(entity130_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 130 - Offset Curve *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DE1 Index    : ", i0)') this%DE1 
    Write(ounit,'(" DE1 Seq. No. : ", i0)') DE2PTR(this%DE1) 
    Write(ounit,'(" FLAG         : ", i0)') this%FLAG 
    Write(ounit,'(" DE2 Index    : ", i0)') this%DE2 
    Write(ounit,'(" DE2 Seq. No. : ", i0)') DE2PTR(this%DE2) 
    Write(ounit,'(" NDIM         : ", i0)') this%NDIM 
    Write(ounit,'(" PTYPE        : ", i0)') this%PTYPE
    Write(ounit,'(" D1           : ", ES15.6)') this%D1
    Write(ounit,'(" TD1          : ", ES15.6)') this%TD1
    Write(ounit,'(" D2           : ", ES15.6)') this%D2
    Write(ounit,'(" TD2          : ", ES15.6)') this%TD2
    Write(ounit,'(" VX           : ", ES15.6)') this%VX
    Write(ounit,'(" VY           : ", ES15.6)') this%VY
    Write(ounit,'(" VZ           : ", ES15.6)') this%VZ
    Write(ounit,'(" TT1          : ", ES15.6)') this%TT1
    Write(ounit,'(" TT2          : ", ES15.6)') this%TT2
 
  End Subroutine outputE130

  Subroutine copyToE130(this, that)
  !! Copy this entity to that entity

    Class(entity130_t), Intent(IN)  :: this
    Type(entity130_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE130

  Subroutine initTypeE130(this, that)
  !! Copy that entity to this entity

    Class(entity130_t), Intent(OUT):: this
    Type(entity130_t),  Intent(IN) :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE130

  Subroutine deallocE140(this)
  !! Deallocate and reset this entity140_t entity

    Class(entity140_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%NX          = 0.0_WP 
    this%NY          = 0.0_WP 
    this%NZ          = 0.0_WP 
    this%D           = 0.0_WP 
    this%DE          = 0 

  End Subroutine deallocE140

  Subroutine getE140(this, Precords, form)
  !! Translate IGES P records into this entity140_t entity type components

    Class(entity140_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET 
    Real(WP) :: NX
    Real(WP) :: NY
    Real(WP) :: NZ
    Real(WP) :: D 
    Integer  :: DE

    NX  = 0.0_WP
    NY  = 0.0_WP
    NZ  = 0.0_WP
    D   = 0.0_WP
    DE  = 0

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, NX, NY, NZ, D, DE

    DE = (DE+1)/2

    this%entity_type = ET
    this%form        = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE140 : Invalid form - must be 0 ")')
      End If
    End If 

    this%NX = NX
    this%NY = NY
    this%NZ = NZ
    this%D  = D 
    this%DE = DE

  End Subroutine getE140

  Subroutine makeE140(this, Precords)
  !! Translate this entity140_t type components into IGES P records

    Class(entity140_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields 
    Character(60) :: buf

    Character(:),   Allocatable :: fmt
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 6 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%NX
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%NY
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%NZ
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%D
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DE)
    fields(6)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE140

  Subroutine initE140(this, DEP, NX, NY, NZ, D, DE, tmat) 
  !! Initialize this entity140_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity140_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Real(WP),           Intent(IN) :: NX
    Real(WP),           Intent(IN) :: NY
    Real(WP),           Intent(IN) :: NZ
    Real(WP),           Intent(IN) :: D 
    Integer,            Intent(IN) :: DE
    Integer, Optional,  Intent(IN) :: tmat 

    this%entity_type = 140 
    this%DEP         = DEP
    this%form        = 0 
    this%NX          = NX
    this%NX          = NY
    this%NZ          = NZ
    this%D           = D 
    this%DE          = DE 
    If (PRESENT(tmat)) this%tmat = tmat
 
  End Subroutine initE140

  Subroutine outputE140(this, ounit)
  !! Output this entity140_t entity type components to a user output unit

    Class(entity140_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 140 - Offset Surface *****'
    Write(ounit,*) ''


    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'("  NX          : ", ES15.6)') this%NX 
    Write(ounit,'("  NY          : ", ES15.6)') this%NY 
    Write(ounit,'("  NZ          : ", ES15.6)') this%NZ 
    Write(ounit,'("  D           : ", ES15.6)') this%D 
    Write(ounit,'("  DE Index    : ", i0)') this%DE 
    Write(ounit,'("  DE Seq. No. : ", i0)') DE2PTR(this%DE) 


  End Subroutine outputE140

  Subroutine copyToE140(this, that)
  !! Copy this entity to that entity

    Class(entity140_t), Intent(IN)  :: this
    Type(entity140_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE140
 
  Subroutine initTypeE140(this, that)
  !! Copy that entity to this entity

    Class(entity140_t), Intent(OUT) :: this
    Type(entity140_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE140
 
  Subroutine deallocE141(this)
  !! Deallocate and reset this entity141_t entity

    Class(entity141_t), Intent(INOUT) :: this

    Integer :: i

    this%entity_type = -1 
    this%TYPE        = 0 
    this%PREF        = 0 
    this%SPTR        = 0 

    If (ALLOCATED(this%CRVPT)) DEALLOCATE(this%CRVPT)
    If (ALLOCATED(this%SENSE)) DEALLOCATE(this%SENSE)
    If (ALLOCATED(this%K))     DEALLOCATE(this%K)
    Do i=1, this%N
      If (ALLOCATED(this%PSCPT(i)%vals)) DEALLOCATE(this%PSCPT(i)%vals)
    End Do 
    If (ALLOCATED(this%PSCPT)) DEALLOCATE(this%PSCPT)
    this%N                  = 0 
  
  End Subroutine deallocE141

  Subroutine getE141(this, Precords, form)
  !! Translate IGES P records into this entity141_t entity type components

    Class(entity141_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable     :: string

    Integer                       :: ET 
    Integer                       :: TYPE
    Integer                       :: PREF 
    Integer                       :: SPTR 
    Integer                       :: N 
    Integer,          Allocatable :: CRVPT(:)
    Integer,          Allocatable :: SENSE(:)
    Integer,          Allocatable :: K(:)
    Type(intArray_t), Allocatable :: PSCPT(:)

    Integer :: i, j, is, NP
    Type(string_t),   Allocatable :: fields(:)

    TYPE = 0
    PREF = 0
    SPTR = 0
    N    = 0
 
    Call PentityToString(Precords, string, this%DEP)
    
    Read(string, *) ET, TYPE, PREF, SPTR, N

    SPTR = (SPTR+1)/2
    ALLOCATE(CRVPT(N), SOURCE=0)
    ALLOCATE(SENSE(N), SOURCE=0)
    ALLOCATE(K(N),     SOURCE=0)
    ALLOCATE(PSCPT(N))
    ALLOCATE(this%PSCPT(N))
   
    Call getEntityFields(string, fields)

    is = 5
    Do i = 1,N 
      Read(fields(is)%str, *) CRVPT(i)
      is = is+1
      Read(fields(is)%str,*)  SENSE(i)
      is = is+1
      Read(fields(is)%str,*)  K(i)
      NP = K(i)
      If (NP > 0) Then
        ALLOCATE(PSCPT(i)%vals(NP), SOURCE=0)
        Do j = 1,NP
          is = is+1
          Read(fields(is)%str,*) PSCPT(i)%vals(j)
        End Do
      Else
        ALLOCATE(PSCPT(i)%vals(0))
      End If
    End Do

    CRVPT(1:) = (CRVPT(1:)+1)/2
    Do i=1,N
      If (K(i) > 0) Then
        PSCPT(i)%vals(1:) = (PSCPT(i)%vals(1:) + 1)/2
      End If
    End Do

    this%entity_type  = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE141 : Invalid form - must be 0 ")')
      End If
    End If 


    this%TYPE = TYPE
    this%PREF = PREF
    this%SPTR = SPTR
    this%N    = N

    Call MOVE_ALLOC(CRVPT, this%CRVPT)
    Call MOVE_ALLOC(SENSE, this%SENSE)
    Call MOVE_ALLOC(K,     this%K)
    Do i=1,N
      Call MOVE_ALLOC(PSCPT(i)%vals, this%PSCPT(i)%vals)
    End Do 

  End Subroutine getE141

  Subroutine makeE141(this, Precords)
  !! Translate this entity141_t type components into IGES P records

    Class(entity141_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, j, id, is, N, NP, nfields 
    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)


    N = this%N
     
    nfields = 5 + 3*N

    Do i=1,N
      nfields = nfields + this%K(i) 
    EndDo

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%TYPE
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PREF
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%SPTR)
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(5)%str = TRIM(ADJUSTL(buf))//","

    is = 5
    Do i=1,N
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%CRVPT(i))
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') this%SENSE(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') this%K(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      NP = this%K(i)
      If (NP > 0) Then
        Do j=1,NP
          is = is+1
          buf = REPEAT(" ",60)
          Write(buf,'(i0)') DE2PTR(this%PSCPT(i)%vals(j))
          fields(is)%str = TRIM(ADJUSTL(buf))//","
        End Do
      End If
    End Do      
    id = INDEX(fields(nfields)%str, ",")
    fields(nfields)%str(id:id) = ";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE141

  Subroutine initE141(this, DEP, TYPE, PREF, SPTR, N, CRVPT, SENSE, K, PSCPT, &
                      tmat) 
  !! Initialize this entity141_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity141_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: TYPE
    Integer,            Intent(IN) :: PREF 
    Integer,            Intent(IN) :: SPTR 
    Integer,            Intent(IN) :: N
    Integer,            Intent(IN) :: CRVPT(N) 
    Integer,            Intent(IN) :: SENSE(N) 
    Integer,            Intent(IN) :: K(N)
    Type(intArray_t),   Intent(IN) :: PSCPT(N) 
    Integer, Optional,  Intent(IN) :: tmat 

    Integer :: i

    this%entity_type = 141
    this%DEP         = DEP
    this%form        = 0

    this%TYPE = TYPE
    this%PREF = PREF
    this%SPTR = SPTR
    this%N    = N

    this%CRVPT  = CRVPT
    this%SENSE  = SENSE
    this%K      = K
    Do i=1,N
      this%PSCPT(i)%vals = PSCPT(i)%vals
    End Do 
    If (PRESENT(tmat)) this%tmat = tmat 

  End Subroutine initE141

  Subroutine outputE141(this,ounit)
  !! Output this entity141_t entity type components to a user output unit

    Class(entity141_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: i

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 141 - Boundary *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form   : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" TYPE          : ",i0)')  this%TYPE
    Write(ounit,'(" PREF          : ",i0)')  this%PREF
    Write(ounit,'(" SPTR Index    : ",i0)')  this%SPTR
    Write(ounit,'(" SPTR Seq. No. : ",i0)')  DE2PTR(this%SPTR)
    Write(ounit,'(" N             : ",i0)')  this%N
    Write(ounit,*) ''
    Write(ounit,'(" CRVPT Index    : ")')
    Write(ounit,*) ''
    Write(ounit,'(1x,8i10)') this%CRVPT(:)
    Write(ounit,'(" CRVPT Seq. No. : ")')
    Write(ounit,*) ''
    Write(ounit,'(1x,8i10)') DE2PTR(this%CRVPT(:))
    Write(ounit,*) ''
    Write(ounit,'(" SENSE        : ")')
    Write(ounit,*) ''
    Write(ounit,'(1x,8i10)') this%SENSE(:)
    Write(ounit,*) ''
    Write(ounit,'(" K            : ")')
    Write(ounit,*) ''
    Write(ounit,'(1x,8i10)') this%K(:)
    Write(ounit,*) ''

    Do i=1, this%N
      If (this%K(i) > 0) Then
        Write(ounit,*) ''
        Write(ounit,'(" PSCPT(:,",i0,") Index    : ")') i
        Write(ounit,'(1X, 8I10)') this%PSCPT(i)%vals(:)
      End If
    End Do 
    Write(ounit,*) ''

    Do i=1, this%N
      If (this%K(i) > 0) Then
        Write(ounit,*) ''
        Write(ounit,'(" PSCPT(:,",i0,") Seq. No. : ")') i
        Write(ounit,'(1X, 8I10)') DE2PTR(this%PSCPT(i)%vals(:))
      End If
    End Do 

  End Subroutine outputE141

  Subroutine copyToE141(this, that)
  !! Copy this entity to that entity

    Class(entity141_t), Intent(IN)  :: this
    Type(entity141_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE141

  Subroutine initTypeE141(this, that)
  !! Copy that entity to this entity

    Class(entity141_t), Intent(OUT) :: this
    Type(entity141_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE141
 
  Subroutine deallocE142(this)
  !! Deallocate and reset this entity142_t entity

    Class(entity142_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%CRTN        = 0 
    this%SPTR        = 0 
    this%BPTR        = 0 
    this%CPTR        = 0 
    this%PREF        = 0 
 
  End Subroutine deallocE142

  Subroutine getE142(this, Precords, form)
  !! Translate IGES P records into this entity142_t entity type components

    Class(entity142_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer :: ET 
    Integer :: CRTN
    Integer :: SPTR 
    Integer :: BPTR 
    Integer :: CPTR 
    Integer :: PREF 

    CRTN = 0
    SPTR = 0
    BPTR = 0
    CPTR = 0
    PREF = 0

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, CRTN, SPTR, BPTR, CPTR, PREF
    
    SPTR = (SPTR+1)/2
    BPTR = (BPTR+1)/2
    CPTR = (CPTR+1)/2

    this%entity_type = ET
    this%form        = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE142 : Invalid form - must be 0 ")')
      End If
    End If 

    this%CRTN = CRTN
    this%SPTR = SPTR
    this%BPTR = BPTR
    this%CPTR = CPTR
    this%PREF = PREF
 
  End Subroutine getE142

  Subroutine makeE142(this, Precords)
  !! Translate this entity142_t type components into IGES P records

    Class(entity142_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields 
    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)


    nfields = 6

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%CRTN
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%SPTR)
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%BPTR)
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%CPTR)
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%PREF
    fields(6)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(fields, Precords)

  End Subroutine makeE142

  Subroutine initE142(this, DEP, CRTN, SPTR, BPTR, CPTR, PREF, tmat) 
  !! Initialize this entity142_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity142_t), Intent(INOUT) :: this
    
    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: CRTN 
    Integer,            Intent(IN)    :: SPTR 
    Integer,            Intent(IN)    :: BPTR 
    Integer,            Intent(IN)    :: CPTR 
    Integer,            Intent(IN)    :: PREF 
    Integer, Optional,  Intent(IN)    :: tmat 

    this%entity_type = 142

    this%DEP  = DEP
    this%CRTN = CRTN
    this%SPTR = SPTR
    this%BPTR = BPTR
    this%CPTR = CPTR
    this%PREF = PREF

    If (PRESENT(tmat)) this%tmat = tmat 

  End Subroutine initE142

  Subroutine outputE142(this, ounit)
  !! Output this entity142_t entity type components to a user output unit

    Class(entity142_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 142 - Curve On Parametric Surface ******'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type   : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form   : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" CRTN          : ", i0)') this%CRTN          
    Write(ounit,'(" SPTR Index    : ", i0)') this%SPTR          
    Write(ounit,'(" SPTR Seq. No. : ", i0)') DE2PTR(this%SPTR)          
    Write(ounit,'(" BPTR Index    : ", i0)') this%BPTR         
    Write(ounit,'(" BPTR Seq. No. : ", i0)') DE2PTR(this%BPTR)          
    Write(ounit,'(" CPTR Index    : ", i0)') this%CPTR          
    Write(ounit,'(" CPTR Seq. No. : ", i0)') DE2PTR(this%CPTR)          
    Write(ounit,'(" PREF          : ", i0)') this%PREF          
          
  End Subroutine outputE142

  Subroutine copyToE142(this, that)
  !! Copy this entity to that entity

    Class(entity142_t), Intent(IN)  :: this
    Type(entity142_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE142

  Subroutine initTypeE142(this, that)
  !! Copy that entity to this entity

    Class(entity142_t), Intent(OUT) :: this
    Type(entity142_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE142
 
  Subroutine deallocE143(this)
  !! Deallocate and reset this entity143_t entity

    Class(entity143_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%TYPE        = 0 
    this%SPTR        = 0 
    this%N           = 0 

    If (ALLOCATED(this%BDPT)) DEALLOCATE(this%BDPT) 

  End Subroutine deallocE143

  Subroutine getE143(this, Precords, form)
  !! Translate IGES P records into this entity143_t entity type components

    Class(entity143_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer              :: ET 
    Integer              :: TYPE
    Integer              :: SPTR 
    Integer              :: N 
    Integer, Allocatable :: BDPT(:) 

    Integer :: dum
 
    TYPE = 0
    SPTR = 0
    N    = 0

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, TYPE, SPTR, N
    ALLOCATE(BDPT(N), SOURCE=0)
    Read(string,*) dum, dum, dum, dum, BDPT

    SPTR = (SPTR+1)/2
    BDPT = (BDPT+1)/2

    this%entity_type  = ET
    this%form         = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE143 : Invalid form - must be 0 ")')
      End If
    End If 

    this%TYPE = TYPE
    this%SPTR = SPTR 
    this%N    = N

    Call MOVE_ALLOC(BDPT, this%BDPT) 

  End Subroutine getE143

  Subroutine makeE143(this, Precords)
  !! Translate this entity143_t type components into IGES P records

    Class(entity143_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer       :: i, is, nfields 
    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)


    nfields = 4 + this%N

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%TYPE
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%SPTR)
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N 
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    is = 4
    Do i=1, this%N-1
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%BDPT(i))
      fields(is)%str = TRIM(ADJUSTL(buf))//","
    End Do
    is = is + 1
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%BDPT(this%N))
    fields(is)%str = TRIM(ADJUSTL(buf))//";"
 

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE143

  Subroutine initE143(this, DEP, TYPE, SPTR, N, BDPT, tmat)
  !! Initialize this entity143_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity143_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: TYPE
    Integer,            Intent(IN)    :: SPTR 
    Integer,            Intent(IN)    :: N
    Integer,            Intent(IN)    :: BDPT(N) 
 
    Integer, Optional,  Intent(IN)    :: tmat 

    this%entity_type = 143 
    this%DEP         = DEP
    this%form        = 0 

    this%TYPE = TYPE
    this%SPTR = SPTR 
    this%N    = N

    this%BDPT = BDPT 

    If (PRESENT(tmat)) this%tmat = tmat
 
  End Subroutine initE143

  Subroutine outputE143(this, ounit)
  !! Output this entity143_t entity type components to a user output unit

    Class(entity143_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 143 - Bounded Surface *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type   : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form   : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" TYPE          : ", i0)') this%TYPE          
    Write(ounit,'(" SPTR Index        : ", i0)') this%SPTR          
    Write(ounit,'(" SPTR Seq. No. : ", i0)') DE2PTR(this%SPTR)          
    Write(ounit,'(" N             : ", i0)') this%N
    Write(ounit,*) ''
    Write(ounit,'(" BDPT Index    : ")')
    Write(ounit,'(1x, 8i10)') this%BDPT(:)
    Write(ounit,*) ''
    Write(ounit,'(" BDPT Seq. No. : ")')
    Write(ounit,'(1x, 8i10)') DE2PTR(this%BDPT(:))
          
          
  End Subroutine outputE143

  Subroutine copyToE143(this, that)
  !! Copy this entity to that entity

    Class(entity143_t), Intent(IN)  :: this
    Type(entity143_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE143

  Subroutine initTypeE143(this, that)
  !! Copy that entity to this entity

    Class(entity143_t), Intent(OUT) :: this
    Type(entity143_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE143
 
  Subroutine deallocE144(this)
  !! Deallocate and reset this entity144_t entity

    Class(entity144_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%PTS         = 0 
    this%N1          = 0 
    this%PTO         = 0 
    If (this%N2 > 0) Then
      If (ALLOCATED(this%PTI)) DEALLOCATE(this%PTI)
    End If
    this%N2          = 0 
 
  End Subroutine deallocE144

  Subroutine getE144(this, Precords, form)
  !! Translate IGES P records into this entity144_t entity type components

    Class(entity144_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer              :: ET 
    Integer              :: PTS
    Integer              :: N1 
    Integer              :: N2 
    Integer              :: PTO 
    Integer, Allocatable :: PTI(:) 

    Integer :: dum

    PTS = 0
    N1  = 0
    N2  = 0
    PTO = 0

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, PTS, N1, N2, PTO

    If (N2 > 0) Then
      ALLOCATE(PTI(N2), SOURCE=0)
      Read(string,*) dum, dum, dum, dum, dum, PTI
    End If
    
    PTS    = (PTS+1)/2 
    PTO    = (PTO+1)/2
    If (N2 > 0) Then
      PTI(1:) = (PTI(1:)+1)/2
    End If
    
    this%entity_type = ET
    this%form        = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE144 : Invalid form - must be 0 ")')
      End If
    End If 


    this%PTS  = PTS
    this%N1   = N1 
    this%N2   = N2
    this%PTO  = PTO
    If (N2 > 0) Then
      Call MOVE_ALLOC(PTI, this%PTI)
    End If 

  End Subroutine getE144

  Subroutine makeE144(this, Precords)
  !! Translate this entity144_t type components into IGES P records

    Class(entity144_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, is, N2, nfields

    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    N2 = this%N2

    nfields = 5 + N2 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%PTS)
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N1
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N2
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%PTO)
    If (N2 > 0) Then
      fields(5)%str = TRIM(ADJUSTL(buf))//","
    Else
      fields(5)%str = TRIM(ADJUSTL(buf))//";"
    End If

    If (N2 > 0) Then
      is = 5
      Do i=1,N2
        is = is + 1
        buf = REPEAT(" ",60)
        Write(buf,'(i0)') DE2PTR(this%PTI(i))
        If (i == N2) Then
          fields(is)%str = TRIM(ADJUSTL(buf))//";"
        Else
          fields(is)%str = TRIM(ADJUSTL(buf))//","
        End If
      End Do
    End If 

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE144

  Subroutine initE144(this, DEP, PTS, N1, N2, PTO, PTI, tmat) 
  !! Initialize this entity144_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity144_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: PTS 
    Integer,            Intent(IN)    :: N1 
    Integer,            Intent(IN)    :: N2 
    Integer,            Intent(IN)    :: PTO
    Integer, Optional,  Intent(IN)    :: PTI(N2) 
    Integer, Optional,  Intent(IN)    :: tmat 

    this%entity_type = 144
    this%DEP         = DEP
    this%form        = 0
 
    this%PTS  = PTS
    this%N1   = N1 
    this%N2   = N2
    this%PTO  = PTO
    If (N2 > 0) Then
      If (PRESENT(PTI)) Then
         this%PTI = PTI
      Else
         Write(stderr,*) ' *** forIGES ERROR ***' 
         Write(stderr,'(" initE144 : PTI array not present for N>0) ")')
      End If 
    End If
    If (PRESENT(tmat)) this%tmat = tmat
 
  End Subroutine initE144

  Subroutine outputE144(this, ounit)
  !! Output this entity144_t entity type components to a user output unit

    Class(entity144_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 144 - Trimmed Parametric Surface *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" PTS Index    : ", i0)') this%PTS         
    Write(ounit,'(" PTS Seq. No. : ", i0)') DE2PTR(this%PTS)         
    Write(ounit,'(" N1           : ", i0)') this%N1          
    Write(ounit,'(" N2           : ", i0)') this%N2          
    Write(ounit,'(" PTO Index    : ", i0)') this%PTO
    Write(ounit,'(" PTO Seq. No. : ", i0)') DE2PTR(this%PTO)
    If (this%N2 > 0) Then
      Write(ounit,*) ''
      Write(ounit,'(" PTI Index    : ")')
      Write(ounit,*) ''
      Write(ounit,'(1x, 8i10)') this%PTI(:)
      Write(ounit,*) ''
      Write(ounit,'(" PTI Seq. No. : ")')
      Write(ounit,*) ''
      Write(ounit,'(1x, 8i10)') DE2PTR(this%PTI(:))
    Else
      Write(ounit,*) ''
      Write(ounit,'(" NO PTI CURVES : ")')
      Write(ounit,*) ''
    End If 
          
  End Subroutine outputE144

  Subroutine copyToE144(this, that)
  !! Copy this entity to that entity
 
    Class(entity144_t), Intent(IN)  :: this
    Type(entity144_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE144

  Subroutine initTypeE144(this, that)
  !! Copy that entity to this entity

    Class(entity144_t), Intent(OUT) :: this
    Type(entity144_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE144
 
  Subroutine deallocE190(this)
  !! Deallocate and reset this entity190_t entity

    Class(entity190_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%DELOC       = 0 
    this%DENRML      = 0 
    this%DEREFD      = 0 

  End Subroutine deallocE190

  Subroutine getE190(this, Precords, form)
  !! Translate IGES P records into this entity190_t entity type components

    Class(entity190_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer :: ET 
    Integer :: DELOC
    Integer :: DENRML
    Integer :: DEREFD

    DELOC  = 0
    DENRML = 0
    DEREFD = 0

    Call PentityToString(Precords, string, this%DEP)

    this%form = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form >1) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE190 : Invalid form - must be 0 or 1")')
      End If
    End If 

    If (this%form == 0) Then
      Read(string,*) ET, DELOC, DENRML
      DEREFD = 0
    Else 
      Read(string,*) ET, DELOC, DENRML, DEREFD
    End If 
    DELOC  = (DELOC+1)/2 
    DENRML = (DENRML+1)/2 
    If (this%form /= 0) DEREFD = (DEREFD+1)/2 

    this%entity_type = ET

    this%DELOC  = DELOC
    this%DENRML = DENRML
    this%DEREFD = DEREFD

  End Subroutine getE190

  Subroutine makeE190(this, Precords)
  !! Translate this entity190_t type components into IGES P records

    Class(entity190_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields

    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)


    nfields = 4

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DELOC) 
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DENRML) 
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DEREFD) 
    fields(4)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE190

  Subroutine initE190(this, form, DEP, DELOC, DENRML, DEREFD, tmat) 
  !! Initialize this entity190_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity190_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: DELOC
    Integer,            Intent(IN)    :: DENRML
    Integer,            Intent(IN)    :: DEREFD
    Integer, Optional,  Intent(IN)    :: tmat 

    this%entity_type = 190
    this%DEP         = DEP
    this%form        = form 
    If (form >=0 .AND. form <=1) Then 
      this%DELOC       = DELOC
      this%DENRML      = DENRML
      If (form == 1) Then
        this%DEREFD  = DEREFD
      Else
        this%DEREFD  = 0
      End If
      If (PRESENT(tmat)) this%tmat = tmat 
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE190 : Invalid form - form must be 0 or 1"
    End If 

  End Subroutine initE190

  Subroutine outputE190(this, ounit)
  !! Output this entity190_t entity type components to a user output unit

    Class(entity190_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: iform

    iform = this%form

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 190 - Plane Surface *****'
    Write(ounit,*) ''

    If (iform == 0) Then
      Write(ounit,*)' ** Unparameterized Surface **'
    Else If (iform == 1) Then
      Write(ounit,*)' ** Parameterized Surface **'
    End If

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DELOC Index     : ", i0)') this%DELOC
    Write(ounit,'(" DELOC Seq. No.  : ", i0)') DE2PTR(this%DELOC)
    Write(ounit,'(" DENRML Index    : ", i0)') this%DENRML
    Write(ounit,'(" DENRML Seq. No. : ", i0)') DE2PTR(this%DENRML)
    Write(ounit,'(" DEREFD Index    : ", i0)') this%DEREFD
    Write(ounit,'(" DEREFD Seq. No. : ", i0)') DE2PTR(this%DEREFD)

  End Subroutine outputE190

  Subroutine copyToE190(this, that)
  !! Copy this entity to that entity

    Class(entity190_t), Intent(IN)  :: this
    Type(entity190_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE190

  Subroutine initTypeE190(this, that)
  !! Copy that entity to this entity

    Class(entity190_t), Intent(OUT) :: this
    Type(entity190_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE190
 
  Subroutine deallocE192(this)
  !! Deallocate and reset this entity192_t entity

    Class(entity192_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%DELOC       = 0 
    this%DEAXIS      = 0 
    this%RADIUS      = 0.0_WP 
    this%DEREFD      = 0 

  End Subroutine deallocE192

  Subroutine getE192(this, Precords, form)
  !! Translate IGES P records into this entity192_t entity type components

    Class(entity192_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET 
    Integer  :: DELOC
    Integer  :: DEAXIS
    Real(WP) :: RADIUS
    Integer  :: DEREFD

    DELOC  = 0
    DEAXIS = 0
    RADIUS = 0.0_WP
    DEREFD = 0

    Call PentityToString(Precords, string, this%DEP)

    this%form = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form >1) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE192 : Invalid form - must be 0 or 1")')
      End If
    End If 

    If (this%form == 0) Then
      Read(string,*) ET, DELOC, DEAXIS, RADIUS
      DEREFD = 0
    Else 
      Read(string,*) ET, DELOC, DEAXIS, RADIUS, DEREFD 
    End If 
    DELOC  = (DELOC+1)/2 
    DEAXIS = (DEAXIS+1)/2 
    If (this%form /= 0) DEREFD = (DEREFD+1)/2 

    this%entity_type = ET

    this%DELOC       = DELOC
    this%DEAXIS      = DEAXIS
    this%RADIUS      = RADIUS 
    this%DEREFD      = DEREFD

  End Subroutine getE192

  Subroutine makeE192(this, Precords)
  !! Translate this entity192_t type components into IGES P records

    Class(entity192_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields

    Character(60)             :: buf
    Character(:), Allocatable :: fmt 

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 5

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DELOC) 
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DEAXIS) 
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%RADIUS 
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DEREFD) 
    fields(5)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE192

  Subroutine initE192(this, form, DEP, DELOC, DEAXIS, RADIUS, DEREFD, tmat)
  !! Initialize this entity192_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity192_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: DELOC
    Integer,            Intent(IN)    :: DEAXIS
    Real(WP),           Intent(IN)    :: RADIUS
    Integer,            Intent(IN)    :: DEREFD
    Integer, Optional,  Intent(IN)    :: tmat 

    this%entity_type = 192
    this%DEP         = DEP
    this%form        = form

    If (form >=0 .AND. form <=1) Then 
      this%DELOC       = DELOC
      this%DEAXIS      = DEAXIS
      this%RADIUS      = RADIUS 
      If (form ==1) Then
        this%DEREFD      = DEREFD
      Else
        this%DEREFD      = 0
      End If
      If (PRESENT(tmat)) this%tmat = tmat 
    Else
      Write (stderr, *) ''
      ERROR STOP " initE192 : Invalid form - must be 0 or 1"
    End If

  End Subroutine initE192

  Subroutine outputE192(this, ounit)
  !! Output this entity192_t entity type components to a user output unit

    Class(entity192_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: iform

    iform = this%form

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 192 - Right Circular Cylinder Surface *****'
    Write(ounit,*) ''

    If (iform == 0) Then
      Write(ounit,*)' ** Unparameterized Surface **'
    Else If (iform == 1) Then
      Write(ounit,*)' ** Parameterized Surface **'
    End If

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DELOC  Index    : ", i0)') this%DELOC
    Write(ounit,'(" DELOC  Seq. No. : ", i0)') DE2PTR(this%DELOC)
    Write(ounit,'(" DEAXIS Index    : ", i0)') this%DEAXIS
    Write(ounit,'(" DEAXIS Seq. No. : ", i0)') DE2PTR(this%DEAXIS)
    Write(ounit,'(" RADIUS          : ", ES15.6)') this%RADIUS
    Write(ounit,'(" DEREFD Index    : ", i0)') this%DEREFD
    Write(ounit,'(" DEREFD Seq. No.: ",  i0)') DE2PTR(this%DEREFD)
 
  End Subroutine outputE192

  Subroutine copyToE192(this, that)
  !! Copy this entity to that entity

    Class(entity192_t), Intent(IN)  :: this
    Type(entity192_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE192

  Subroutine initTypeE192(this, that)
  !! Copy that entity to this entity

    Class(entity192_t), Intent(OUT) :: this
    Type(entity192_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE192

  Subroutine deallocE194(this)
  !! Deallocate and reset this entity194_t entity

    Class(entity194_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%DELOC       = 0 
    this%DEAXIS      = 0 
    this%RADIUS      = 0.0_WP 
    this%SANGLE      = 0.0_WP 
    this%DEREFD      = 0 

  End Subroutine deallocE194

  Subroutine getE194(this, Precords, form)
  !! Translate IGES P records into this entity194_t entity type components

    Class(entity194_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET 
    Integer  :: DELOC
    Integer  :: DEAXIS
    Real(WP) :: RADIUS
    Real(WP) :: SANGLE 
    Integer  :: DEREFD

    DELOC  = 0
    DEAXIS = 0
    RADIUS = 0.0_WP
    SANGLE = 0.0_WP
    DEREFD = 0

    Call PentityToString(Precords, string, this%DEP)

    this%form = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form >1) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE194 : Invalid form - must be 0 or 1")')
      End If
    End If 

    If (this%form == 0) Then
      Read(string,*) ET, DELOC, DEAXIS, RADIUS, SANGLE
      DEREFD = 0
    Else
      Read(string,*) ET, DELOC, DEAXIS, RADIUS, SANGLE, DEREFD
    End If 
 
    DELOC  = (DELOC+1)/2 
    DEAXIS = (DEAXIS+1)/2 
    If (this%form /= 0) DEREFD = (DEREFD+1)/2 

    this%entity_type = ET

    this%DELOC  = DELOC
    this%DEAXIS = DEAXIS
    this%RADIUS = RADIUS 
    this%SANGLE = SANGLE 
    this%DEREFD = DEREFD

  End Subroutine getE194

  Subroutine makeE194(this, Precords)
  !! Translate this entity194_t type components into IGES P records

    Class(entity194_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields

    Character(60)             :: buf
    Character(:), Allocatable ::fmt

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 6

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DELOC) 
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DEAXIS) 
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%RADIUS 
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%SANGLE 
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DEREFD) 
    fields(6)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE194

  Subroutine initE194(this, DEP, form, DELOC, DEAXIS, RADIUS, SANGLE, DEREFD,  &
                      tmat) 
  !! Initialize this entity194_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity194_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: DELOC
    Integer,            Intent(IN)    :: DEAXIS
    Real(WP),           Intent(IN)    :: RADIUS
    Real(WP),           Intent(IN)    :: SANGLE 
    Integer,            Intent(IN)    :: DEREFD
    Integer, Optional,  Intent(IN)    :: tmat 

    this%entity_type = 194
    this%DEP         = DEP
    this%form        = form
    If (form >=0 .AND. form <=1) Then  
      this%DELOC       = DELOC
      this%DEAXIS      = DEAXIS
      this%RADIUS      = RADIUS 
      this%SANGLE      = SANGLE 
      If (form == 1) Then
        this%DEREFD    = DEREFD
      Else
        this%DEREFD    = 0
      End If
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE194 : Invalid form - must be 0 or 1"
    End If

    If (PRESENT(tmat)) this%tmat = tmat 

  End Subroutine initE194

  Subroutine outputE194(this, ounit)
  !! Output this entity194_t entity type components to a user output unit

    Class(entity194_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit


    Integer :: iform

    iform = this%form

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 194 - Right Circular Conical Surface *****'
    Write(ounit,*) ''

    If (iform == 0) Then
      Write(ounit,*)' ** Unparameterized Surface **'
    Else If (iform == 1) Then
      Write(ounit,*)' ** Parameterized Surface **'
    End If

    Write(ounit,*) ''
    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DELOC  Index    : ", i0)') this%DELOC
    Write(ounit,'(" DELOC  Seq. No. : ", i0)') DE2PTR(this%DELOC)
    Write(ounit,'(" DEAXIS Index    : ", i0)') this%DEAXIS
    Write(ounit,'(" DEAXIS Seq. No. : ", i0)') DE2PTR(this%DEAXIS)
    Write(ounit,'(" RADIUS          : ", ES15.6)') this%RADIUS
    Write(ounit,'(" SANGLE          : ", ES15.6)') this%SANGLE
    Write(ounit,'(" DEREFD Index    : ", i0)') this%DEREFD
    Write(ounit,'(" DEREFD Seq. No. : ", i0)') DE2PTR(this%DEREFD)
 
  End Subroutine outputE194

  Subroutine copyToE194(this, that)
  !! Copy this entity to that entity

    Class(entity194_t), Intent(IN)  :: this
    Type(entity194_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE194

  Subroutine initTypeE194(this, that)
  !! Copy that entity to this entity

    Class(entity194_t), Intent(OUT) :: this
    Type(entity194_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE194
 
  Subroutine deallocE196(this)
  !! Deallocate and reset this entity196_t entity

    Class(entity196_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%DELOC       = 0 
    this%RADIUS      = 0.0_WP 
    this%DEAXIS      = 0 
    this%DEREFD      = 0 

  End Subroutine deallocE196

  Subroutine getE196(this, Precords, form)
  !! Translate IGES P records into this entity196_t entity type components

    Class(entity196_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET 
    Integer  :: DELOC
    Real(WP) :: RADIUS
    Integer  :: DEAXIS
    Integer  :: DEREFD

    DELOC  = 0
    RADIUS = 0.0_WP
    DEAXIS = 0
    DEREFD = 0

    Call PentityToString(Precords, string, this%DEP)

    this%form = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form >1) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE196 : Invalid form - must be 0 or 1")')
      End If
    End If 

    If (this%form == 0) Then
      Read(string,*) ET, DELOC, RADIUS
      DEAXIS = 0
      DEREFD = 0
    Else 
      Read(string,*) ET, DELOC, RADIUS, DEAXIS, DEREFD 
    End If
 
    DELOC  = (DELOC+1)/2
    If (this%form /= 0) Then 
      DEAXIS = (DEAXIS+1)/2 
      DEREFD = (DEREFD+1)/2 
    End If
    this%entity_type = ET

    this%DELOC  = DELOC
    this%RADIUS = RADIUS 
    this%DEAXIS = DEAXIS
    this%DEREFD = DEREFD

  End Subroutine getE196

  Subroutine makeE196(this, Precords)
  !! Translate this entity196_t type components into IGES P records

    Class(entity196_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields

    Character(60)             :: buf
    Character(:), Allocatable :: fmt

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 5

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DELOC) 
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%RADIUS 
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DEAXIS) 
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DEREFD) 
    fields(5)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE196

  Subroutine initE196(this, DEP, form, DELOC, RADIUS, DEAXIS, DEREFD, tmat)
  !! Initialize this entity196_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity196_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: DELOC
    Real(WP),           Intent(IN)    :: RADIUS
    Integer,            Intent(IN)    :: DEAXIS
    Integer,            Intent(IN)    :: DEREFD
    Integer, Optional,  Intent(IN)    :: tmat 

    this%entity_type = 196
    this%DEP         = DEP
    this%form        = form
    If (form>=0 .AND. form<=1) Then 
      this%DELOC       = DELOC
      this%RADIUS      = RADIUS
      If (form == 1) Then 
        this%DEAXIS    = DEAXIS
        this%DEREFD    = DEREFD
      Else
        this%DEAXIS = 0
        this%DEREFD = 0
      End If
      If (PRESENT(tmat)) this%tmat = tmat 
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE196 : Invalid form - must be 0 or 1"
    End If

  End Subroutine initE196

  Subroutine outputE196(this, ounit)
  !! Output this entity196_t entity type components to a user output unit

    Class(entity196_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit


    Integer :: iform

    iform = this%form

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 196 - Spherical Surface *****'
    Write(ounit,*) ''

    If (iform == 0) Then
      Write(ounit,*)' ** Unparameterized Surface **'
    Else If (iform == 1) Then
      Write(ounit,*)' ** Parameterized Surface **'
    End If

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DELOC  Index    : ", i0)') this%DELOC
    Write(ounit,'(" DELOC  Seq. No. : ", i0)') DE2PTR(this%DELOC)
    Write(ounit,'(" RADIUS          : ", ES15.6)') this%RADIUS
    Write(ounit,'(" DEAXIS Index    : ", i0)') this%DEAXIS
    Write(ounit,'(" DEAXIS Seq. No. : ", i0)') DE2PTR(this%DEAXIS)
    Write(ounit,'(" DEREFD Index    : ", i0)') this%DEREFD
    Write(ounit,'(" DEREFD Seq. No. : ", i0)') DE2PTR(this%DEREFD)

  End Subroutine outputE196

  Subroutine copyToE196(this, that)
  !! Copy this entity to that entity

    Class(entity196_t), Intent(IN)  :: this
    Type(entity196_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE196

  Subroutine initTypeE196(this, that)
  !! Copy that entity to this entity

    Class(entity196_t), Intent(OUT) :: this
    Type(entity196_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE196
 
  Subroutine deallocE198(this)
  !! Deallocate and reset this entity198_t entity

    Class(entity198_t), Intent(INOUT) :: this

    this%entity_type = -1
    this%form        = 0 
    this%DELOC       = 0 
    this%DEAXIS      = 0 
    this%MAJRAD      = 0.0_WP 
    this%MINRAD      = 0.0_WP 
    this%DEREFD      = 0.0

  End Subroutine deallocE198

  Subroutine getE198(this, Precords, form)
  !! Translate IGES P records into this entity198_t entity type components

    Class(entity198_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET 
    Integer  :: DELOC
    Integer  :: DEAXIS
    Integer  :: DEREFD
    Real(WP) :: MAJRAD 
    Real(WP) :: MINRAD

    DELOC  = 0
    DEAXIS = 0
    DEREFD = 0
    MAJRAD = 0.0_WP
    MINRAD = 0.0_WP 

    Call PentityToString(Precords, string, this%DEP)

    this%form = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form >1) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE198 : Invalid form - must be 0 or 1")')
      End If
    End If 

    If (this%form == 0) Then
      Read(string,*) ET, DELOC, DEAXIS, MAJRAD, MINRAD
      DEREFD = 0
    Else
      Read(string,*) ET, DELOC, DEAXIS, MAJRAD, MINRAD, DEREFD
    End IF
 
    DELOC  = (DELOC+1)/2 
    DEAXIS = (DEAXIS+1)/2 
    If (this%form /= 0) DEREFD = (DEREFD+1)/2

    this%entity_type = ET

    this%DELOC  = DELOC
    this%DEAXIS = DEAXIS
    this%MAJRAD = MAJRAD
    this%MINRAD = MINRAD
    this%DEREFD = DEREFD

  End Subroutine getE198

  Subroutine makeE198(this, Precords)
  !! Translate this entity198_t type components into IGES P records

    Class(entity198_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields

    Character(60)             :: buf
    Character(:), Allocatable :: fmt

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)


    fmt = REAL_FORMAT

    If (this%form == 1) Then
      nfields = 6
    Else
      nfields = 5
    End If

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DELOC) 
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DEAXIS) 
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%MAJRAD 
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%MINRAD
    If (this%form == 1) Then
      fields(5)%str = TRIM(ADJUSTL(buf))//","
    Else
      fields(5)%str = TRIM(ADJUSTL(buf))//";"
    End If
    If (this%form == 1) Then 
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%DEREFD) 
      fields(6)%str = TRIM(ADJUSTL(buf))//";"
    End If

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE198

  Subroutine initE198(this, DEP, form, DELOC, DEAXIS, MAJRAD, MINRAD, DEREFD, &
                      tmat)
  !! Initialize this entity198_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity198_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: DELOC
    Integer,            Intent(IN)    :: DEAXIS
    Real(WP),           Intent(IN)    :: MAJRAD 
    Real(WP),           Intent(IN)    :: MINRAD 
    Integer,            Intent(IN)    :: DEREFD
    Integer, Optional,  Intent(IN)    :: tmat 

    this%entity_type = 198
    this%DEP         = DEP
    this%form        = form
    If (form >-0 .AND. form <=1) Then
      this%DELOC       = DELOC
      this%DEAXIS      = DEAXIS
      this%MAJRAD      = MAJRAD 
      this%MINRAD      = MINRAD
      If (form == 1) Then 
        this%DEREFD    = DEREFD
      Else
        this%DEREFD    = 0
      End If
      If (PRESENT(tmat)) this%tmat = tmat 
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE198 - Invalid form - must be 0 or 1"
    End If

  End Subroutine initE198

  Subroutine outputE198(this, ounit)
  !! Output this entity198_t entity type components to a user output unit

    Class(entity198_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: iform
  
    iform = this%form
    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 198 - Toroidal Surface *****'
    Write(ounit,*) ''

    If (iform == 0) Then
      Write(ounit,*)' ** Unparameterized Surface **'
    Else If (iform == 1) Then
      Write(ounit,*)' ** Parameterized Surface **'
    End If

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DELOC  Index    : ", i0)') this%DELOC
    Write(ounit,'(" DELOC  Seq. No. : ", i0)') DE2PTR(this%DELOC)
    Write(ounit,'(" DEAXIS Index    : ", i0)') this%DEAXIS
    Write(ounit,'(" DEAXIS Seq. No. : ", i0)') DE2PTR(this%DEAXIS)
    Write(ounit,'(" MAJRAD          : ", ES15.6)') this%MAJRAD
    Write(ounit,'(" MINRAD          : ", ES15.6)') this%MINRAD
    Write(ounit,'(" DEREFD Index    : ", i0)') this%DEREFD
    Write(ounit,'(" DEREFD Seq. No. : ", i0)') DE2PTR(this%DEREFD)

  End Subroutine outputE198
 
  Subroutine copyToE198(this, that)
  !! Copy this entity to that entity

    Class(entity198_t), Intent(IN)  :: this
    Type(entity198_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE198

  Subroutine initTypeE198(this, that)
  !! Copy that entity to this entity

    Class(entity198_t), Intent(OUT) :: this
    Type(entity198_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE198
 
  Subroutine deallocE212(this)
  !! Deallocate and reset this entity212_t entity

    Class(entity212_t), Intent(INOUT) :: this

    Integer :: i

    this%entity_type = -1 
 
    If (ALLOCATED(this%NC)) DEALLOCATE(this%NC)
    If (ALLOCATED(this%WT)) DEALLOCATE(this%WT)
    If (ALLOCATED(this%HT)) DEALLOCATE(this%HT)
    If (ALLOCATED(this%FC)) DEALLOCATE(this%FC)
    If (ALLOCATED(this%SL)) DEALLOCATE(this%SL)
    If (ALLOCATED(this%A))  DEALLOCATE(this%A)
    If (ALLOCATED(this%M))  DEALLOCATE(this%M)
    If (ALLOCATED(this%VH)) DEALLOCATE(this%VH)
    If (ALLOCATED(this%XS)) DEALLOCATE(this%XS)
    If (ALLOCATED(this%YS)) DEALLOCATE(this%YS)
    If (ALLOCATED(this%ZS)) DEALLOCATE(this%ZS)

    Do i=1,this%NS
      If (ALLOCATED(this%TEXT(i)%str)) DEALLOCATE(this%TEXT(i)%str)
    End Do

    If (ALLOCATED(this%TEXT)) DEALLOCATE(this%TEXT)
    this%NS          = 0 
 
  End Subroutine deallocE212

  Subroutine getE212(this, Precords, form)
  !! Translate IGES P records into this entity212_t entity type components

    Class(entity212_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form

    Character(:),   Allocatable :: string

    Integer                     :: ET 
    Integer                     :: NS
    Integer,        Allocatable :: NC(:) 
    Real(WP),       Allocatable :: WT(:) 
    Real(WP),       Allocatable :: HT(:)
    Integer,        Allocatable :: FC(:) 
    Real(WP),       Allocatable :: SL(:)
    Real(WP),       Allocatable :: A(:)
    Integer,        Allocatable :: M(:) 
    Integer,        Allocatable :: VH(:) 
    Real(WP),       Allocatable :: XS(:)
    Real(WP),       Allocatable :: YS(:)
    Real(WP),       Allocatable :: ZS(:)
    Type(string_t), Allocatable :: TEXT(:)

    Integer       :: i, j, ls, ifl, ih
    Character(80) :: buf

    Type(string_t), Allocatable :: fields(:)

    NS = 0
    this%form = 0
    If (PRESENT(form)) Then
      Select Case(form)
        Case(0,1,2,3,4,5,6,7,8, 100,101,102, 105)
          this%form = form
        Case Default
          this%form = form
          Write(stderr, *) ''
          Write(stderr, *) ' *** forIGES WARNING ***'
          Write(stderr, '(" getE212 : Invalid form - must be 0-8, 100-102 or 105")')
      End Select
    End If 

    Call PentityToString(Precords, string, this%DEP)

    Call filterHstrings(string)

    Call getEntityFields(string,fields)

    Read(string,*) ET, NS 

    ALLOCATE(NC(NS), SOURCE=0)
    ALLOCATE(WT(NS), SOURCE=0.0_WP) 
    ALLOCATE(HT(NS), SOURCE=0.0_WP) 
    ALLOCATE(FC(NS), SOURCE=0)
    ALLOCATE(SL(NS), SOURCE=0.0_WP) 
    ALLOCATE(A(NS),  SOURCE=0.0_WP)
    ALLOCATE(M(NS),  SOURCE=0)
    ALLOCATE(VH(NS), SOURCE=0)
    ALLOCATE(XS(NS), SOURCE=0.0_WP)
    ALLOCATE(YS(NS), SOURCE=0.0_WP)
    ALLOCATE(ZS(NS), SOURCE=0.0_WP)
    ALLOCATE(text(NS))

    ifl = 3
    Do i=1,NS
      Read(fields(ifl)%str,    *) NC(i)
      Read(fields(ifl+1)%str,  *) WT(i)
      Read(fields(ifl+2)%str,  *) HT(i)
      Read(fields(ifl+3)%str,  *) FC(i)
      Read(fields(ifl+4)%str,  *) SL(i)
      Read(fields(ifl+5)%str,  *) A(i)
      Read(fields(ifl+6)%str,  *) M(i)
      Read(fields(ifl+7)%str,  *) VH(i)
      Read(fields(ifl+8)%str,  *) XS(i)
      Read(fields(ifl+9)%str,  *) YS(i)
      Read(fields(ifl+10)%str, *) ZS(i)
      buf = REPEAT(" ",80)
      Read(fields(ifl+11)%str, '(A)') buf 
      ih = INDEX(buf, "H") + 1
      TEXT(i)%str = TRIM(ADJUSTL(buf(ih:(ih+NC(i)-1))))
      ifl = ifl+12 
    End Do 
    Do i=1,NS
      ls = LEN(TEXT(i)%str)
      Do j=1,ls
        If (TEXT(i)%str(j:j) == "@") TEXT(i)%str(j:j) = ","
      End Do
    End Do
     
    this%entity_type = ET

    this%NS          = NS
 
    Call MOVE_ALLOC(NC, this%NC)
    Call MOVE_ALLOC(WT, this%WT)
    Call MOVE_ALLOC(HT, this%HT)
    Call MOVE_ALLOC(FC, this%FC)
    Call MOVE_ALLOC(SL, this%SL)
    Call MOVE_ALLOC(A,  this%A)
    Call MOVE_ALLOC(M,  this%M)
    Call MOVE_ALLOC(VH, this%VH)
    Call MOVE_ALLOC(XS, this%XS)
    Call MOVE_ALLOC(YS, this%YS)
    Call MOVE_ALLOC(ZS, this%ZS)

    ALLOCATE(this%TEXT(NS))

    Do i=1,NS
      this%TEXT(i)%str = TEXT(i)%str
    End Do
 
  End Subroutine getE212

  Subroutine makeE212(this, Precords)
  !! Translate this entity212_t type components into IGES P records

    Class(entity212_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, is, ns, nc, nfields

    Character(60) :: buf
    Character(:), Allocatable :: fmt
    Character(:), Allocatable :: nh
 
    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    NS      = this%NS
    nfields = 2 + 12*NS 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%NS
    fields(2)%str = TRIM(ADJUSTL(buf))//","

    is = 2
    Do i=1,NS
      is = is + 1
      buf = REPEAT(" ",60)
      NC = this%NC(i)
      Write(buf,'(i0)') this%NC(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%WT(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%HT(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') this%FC(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%SL(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%A(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') this%M(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') this%VH(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%XS(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%YS(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%ZS(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0,"H")') nc
      nh = TRIM(ADJUSTL(buf))
      If (i == NS) Then 
        fields(is)%str = nh//TRIM(ADJUSTL(this%TEXT(i)%str))//";"
      Else 
        fields(is)%str = nh//TRIM(ADJUSTL(this%TEXT(i)%str))//","
      End If
    End Do

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE212

  Subroutine initE212(this, DEP, form, NS, NC, WT, HT, FC, SL, A, M, VH, XS,  &
                      YS, ZS, TEXT, tmat) 
  !! Initialize this entity212_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity212_t), Intent(INOUT)   :: this

    Integer,                 Intent(IN) :: DEP
    Integer,                 Intent(IN) :: form
    Integer,                 Intent(IN) :: NS
    Integer,                 Intent(IN) :: NC(NS) 
    Real(WP),                Intent(IN) :: WT(NS) 
    Real(WP),                Intent(IN) :: HT(NS)
    Integer,                 Intent(IN) :: FC(NS) 
    Real(WP),                Intent(IN) :: SL(NS)
    Real(WP),                Intent(IN) :: A(NS)
    Integer,                 Intent(IN) :: M(NS) 
    Integer,                 Intent(IN) :: VH(NS) 
    Real(WP),                Intent(IN) :: XS(NS)
    Real(WP),                Intent(IN) :: YS(NS)
    Real(WP),                Intent(IN) :: ZS(NS)
    Type(string_t),          Intent(IN) :: TEXT(NS)
    Integer,       Optional, Intent(IN) :: tmat 
    Integer :: i

    this%entity_type = 212
    this%DEP         = DEP
    this%form        = form

    Select Case(form)
      Case(0,1,2,3,4,5,6,7,8,100,101,102,105)

        this%NS = NS
        this%NC = NC
        this%WT = WT
        this%HT = HT
        this%FC = FC
        this%SL = SL
        this%A  = A
        this%M  = M
        this%VH = VH
        this%XS = XS
        this%YS = YS
        this%ZS = ZS

        Do i=1,NS
          this%TEXT(i)%str = TEXT(i)%str
        End Do

        If (PRESENT(tmat)) this%tmat = tmat
      Case Default
        Write(stderr,*) ' *** forIGES ERROR ***'
        ERROR STOP " initE212 - Invalid form - must be 0-8, 100-102, 105"
    End Select 
  End Subroutine initE212

  Subroutine outputE212(this, ounit)
  !! Output this entity212_t entity type components to a user output unit

    Class(entity212_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: i

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 212 - General Note *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" NS           : ", i0)')     this%NS
    Write(ounit,'(" NC           : ", 8I10)')   this%NC(:)
    Write(ounit,'(" WT           : ", 5ES15.6)') this%WT(:)
    Write(ounit,'(" HT           : ", 5ES15.6)') this%HT(:)
    Write(ounit,'(" FC           : ", 8I10)')   this%FC(:)
    Write(ounit,'(" SL           : ", 5ES15.6)') this%SL(:)
    Write(ounit,'(" A            : ", 5ES15.6)') this%A(:)
    Write(ounit,'(" M            : ", 8I10)')   this%M(:)
    Write(ounit,'(" VH           : ", 8I10)')   this%VH(:)
    Write(ounit,'(" XS           : ", 5ES15.6)') this%XS(:)
    Write(ounit,'(" YS           : ", 5ES15.6)') this%YS(:)
    Write(ounit,'(" ZS           : ", 5ES15.6)') this%ZS(:)
    Do i=1,this%NS
      Write(ounit,'(" TEXT(",i0,") : ",A)') i, this%TEXT(i)%str
    End Do 

  End Subroutine outputE212

  Subroutine copyToE212(this, that)
  !! Copy this entity to that entity

    Class(entity212_t), Intent(IN)  :: this
    Type(entity212_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE212

  Subroutine initTypeE212(this, that)
  !! Copy that entity to this entity

    Class(entity212_t), Intent(OUT) :: this
    Type(entity212_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE212
 
  Subroutine deallocE308(this)
  !! Deallocate and reset this entity308_t entity

    Class(entity308_t), Intent(INOUT) :: this

    this%entity_type        = -1 
    this%DEPTH              = 0 
    this%N                  = 0 
    If (ALLOCATED(this%NAME)) DEALLOCATE(this%NAME)
    If (ALLOCATED(this%DE))   DEALLOCATE(this%DE)

  End Subroutine deallocE308

  Subroutine getE308(this, Precords, form)
  !! Translate IGES P records into this entity308_t entity type components

    Class(entity308_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer                   :: ET
    Integer                   :: DEPTH
    Character(80)             :: NAME
    Integer                   :: N
    Integer,      Allocatable :: DE(:)

    Integer                     :: i, ih, is
    Type(string_t), Allocatable :: fields(:)


    DEPTH = 0
    NAME  = REPEAT(" ",80)
    N     = 0

    Call PentityToString(Precords, string, this%DEP)
    Call getEntityFields(string,fields)

    Read(string,*) ET, DEPTH
    Read(fields(4)%str, *) N
    ALLOCATE(DE(N), SOURCE=0)
    
    NAME = REPEAT(" ",80)       

    ih = INDEX(fields(3)%str, "H")
    If (ih > 0) Then
      ih = ih+1
      Read(fields(3)%str(ih:), '(A)') NAME
    End If
    is = 4
    Do i=1,N
      is = is+1
      Read(fields(is)%str, *) DE(i)
    End Do
    DE = (DE+1)/2
   
    this%entity_type = ET
    this%form        = 0 
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE308 : Invalid form - must be 0")')
      End If 
    End If 

    this%DEPTH = DEPTH
    this%NAME  = TRIM(ADJUSTL(NAME))
    this%N     = N
    Call MOVE_ALLOC(DE, this%DE)

  End Subroutine getE308

  Subroutine makeE308(this, Precords)
  !! Translate this entity308_t type components into IGES P records

    Class(entity308_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, N, is, nfields, nlen 

    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    N       = this%N
    nfields = 4 + N 

    ALLOCATE(fields(nfields))

    nlen = LEN_TRIM(this%NAME)
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%DEPTH
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf, '(i0,"H",A)') nlen, TRIM(ADJUSTL(this%NAME))
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(4)%str = TRIM(ADJUSTL(buf))//","

    is = 4
    Do i=1,N
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%DE(i))
      If (i == N) Then
        fields(is)%str = TRIM(ADJUSTL(buf))//";"
      Else
        fields(is)%str = TRIM(ADJUSTL(buf))//","
      End If 
    End Do

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE308

  Subroutine initE308(this, DEP, DEPTH, NAME, N, DE, tmat)
  !! Initialize this entity308_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity308_t), Intent(INOUT) :: this

    Integer,               Intent(IN) :: DEP
    Integer,               Intent(IN) :: DEPTH
    Character(*),          Intent(IN) :: NAME
    Integer,               Intent(IN) :: N 
    Integer,               Intent(IN) :: DE(N)
    Integer,     Optional, Intent(IN) :: tmat
 
    this%entity_type = 308
    this%DEP         = DEP
    this%form        = 0

    this%DEPTH = DEPTH
    this%NAME  = TRIM(ADJUSTL(NAME))
    this%N     = N
    this%DE    = DE

    If (PRESENT(tmat)) this%tmat = tmat
 
  End Subroutine initE308

  Subroutine outputE308(this, ounit)
  !! Output this entity308_t entity type components to a user output unit

    Class(entity308_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 308 - Subfigure Definition *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DEPTH        : ", i0)') this%DEPTH
    Write(ounit,'(" NAME         : ",  A)') this%NAME
    Write(ounit,'(" N            : ", i0)') this%N 
    Write(ounit,*) ''
    Write(ounit,'(" DE Index     : ")')
    Write(ounit,*) ''
    Write(ounit,'(1x, 8i10)') this%DE(:)
    Write(ounit,*) ''
    Write(ounit,'(" DE Seq. No. : ")')
    Write(ounit,*) ''
    Write(ounit,'(1x, 8i10)') DE2PTR(this%DE(:))

  End Subroutine outputE308

 
  Subroutine copyToE308(this, that)
  !! Copy this entity to that entity

    Class(entity308_t), Intent(IN)  :: this
    Type(entity308_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE308

  Subroutine initTypeE308(this, that)
  !! Copy that entity to this entity

    Class(entity308_t), Intent(OUT) :: this
    Type(entity308_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE308

  Subroutine deallocE314(this)
  !! Deallocate and reset this entity314_t entity

    Class(entity314_t), Intent(INOUT) :: this

    this%entity_type = -1 
    this%CC1         = 0.0_WP 
    this%CC2         = 0.0_WP 
    this%CC3         = 0.0_WP 

    If (ALLOCATED(this%CNAME)) DEALLOCATE(this%CNAME)
  
  End Subroutine deallocE314

  Subroutine getE314(this, Precords, form)
  !! Translate IGES P records into this entity314_t entity type components

    Class(entity314_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer       :: ET
    Real(WP)      :: CC1
    Real(WP)      :: CC2
    Real(WP)      :: CC3
    Character(60) :: CNAME

    Integer      :: ih
    Type(string_t), Allocatable :: fields(:)

    CC1   = 0.0_WP
    CC2   = 0.0_WP
    CC3   = 0.0_WP
    CNAME = REPEAT(" ",60)

    Call PentityToString(Precords, string, this%DEP)

    Call getEntityFields(string, fields)

    Read(string,*) ET, CC1, CC2, CC3
    CNAME = REPEAT(" ",60)
    
    ih = INDEX(fields(5)%str,"H")
    If (ih /= 0) Then
      ih = ih+1
      Read(fields(5)%str(ih:),'(A)') CNAME
    End If

    this%entity_type        = ET
    this%form = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr, *) ''
        Write(stderr, *) ' *** forIGES WARNING ***'
        Write(stderr, '(" getE314 : Invalid form - must be 0")')
      End If 
    End If 

    this%CC1   = CC1
    this%CC2   = CC2
    this%CC3   = CC3
    this%CNAME = TRIM(ADJUSTL(CNAME))
  
  End Subroutine getE314

  Subroutine makeE314(this, Precords)
  !! Translate this entity314_t type components into IGES P records

    Class(entity314_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nc, nfields

    Character(60)             :: buf
    Character(:), Allocatable :: fmt
    Character(:), Allocatable :: nh 

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT
 
    nfields = 5 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%CC1 
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%CC2 
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%CC3 
    fields(4)%str = TRIM(ADJUSTL(buf))//","

    nc = LEN(this%CNAME)
    buf = REPEAT(" ",60)
    Write(buf,'(i0,"H")') nc
    nh = TRIM(ADJUSTL(nh))
    fields(5)%str = nh//this%CNAME//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE314

  Subroutine initE314(this, DEP, CC1, CC2, CC3, CNAME, tmat) 
  !! Initialize this entity314_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity314_t),    Intent(INOUT) :: this
    Integer,               Intent(IN)    :: DEP
    Real(WP),              Intent(IN)    :: CC1
    Real(WP),              Intent(IN)    :: CC2
    Real(WP),              Intent(IN)    :: CC3
    Character(*),          Intent(IN)    :: CNAME
    Integer,     Optional, Intent(IN) :: tmat

    this%entity_type = 314
    this%DEP         = DEP
    this%form        = 0

    this%CC1   = CC1
    this%CC2   = CC2
    this%CC3   = CC3
    this%CNAME = TRIM(ADJUSTL(CNAME))

    If (PRESENT(tmat)) this%tmat = tmat

  End Subroutine initE314

  Subroutine outputE314(this, ounit)
  !! Output this entity314_t entity type components to a user output unit

    Class(entity314_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 314 - Color Definition *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" CC1          : ", ES15.6)') this%CC1 
    Write(ounit,'(" CC2          : ", ES15.6)') this%CC2 
    Write(ounit,'(" CC3          : ", ES15.6)') this%CC3 
    Write(ounit,'(" CNAME        : ", A)')      this%CNAME

  End Subroutine outputE314

  Subroutine copyToE314(this, that)
  !! Copy this entity to that entity

    Class(entity314_t), Intent(IN)  :: this
    Type(entity314_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE314

  Subroutine initTypeE314(this, that)
  !! Copy that entity to this entity

    Class(entity314_t), Intent(OUT) :: this
    Type(entity314_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE314

  Subroutine deallocE402(this)
  !! Deallocate and reset this entity402_t entity

    Class(entity402_t), Intent(INOUT) :: this

    Integer :: iform

    this%entity_type = -1
    iform            = this%form 
    Select Case(iform)
      Case(1,7,14,15) 
        If (ALLOCATED(this%DE)) DEALLOCATE(this%DE)
        this%N                  = 0 
    End Select
    this%form = 0 
 
  End Subroutine deallocE402

  Subroutine getE402(this, Precords, form)
  !! Translate IGES P records into this entity402_t entity type components

    Class(entity402_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer                   :: ET
    Integer                   :: N
    Integer,      Allocatable :: DE(:)

    N = 0

    Call PentityToString(Precords, string, this%DEP)
    
    this%entity_type = ET
    this%form        = 0
    If (PRESENT(form)) this%form = form
    Select Case(form)
      Case(1,7,14,15)
        Read(string,*) ET, N
        ALLOCATE(DE(N), SOURCE=0)
        Read(string,*) ET, N, DE
        DE     = (DE+1)/2
        this%N = N
        Call MOVE_ALLOC(DE, this%DE)
      Case Default
        Write(stderr,*) ''
        Write(stderr,*) ' *** forIGES WARNING ***'
        Write(stderr,'(" getE402 : Invalid form - must be 1, 7, 14 or 15 ")')
    End Select 
 
  End Subroutine getE402

  Subroutine makeE402(this, Precords)
  !! Translate this entity402_t type components into IGES P records

    Class(entity402_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, is, iform, N, nfields

    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    iform = this%form
    Select Case(iform)
      Case(1,7,14,15)
        N = this%N
        nfields = 2 + N 
        ALLOCATE(fields(nfields))
        buf = REPEAT(" ",60)
        Write(buf,'(i0)') this%entity_type
        fields(1)%str = TRIM(ADJUSTL(buf))//","
        buf = REPEAT(" ",60)
        Write(buf,'(i0)') N
        fields(2)%str = TRIM(ADJUSTL(buf))//","
        is = 2
        Do i=1, N
          is = is + 1 
          buf = REPEAT(" ",60)
          Write(buf,'(i0)') DE2PTR(this%DE)
          If (i == N) Then 
            fields(is)%str = TRIM(ADJUSTL(buf))//";"
          Else
            fields(is)%str = TRIM(ADJUSTL(buf))//","
          End If
        End Do
        Call fieldsToRecs(fields, recbuf, 64)
        Call genPrecords(recbuf, Precords)
      Case Default
        Write(stderr,*) ' *** forIGES WARNING ***'
        Write(stderr,'(" makeE402 : Invalid form - must be 1, 7, 14 or 15")')
    End Select

  End Subroutine makeE402

  Subroutine initE402(this, DEP, form, N, DE, tmat)
  !! Initialize this entity402_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity402_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: form
    Integer,            Intent(IN)    :: N 
    Integer,            Intent(IN)    :: DE(N)
    Integer, Optional,  Intent(IN)    :: tmat 

 
    this%entity_type = 402
    this%DEP         = DEP
    this%form        = form
    If (PRESENT(tmat)) this%tmat= tmat
  
    Select Case(form)
      Case(1,7,14,15)
        this%N  = N
        this%DE = DE
      Case Default
        Write(stderr,*) ' *** forIGES ERROR ***'
        ERROR STOP " initE402 : Invalid form - must be 1, 7, 14, and 15"
    End Select 

  End Subroutine initE402

  Subroutine outputE402(this, ounit)
  !! Output this entity402_t entity type components to a user output unit

    Class(entity402_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer  :: iform 

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 402 - Associativity Instance ****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    iform = this%form

    Select Case(iform)
      Case(1,7,14,15)   
        Write(ounit,'(" N            : ", i0)') this%N 
        Write(ounit,*) ''
        Write(ounit,'(" DE Index     : ")')
        Write(ounit,*) ''
        Write(ounit,'(1x,8i10)') this%DE(:)
        Write(ounit,*) ''
        Write(ounit,'(" DE Seq. No.  : ")')
        Write(ounit,*) ''
        Write(ounit,'(1x,8i10)') DE2PTR(this%DE(:))
      Case Default
        Write(ounit,*) ''
        Write(ounit,'(" Entity 402 Form ",i0," is not supported")') iform
    End Select

  End Subroutine outputE402

  Subroutine copyToE402(this, that)
  !! Copy this entity to that entity

    Class(entity402_t), Intent(IN)  :: this
    Type(entity402_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE402

  Subroutine initTypeE402(this, that)
  !! Copy that entity to this entity

    Class(entity402_t), Intent(OUT) :: this
    Type(entity402_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE402

  Subroutine deallocE406(this)
  !! Deallocate and reset this entity406_t entity

    Class(entity406_t), Intent(INOUT) :: this

    If (this%form == 15) Then
      this%entity_type = -1 
      this%form        = 0 
      this%NP          = 0 
      If (ALLOCATED(this%NAME)) DEALLOCATE(this%NAME)
    Else If (this%form == 1) Then
      this%entity_type = -1 
      this%form        = 0 
      this%NP          = 0 
      If (ALLOCATED(this%L)) DEALLOCATE(this%L)

    End If
         
  End Subroutine deallocE406

  Subroutine getE406(this, Precords, form)
  !! Translate IGES P records into this entity406_t entity type components

    Class(entity406_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form
 
    Integer                       :: dum
    Integer                       :: ih
    Integer                       :: ET
    Integer                       :: NP
    Character(60)                 :: NAME
    Integer,        Allocatable   :: L(:) 
    Character(:),   Allocatable   :: string
    Type(string_t), Allocatable   :: fields(:)

    this%form = 0
    If (PRESENT(form)) this%form = form

    Call PentityToString(Precords, string, this%DEP)
    Read(string,*) ET, NP
    this%entity_type = ET
    this%NP          = NP
    If (this%form == 15) Then
      Call getEntityFields(string, fields)
      ih = INDEX(fields(3)%str,"H")
      NAME = REPEAT(" ", 60)
      If (ih /= 0) Then
        ih=ih+1
        Read(fields(3)%str(ih:), '(A)') NAME
      End If
      this%NAME        = TRIM(ADJUSTL(NAME))
    ElseIf (this%form == 1) Then 
      ALLOCATE(L(NP), SOURCE = 0)
      Read(string,*) dum, dum, L
      CALL MOVE_ALLOC(L, this%L)
      this%NAME = C_NULL_CHAR 
    Else
      Write(stderr,*) ''
      Write(stderr,*) ' *** forIGES WARNING ***'
      Write(stderr,'(" getE406 : Invalid form - must be 1 or 15")') 
    End If
         
  End Subroutine getE406

  Subroutine makeE406(this, Precords)
  !! Translate this entity406_t type components into IGES P records

    Class(entity406_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 


    Integer :: i, is, nc, np, nfields

    Character(60) :: buf
    Character(:), Allocatable :: nh

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    np = this%NP
    If (this%form == 15) Then
      nfields = 3
    Else  
      nfields = 2 + NP
    End If    

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%NP
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    If (this%form == 1) Then
      is = 2
      Do i=1,NP
        is = is + 1
        buf = REPEAT(" ",60)
        Write(buf,'(i0)') this%L(i)
        If (i == NP) Then
          fields(is)%str = TRIM(ADJUSTL(buf))//";"
        Else
          fields(is)%str = TRIM(ADJUSTL(buf))//","
        End If
      End Do
    ElseIf(this%form == 15) Then
      nc  = LEN(this%NAME)
      buf = REPEAT(" ",60)
      Write(buf, '(i0,"H")') nc
      nh = TRIM(ADJUSTL(buf))
      fields(3)%str = nh//this%NAME//";"
    End If

    If (this%form == 15 .OR. this%form == 1) Then
      Call fieldsToRecs(fields, recbuf, 64)
      Call genPrecords(recbuf, Precords)
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      Write(stderr,*) ' makeE406 : Invalid form - must be 1 or 15 '
    End if

  End Subroutine makeE406

  Subroutine initE406(this, DEP, form,  NP, NAME, L, tmat) 
  !! Initialize this entity406_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity406_t), Intent(INOUT) :: this

    Integer,                Intent(INOUT) :: DEP
    Integer,                Intent(IN)    :: form 
    Integer,                Intent(INOUT) :: NP 
    Character(*), Optional, Intent(IN)    :: NAME
    Integer,      Optional, Intent(IN)    :: L(NP)
    Integer,      Optional, Intent(IN)    :: tmat 


    this%entity_type = 406
    this%DEP         = DEP
    this%NP          = NP
    this%form        = form
    If (PRESENT(tmat)) this%tmat = tmat

  
    If (form == 15) Then
      this%NAME        = TRIM(ADJUSTL(NAME))
    ElseIf (form == 1) Then 
      this%L = L 
      this%NAME = C_NULL_CHAR 
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE406 - Invalid form - must be 1 or 15" 
    End If

  End Subroutine initE406

  Subroutine outputE406(this, ounit)
  !! Output this entity406_t entity type components to a user output unit

    Class(entity406_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit


    If (this%form == 15) Then
      Write(ounit,*) ''
      Write(ounit,*) ' *****  Entity 406 - Property Form 15 (name) ****'
      Write(ounit,*) ''

      Write(ounit,'(" Entity Type : ", i0)') this%entity_type
      Write(ounit,'(" Entity Form : ", i0)') this%form
      Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

      Write(ounit,'(" NP          : ", i0)') this%NP
      Write(ounit,'(" NAME        : ", A)')  this%NAME
    ElseIf (this%form == 1) Then
      Write(ounit,*) ''
      Write(ounit,*) ' *****  Entity 406 - Property Form 1 (Levels) ****'
      Write(ounit,*) ''

      Write(ounit,'(" Entity Type : ", i0)') this%entity_type
      Write(ounit,'(" Entity Form : ", i0)') this%form
      Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

      Write(ounit,'(" NP          : ", i0)') this%NP
      Write(ounit,'(" L           : ")')
      Write(ounit,'(1x, 8i10)') this%L
    Else
      Write(ounit,*) ''
      Write(ounit,'(" ***** Entity 406 Form ", i0," is not supported *****")')&
            this%form
    End If
             
  End Subroutine outputE406

  Subroutine copyToE406(this, that)
  !! Copy this entity to that entity

    Class(entity406_t), Intent(IN)  :: this
    Type(entity406_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE406

  Subroutine initTypeE406(this, that)
  !! Copy that entity to this entity

    Class(entity406_t), Intent(OUT) :: this
    Type(entity406_t),  Intent(IN)  :: that

    Call that%copyTo(this)

  End Subroutine initTypeE406

  Subroutine deallocE408(this)
  !! Deallocate and reset this entity408_t entity

    Class(entity408_t), Intent(INOUT) :: this
 
    this%entity_type = -1
    this%form        = 0
    this%DE          = 0
    this%X           = 0.0_WP
    this%Y           = 0.0_WP
    this%Z           = 0.0_WP
    this%S           = 0.0_WP

  End Subroutine deallocE408

  Subroutine getE408(this, Precords, form)
  !! Translate IGES P records into this entity408_t entity type components

    Class(entity408_t),           Intent(INOUT) :: this
    Character(80),                Intent(IN)    :: Precords(:)
    Integer,            Optional, Intent(IN)    :: form 
 
    Character(:), Allocatable :: string

    Integer  :: ET
    Integer  :: DE
    Real(WP) :: X
    Real(WP) :: Y
    Real(WP) :: Z
    Real(WP) :: S

    DE = 0
    X  = 0.0_WP
    Y  = 0.0_WP
    Z  = 0.0_WP
    S  = 0.0_WP

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, DE, X, Y, Z, S

    DE = (DE+1)/2
   
    this%entity_type = ET
    this%form        = 0
    If (PRESENT(form)) Then
      If (form /= 0) Then
        Write(stderr,*) ''
        Write(stderr,*) ' *** forIGES WARNING ***'
        Write(stderr,'(" getE408 : Invalid form - must be 0")') 
      End If
    End If
 
    this%DE = DE
    this%X  = X
    this%Y  = Y
    this%Z  = Z
    this%S  = S

  End Subroutine getE408

  Subroutine makeE408(this, Precords)
  !! Translate this entity408_t type components into IGES P records

    Class(entity408_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: nfields

    Character(60) :: buf
    Character(:), Allocatable :: fmt

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    nfields = 6 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%DE) 
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%X 
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Y 
    fields(4)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%Z 
    fields(5)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,fmt) this%S 
    fields(6)%str = TRIM(ADJUSTL(buf))//";"

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE408

  Subroutine initE408(this, DEP, DE, X, Y, Z, S, tmat) 
  !! Initialize this entity408_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity408_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: DE
    Real(WP),           Intent(IN)    :: X
    Real(WP),           Intent(IN)    :: Y
    Real(WP),           Intent(IN)    :: Z
    Real(WP),           Intent(IN)    :: S
    Integer,  Optional, Intent(IN)    :: tmat

    this%entity_type = 408
    this%DEP         = DEP 
    this%DE          = DE
    this%X           = X
    this%Y           = Y
    this%Z           = Z
    this%S           = S

    If (PRESENT(tmat)) this%tmat = tmat

  End Subroutine initE408

  Subroutine outputE408(this, ounit)
  !! Output this entity408_t entity type components to a user output unit

    Class(entity408_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit
 
    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 408 - Singular Subfigure Instance ****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" DE Index     : ", i0)') this%DE
    Write(ounit,'(" DE Seq. No.  : ", i0)') DE2PTR(this%DE) 
    Write(ounit,'(" X            : ", ES15.6)') this%X
    Write(ounit,'(" Y            : ", ES15.6)') this%Y
    Write(ounit,'(" Z            : ", ES15.6)') this%Z
    Write(ounit,'(" S            : ", ES15.6)') this%S

  End Subroutine outputE408

  Subroutine copyToE408(this, that)
  !! Copy this entity to that entity

    Class(entity408_t), Intent(IN)  :: this
    Type(entity408_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE408


  Subroutine initTypeE408(this, that)
  !! Copy that entity to this entity

    Class(entity408_t), Intent(OUT) :: this
    Type(entity408_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE408

  Subroutine deallocE502(this)
  !! Deallocate and reset this entity502_t entity

    Class(entity502_t), Intent(INOUT) :: this
  
    this%entity_type = -1
    this%form        = 0
    this%N           = 0
    If (ALLOCATED(this%X)) DEALLOCATE(this%X)
    If (ALLOCATED(this%Y)) DEALLOCATE(this%Y)
    If (ALLOCATED(this%Z)) DEALLOCATE(this%Z)

  End Subroutine deallocE502

  Subroutine getE502(this, Precords, form)
  !! Translate IGES P records into this entity502_t entity type components

    Class(entity502_t),         Intent(INOUT) :: this
    Character(80),              Intent(IN)    :: Precords(:)
    Integer,          Optional, Intent(IN)    :: form 

    Character(:), Allocatable :: string

    Integer               :: ET
    Integer               :: N
    Real(WP), Allocatable :: X(:) 
    Real(WP), Allocatable :: Y(:) 
    Real(WP), Allocatable :: Z(:) 

    Integer  :: i, dum

    N = 0
    Call PentityToString(Precords, string, this%DEP)
   
    this%form = 1
    If (PRESENT(form)) Then
      If (form /= 1) Then
        Write(stderr,*) ''
        Write(stderr,*) ' *** forIGES WARNING ***'
        Write(stderr,'(" getE502 : Invalid form - must be 1")') 
      End If
    End If
 
    If (PRESENT(form)) this%form = form 

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, N

    ALLOCATE(X(N), SOURCE=0.0_WP)
    ALLOCATE(Y(N), SOURCE=0.0_WP)
    ALLOCATE(Z(N), SOURCE=0.0_WP)

    Read(string,*) dum, dum, (X(i),Y(i),Z(i), i=1,N)

    this%entity_type = ET
    this%N           = N

    Call MOVE_ALLOC(X, this%X)
    Call MOVE_ALLOC(Y, this%Y)
    Call MOVE_ALLOC(Z, this%Z)

  End Subroutine getE502

  Subroutine makeE502(this, Precords)
  !! Translate this entity502_t type components into IGES P records

    Class(entity502_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, is, N, nfields

    Character(60) :: buf
    Character(:), Allocatable :: fmt

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    fmt = REAL_FORMAT

    N = this%N
    nfields = 2 + 3*N 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    is = 2
    Do i=1,N
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%X(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%Y(i)
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,fmt) this%Z(i)
      If (i == N) Then
        fields(is)%str = TRIM(ADJUSTL(buf))//";"
      Else
        fields(is)%str = TRIM(ADJUSTL(buf))//","
      End If
    End Do

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE502

  Subroutine initE502(this, DEP, form, N, X, Y, Z, tmat) 
  !! Initialize this entity502_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity502_t), Intent(INOUT) :: this

    Integer,            Intent(IN) :: DEP
    Integer,            Intent(IN) :: form 
    Integer,            Intent(IN) :: N 
    Real(WP),           Intent(IN) :: X(N) 
    Real(WP),           Intent(IN) :: Y(N) 
    Real(WP),           Intent(IN) :: Z(N)
    Integer,  Optional, Intent(IN) :: tmat
 
    this%entity_type = 502
    this%DEP         = DEP
    this%form        = form
    If (form == 1) Then 
      this%N           = N
      this%X           = X
      this%Y           = Y
      this%Z           = Z
      If (PRESENT(tmat)) this%tmat = tmat
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE502 : Invalid form - must be 1"
    End If

  End Subroutine initE502

  Subroutine outputE502(this, ounit)
  !! Output this entity502_t entity type components to a user output unit

    Class(entity502_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 502 - Vertex List *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" N            : ", i0)') this%N
    Write(ounit,*) ''
    Write(ounit,'(" X            : ")')
    Write(ounit,'(1x,5ES15.6)') this%X(:)
    Write(ounit,*) ''
    Write(ounit,'(" Y            : ")')
    Write(ounit,'(1x,5ES15.6)') this%Y(:)
    Write(ounit,*) ''
    Write(ounit,'(" Z          : ")')
    Write(ounit,'(1x,5ES15.6)') this%Z(:)

  End Subroutine outputE502

  Subroutine copyToE502(this, that)
  !! Copy this entity to that entity

    Class(entity502_t), Intent(IN)  :: this
    Type(entity502_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE502

  Subroutine initTypeE502(this, that)
  !! Copy that entity to this entity

    Class(entity502_t), Intent(OUT) :: this
    Type(entity502_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE502

  Subroutine deallocE504(this)
  !! Deallocate and reset this entity504_t entity

    Class(entity504_t), Intent(INOUT) :: this
  
    this%entity_type = -1
    this%form        = 0
    this%N           = 0

    If (ALLOCATED(this%CURV)) DEALLOCATE(this%CURV)
    If (ALLOCATED(this%SVP))  DEALLOCATE(this%SVP)
    IF (ALLOCATED(this%SV))   DEALLOCATE(this%SV)
    If (ALLOCATED(this%TVP))  DEALLOCATE(this%TVP)
    If (ALLOCATED(this%TV))   DEALLOCATE(this%TV)

  End Subroutine deallocE504

  Subroutine getE504(this, Precords, form)
  !! Translate IGES P records into this entity504_t entity type components

    Class(entity504_t),         Intent(INOUT) :: this
    Character(80),              Intent(IN)    :: Precords(:)
    Integer,          Optional, Intent(IN)    :: form 

    Character(:), Allocatable :: string

    Integer                   :: ET
    Integer                   :: N
    Integer,      Allocatable :: CURV(:)  
    Integer,      Allocatable :: SVP(:)  
    Integer,      Allocatable :: SV(:)  
    Integer,      Allocatable :: TVP(:)  
    Integer,      Allocatable :: TV(:)
  
    Integer :: i, dum

    N = 0

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) ET, N
 
    this%entity_type = ET
    this%form = 1
    If (PRESENT(form)) Then
      If (form /= 1) Then
        Write(stderr,*) ''
        Write(stderr,*) ' *** forIGES WARNING ***'
        Write(stderr,'(" getE504 : Invalid form - must be 1")') 
      End If
    End If
 
    ALLOCATE(CURV(N), SOURCE=0)
    ALLOCATE(SVP(N),  SOURCE=0)
    ALLOCATE(SV(N),   SOURCE=0)
    ALLOCATE(TVP(N),  SOURCE=0)
    ALLOCATE(TV(N),   SOURCE=0)

    Read(string,*) dum, dum, (CURV(i), SVP(i), SV(i), TVP(i), TV(i), i=1,N)

    CURV = (CURV + 1)/2
    SVP  = (SVP + 1)/2
    TVP  = (TVP + 1)/2

    Call MOVE_ALLOC(CURV, this%CURV)
    Call MOVE_ALLOC(SVP,  this%SVP)
    Call MOVE_ALLOC(SV,   this%SV)
    Call MOVE_ALLOC(TVP,  this%TVP)
    Call MOVE_ALLOC(TV,   this%TV)

  End Subroutine getE504

  Subroutine makeE504(this, Precords)
  !! Translate this entity504_t entity type components to IGES P records

    Class(entity504_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, N, is, nfields

    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    N = this%N
    nfields = 2 + 5*N 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(1)%str = TRIM(ADJUSTL(buf))//","

    is = 2
    Do i=1,N
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%CURV) 
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%SVP) 
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') this%SV 
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%TVP) 
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is + 1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') this%TV
      If (i == N) Then 
        fields(is)%str = TRIM(ADJUSTL(buf))//";"
      Else
        fields(is)%str = TRIM(ADJUSTL(buf))//","
      End if
    End Do

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE504

  Subroutine initE504(this, DEP, form, N, CURV, SVP, SV, TVP, TV, tmat) 
  !! Initialize this entity504_t entity type with user supplied data. See 
  !! the entity type for variable definitions
 
    Class(entity504_t), Intent(INOUT) :: this

    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: N
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: CURV(N)
    Integer,            Intent(IN)    :: SVP(N)
    Integer,            Intent(IN)    :: SV(N)
    Integer,            Intent(IN)    :: TVP(N)
    Integer,            Intent(IN)    :: TV(N)
    Integer, Optional,  Intent(IN)    :: tmat

    this%entity_type = 504 
    this%DEP         = DEP
    this%form        = form

    If (form == 1) Then 
      this%N           = N
      this%CURV        = CURV
      this%SVP         = SVP
      this%SV          = SV
      this%TVP         = TVP
      this%TV          = TV
      If (PRESENT(tmat)) this%tmat = tmat
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE504 : Invalid form - must be 1"
    End If

  End Subroutine initE504

  Subroutine outputE504(this, ounit)
  !! Output this entity504_t entity type components to a user output unit

    Class(entity504_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 504 - Edge *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type   : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form   : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" N             : ", i0)') this%N
    Write(ounit,*) ''
    Write(ounit,'(" CURV Index    : ")')
    Write(ounit,'(1x, 8i10)')               this%CURV
    Write(ounit,*) ''
    Write(ounit,'(" CURV Seq. No. : ")')
    Write(ounit,'(1x, 8i10)')               DE2PTR(this%CURV)

    Write(ounit,*) ''
    Write(ounit,'(" SVP  Index    : ")')
    Write(ounit,'(1x, 8i10)')               this%SVP
    Write(ounit,*) ''
    Write(ounit,'(" SVP Seq. No.  : ")')
    Write(ounit,'(1x, 8i10)')               DE2PTR(this%SVP)

    Write(ounit,*) ''
    Write(ounit,'(" SV            : ")')
    Write(ounit,'(1x, 8i10)')               this%SV  

    Write(ounit,*) ''
    Write(ounit,'(" TVP Index     : ")')
    Write(ounit,'(1x, 8i10)')               this%TVP
    Write(ounit,*) ''
    Write(ounit,'(" TVP Seq. No.  : ")')
    Write(ounit,'(1x, 8i10)')               DE2PTR(this%TVP)

    Write(ounit,*) ''
    Write(ounit,'(" TV            : ")')
    Write(ounit,'(1x, 8i10)')               this%TV  

  End Subroutine outputE504

  Subroutine copyToE504(this, that)
  !! Copy this entity to that entity

    Class(entity504_t), Intent(IN)  :: this
    Type(entity504_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE504

  Subroutine initTypeE504(this, that)
  !! Copy that entity to this entity

    Class(entity504_t), Intent(OUT) :: this
    Type(entity504_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE504

  Subroutine deallocE508(this)
  !! Deallocate and reset this entity508_t entity

    Class(entity508_t), Intent(INOUT) :: this
 
    Integer :: i
 
    this%entity_type = -1
    this%form        = 0

    If (ALLOCATED(this%TYPE)) DEALLOCATE(this%TYPE)
    If (ALLOCATED(this%EDGE)) DEALLOCATE(this%EDGE)
    If (ALLOCATED(this%NDX))  DEALLOCATE(this%NDX)
    If (ALLOCATED(this%OF))   DEALLOCATE(this%OF)
    If (ALLOCATED(this%K))    DEALLOCATE(this%K)

    Do i=1, this%N
      If (ALLOCATED(this%ISOP(i)%vals)) DEALLOCATE(this%ISOP(i)%vals)
      If (ALLOCATED(this%CURV(i)%vals)) DEALLOCATE(this%CURV(i)%vals)
    End Do
    If (ALLOCATED(this%ISOP)) DEALLOCATE(this%ISOP)
    If (ALLOCATED(this%CURV)) DEALLOCATE(this%CURV)

    this%N = 0

  End Subroutine deallocE508

  Subroutine getE508(this, Precords, form)
  !! Translate IGES P records into this entity508_t entity type components

    Class(entity508_t),         Intent(INOUT) :: this
    Character(80),              Intent(IN)    :: Precords(:)
    Integer,          Optional, Intent(IN)    :: form 

    Character(:), Allocatable     :: string

    Integer              :: ET
    Integer              :: N
    Integer, Allocatable :: TYPE(:) 
    Integer, Allocatable :: EDGE(:) 
    Integer, Allocatable :: NDX(:)
    Logical, Allocatable :: OF(:) 
    Integer, Allocatable :: K(:)

    Integer :: i, j, is, NP

    Type(intArray_t), Allocatable :: ISOP(:) 
    Type(intArray_t), Allocatable :: CURV(:)
    Type(string_t),   Allocatable :: fields(:) 

    N = 0
    
    Call PentityToString(Precords, string, this%DEP)
    Call getEntityFields(string, fields)
 
    Read(string,*) ET, N

    ALLOCATE(TYPE(N),  SOURCE=0) 
    ALLOCATE(EDGE(N), SOURCE=0) 
    ALLOCATE(NDX(N),  SOURCE=0) 
    ALLOCATE(OF(N)) 
    ALLOCATE(K(N),    SOURCE=0)

    ALLOCATE(ISOP(N))
    ALLOCATE(CURV(N))
    ALLOCATE(this%ISOP(N))
    ALLOCATE(this%CURV(N))
 
 
    this%entity_type = ET
    this%form        = 0
    If (PRESENT(form)) Then
      this%form = form
      If (form <0 .OR. form>1) Then
        Write(stderr,*) ''
        Write(stderr,*) ' *** forIGES WARNING ***'
        Write(stderr,'(" getE508 : Invalid form - must be 0 or 1")') 
      End If
    End If
 
    is = 3
    Do i = 1,N 
      Read(fields(is)%str, *) TYPE(i)
      is = is+1
      Read(fields(is)%str,*)  EDGE(i)
      is = is+1
      Read(fields(is)%str,*)  NDX(i)
      is = is+1
      Read(fields(is)%str,*)  OF(i)
      is = is+1
      Read(fields(is)%str,*)  K(i)
      NP = K(i)
      ALLOCATE(ISOP(i)%vals(NP))
      ALLOCATE(CURV(i)%vals(NP), SOURCE=0)
      Do j = 1,NP
        is = is+1
        Read(fields(is)%str,*) ISOP(i)%vals(j)
        is = is+1
        Read(fields(is)%str,*) CURV(i)%vals(j)
      End Do
    End Do

    EDGE = (EDGE+1)/2
    Do i=1,N
      CURV(i)%vals = (CURV(i)%vals+1)/2
    End Do

    this%N = N

    CALL MOVE_ALLOC(TYPE,  this%TYPE)
    CALL MOVE_ALLOC(EDGE, this%EDGE)
    CALL MOVE_ALLOC(NDX,  this%NDX)
    CALL MOVE_ALLOC(OF,   this%OF) 
    CALL MOVE_ALLOC(K,    this%K)

    Do i=1, N
      CALL MOVE_ALLOC(ISOP(i)%vals, this%ISOP(i)%vals) 
      CALL MOVE_ALLOC(CURV(i)%vals, this%CURV(i)%vals)
    End Do 

  End Subroutine getE508

  Subroutine makeE508(this, Precords)
  !! Translate this entity508_t type components into IGES P records

    Class(entity508_t),            Intent(IN)    :: this
    Character(80),    Allocatable, Intent(INOUT) :: Precords(:) 

    Integer :: i, j, is, N, NP, nfields

    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    N = this%N
    nfields = 2
    Do i = 1,N
      NP = this%K(i)
      nfields = nfields + NP
    End Do  

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(2)%str = TRIM(ADJUSTL(buf))//","
     
    is = 2
    Do i=1,N
      NP = this%K(i)
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%EDGE(i))
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') this%NDX 
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,'(l1)') this%OF 
      fields(is)%str = TRIM(ADJUSTL(buf))//","
      Do j=1,NP
        is = is+1
        buf = REPEAT(" ",60)
        Write(buf,'(l1)') this%ISOP(i)%vals(j) 
        fields(is)%str = TRIM(ADJUSTL(buf))//","
        is = is+1
        buf = REPEAT(" ",60)
        Write(buf,'(i0)') DE2PTR(this%CURV(i)%vals(j)) 
        If (j == NP .AND. i == N) Then
          fields(is)%str = TRIM(ADJUSTL(buf))//";"
        Else
          fields(is)%str = TRIM(ADJUSTL(buf))//","
        End if
      End Do
    End Do
 
    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE508

  Subroutine initE508(this, DEP, form, N, TYPE, EDGE, NDX, OF, K, ISOP, CURV,  &
                      tmat) 
  !! Initialize this entity508_t entity type with user supplied data. See 
  !! entity type for variable definitions

    Class(entity508_t),         Intent(INOUT) :: this
    Integer,                    Intent(IN)    :: DEP
    Integer,                    Intent(IN)    :: form 
    Integer,                    Intent(IN)    :: N
    Integer,                    Intent(IN)    :: TYPE(N)
    Integer,                    Intent(IN)    :: EDGE(N)
    Integer,                    Intent(IN)    :: NDX(N)
    Logical,                    Intent(IN)    :: OF(N)
    Integer,                    Intent(IN)    :: K(N)
    Type(intArray_t),           Intent(IN)    :: ISOP(N)
    Type(intArray_t),           Intent(IN)    :: CURV(N)
    Integer,          Optional, Intent(IN)    :: tmat
    Integer :: i

    this%entity_type = 508
    this%DEP         = DEP 
    this%form        = form

    If (form == 1) Then
      this%N     = N
      this%TYPE  = TYPE
      this%EDGE  = EDGE
      this%NDX   = NDX
      this%OF    = OF 
      this%K     = K

      Do i=1, N
        this%ISOP(i)%vals = ISOP(i)%vals
        this%CURV(i)%vals = CURV(i)%vals
      End Do 

      If (PRESENT(tmat)) this%tmat = tmat

    Else

      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE508 : Invalid form - must be 1"

    End If
 
  End Subroutine initE508

  Subroutine outputE508(this, ounit)
  !! Output this entity508_t entity type components to a user output unit

    Class(entity508_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Integer :: i

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 508 - Loop *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" N            : ", i0)') this%N
    Write(ounit,*) ''
    Write(ounit,'(" TYPE         : ")')
    Write(ounit,'(1x,8i10)')                 this%TYPE
    Write(ounit,*) ''
    Write(ounit,'(" EDGE Index        : ")')
    Write(ounit,'(1x,8i10)')                 this%EDGE
    Write(ounit,*) ''
    Write(ounit,'(" EDGE Seq. No.        : ")')
    Write(ounit,'(1x,8i10)')                 DE2PTR(this%EDGE)

    Write(ounit,*) ''
    Write(ounit,'(" NDX          : ")')
    Write(ounit,'(1x,8i10)')                 this%NDX
    Write(ounit,*) ''
    Write(ounit,'(" OF           : ")')
    Write(ounit,'(1x,*(i0))')                 this%OF
    Write(ounit,*) ''
    Write(ounit,'(" K            : ")')
    Write(ounit,'(1x,8i10)')                 this%K
    Write(ounit,*) ''

    Do i=1,this%N
      Write(ounit,*) ''
      Write(ounit,'(" ISOP(",i0,") : ")') i
      Write(ounit,'(1x, *(i0))') this%ISOP(i)%vals
    End Do

    Do i=1,this%N
      Write(ounit,*) ''
      Write(ounit,'(" CURV(",i0,") Index    : ")') i
      Write(ounit,'(1x, 8i10)') this%CURV(i)%vals
      Write(ounit,*) ''
      Write(ounit,'(" CURV(",i0,") Seq. No. : ")') i
      Write(ounit,'(1x, 8i10)') DE2PTR(this%CURV(i)%vals)
    End Do

  End Subroutine outputE508

  Subroutine copyToE508(this, that)
  !! Copy this entity to that entity type

    Class(entity508_t), Intent(IN)  :: this
    Type(entity508_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE508

  Subroutine initTypeE508(this, that)
  !! Copy that entity to this entity

    Class(entity508_t), Intent(OUT) :: this
    Type(entity508_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE508

  Subroutine deallocE510(this)
  !! Deallocate and reset this entity510_t entity 

    Class(entity510_t), Intent(INOUT) :: this
  
    this%entity_type = -1
    this%form        = 0
    this%SURF        = 0
    this%N           = 0
    this%OF          = .FALSE.
    If (ALLOCATED(this%LOOP)) DEALLOCATE(this%LOOP)

  End Subroutine deallocE510

  Subroutine getE510(this, Precords, form)
  !! Translate input IGES P records into this entity510_t type components

    Class(entity510_t),         Intent(INOUT) :: this
    Character(80),              Intent(IN)    :: Precords(:)
    Integer,          Optional, Intent(IN)    :: form 

    Character(:), Allocatable :: string
    Integer                   :: ET
    Integer                   :: SURF 
    Integer                   :: N
    Logical                   :: OF
    Integer,      Allocatable :: LOOP(:)

    Integer :: dum

    SURF = 0
    N    = 0
    OF   = .FALSE.

    this%form = 1
    If (PRESENT(form)) Then
      If (form /= 1) Then
        Write(stderr,*) ''
        Write(stderr,*) ' *** forIGES WARNING ***'
        Write(stderr,'(" getE508 : Invalid form - must be 1")') 
      End If
    End If

    Call PentityToString(Precords, string, this%DEP)

    Read(string,*) SURF, ET, N

    ALLOCATE(LOOP(N), SOURCE=0)
 
    Read(string,*) dum, dum, dum, OF, LOOP 

    SURF = (SURF + 1)/2
    LOOP = (LOOP + 1)/2

    this%entity_type = ET
    this%SURF        = SURF
    this%N           = N
    this%OF          = OF

    CALL MOVE_ALLOC(LOOP, this%LOOP)
    
  End Subroutine getE510

  Subroutine makeE510(this, Precords)
  !! Translate this entity510_t type components into an array of P records

    Class(entity510_t),              Intent(IN)    :: this
    Character(80),      Allocatable, Intent(INOUT) :: Precords(:) 


    Integer :: i, N, is, nfields

    Character(60) :: buf

    Type(string_t), Allocatable :: fields(:)
    Type(string_t), Allocatable :: recbuf(:)

    N       = this%N
    nfields = 4 + N 

    ALLOCATE(fields(nfields))

    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%entity_type
    fields(1)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') DE2PTR(this%SURF)
    fields(2)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%N
    fields(3)%str = TRIM(ADJUSTL(buf))//","
    buf = REPEAT(" ",60)
    Write(buf,'(i0)') this%OF
    fields(4)%str = TRIM(ADJUSTL(buf))//","

    is = 4
    Do i=1,N
      is = is+1
      buf = REPEAT(" ",60)
      Write(buf,'(i0)') DE2PTR(this%LOOP(i))
      If (i == N) Then
        fields(is)%str = TRIM(ADJUSTL(buf))//";"
      Else
        fields(is)%str = TRIM(ADJUSTL(buf))//","
      End If

    End Do 

    Call fieldsToRecs(fields, recbuf, 64)
    Call genPrecords(recbuf, Precords)

  End Subroutine makeE510

  Subroutine initE510(this, DEP, form, SURF, N, OF, LOOP, tmat) 
  !! Initialize this entity510_t entity using user supplied data. See entity
  !! type for variable definitions

    Class(entity510_t), Intent(INOUT) :: this
    Integer,            Intent(IN)    :: DEP
    Integer,            Intent(IN)    :: form 
    Integer,            Intent(IN)    :: SURF
    Integer,            Intent(IN)    :: N
    Logical,            Intent(IN)    :: OF 
    Integer,            Intent(IN)    :: LOOP(N)
    Integer, Optional,  Intent(IN)    :: tmat
 
    this%entity_type = 510
    this%DEP         = DEP
    this%form        = form
    If (form == 1) Then 
      this%SURF        = SURF
      this%N           = N
      this%OF          = OF
      this%LOOP        = LOOP 
      If (PRESENT(tmat)) this%tmat = tmat
    Else
      Write(stderr,*) ' *** forIGES ERROR ***'
      ERROR STOP " initE510 : Invalid form - must be 1"
    End If

  End Subroutine initE510

  Subroutine outputE510(this, ounit)
  !! Output this entity510_t entity type components to a user output unit

    Class(entity510_t), Intent(IN) :: this
    Integer,            Intent(IN) :: ounit

    Write(ounit,*) ''
    Write(ounit,*) ' *****  Entity 510 - Face *****'
    Write(ounit,*) ''

    Write(ounit,'(" Entity Type  : ", i0)') this%entity_type
    Write(ounit,'(" Entity Form  : ", i0)') this%form
    Write(ounit,'(" Entity Tmat  : ", i0)') this%tmat

    Write(ounit,'(" SURF Index    : ", i0)') this%SURF
    Write(ounit,'(" SURF Seq. No. : ", i0)') DE2PTR(this%SURF)
    Write(ounit,'(" N             : ", i0)') this%N
    Write(ounit,'(" OF            : ", i0)') this%OF
    Write(ounit,*) ''
    Write(ounit,'(" LOOP Index    : ")')
    Write(ounit,'(1x, 8i10)') this%LOOP(:) 
    Write(ounit,*) ''
    Write(ounit,'(" LOOP Seq. No. : ")')
    Write(ounit,'(1x, 8i10)') DE2PTR(this%LOOP(:))
 
  End Subroutine outputE510

  Subroutine copyToE510(this, that)
  !! Copy that entity to this entity
    Class(entity510_t), Intent(IN)  :: this
    Type(entity510_t),  Intent(OUT) :: that

    that = this

  End Subroutine copyToE510

  Subroutine initTypeE510(this, that)
  !! Copy that entity to this entity
    Class(entity510_t), Intent(OUT) :: this
    Type(entity510_t),  Intent(IN)  :: that

    Call that%copyTo(this)
 
  End Subroutine initTypeE510

End Module IGES_Psection
