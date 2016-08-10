!>@file   m_spheric_constants.f90
!!@brief  module m_spheric_constants
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Constants for spectr data
!!
!!@verbatim
!!        igrid_Chebyshev =    2 :: Chebyshev collocation points
!!        igrid_non_equidist = 1 :: non-equi-distance
!!        igrid_equidistance = 0 :: equi-distance
!!
!!       spherical mesh type  -1: none, 0: Gauss points only
!!                             1: include poles
!!                             2: include center
!!@endverbatim
!
      module m_spheric_constants
!
      use m_precision
!
      integer(kind = kint), parameter :: igrid_half_Chebyshev = 3
      integer(kind = kint), parameter :: igrid_Chebyshev =      2
      integer(kind = kint), parameter :: igrid_non_equidist =   1
      integer(kind = kint), parameter :: igrid_equidistance =   0
!
      integer(kind = kint), parameter :: iflag_no_FEMMESH =   -1
      integer(kind = kint), parameter :: iflag_MESH_same =     0
      integer(kind = kint), parameter :: iflag_MESH_w_pole =   1
      integer(kind = kint), parameter :: iflag_MESH_w_center = 2
!
!
      character(len = kchara), parameter                                &
     &             :: label_half_Cbyv = 'half_Chebyshev'
      character(len = kchara), parameter                                &
     &             :: label_Chebyshev = 'Chebyshev'
      character(len = kchara), parameter                                &
     &             :: label_explicit =  'explicit'
      character(len = kchara), parameter                                &
     &             :: label_equi =      'equi_distance'
!
      character(len = kchara), parameter                                &
     &             :: label_no_FEMMESH =  'no_mesh'
      character(len = kchara), parameter                                &
     &             :: label_MESH_same =   'no_pole'
      character(len = kchara), parameter                                &
     &             :: label_MESH_pole =   'with_pole'
      character(len = kchara), parameter                                &
     &             :: label_MESH_ctr =    'with_center'
!
      end module m_spheric_constants
