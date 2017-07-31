!>@file   t_spheric_parameter.f90
!!@brief  module t_spheric_parameter
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for indexing table of speherical harmonics transform
!!
!!@verbatim
!!      iflag_radial_grid:: radial grid type
!!        igrid_Chebyshev =    2 :: Chebyshev collocation points
!!        igrid_non_equidist = 1 :: non-equi-distance
!!        igrid_equidistance = 0 :: equi-distance
!!
!!      subroutine alloc_type_spheric_parameter(sph)
!!        type(sph_grids), intent(inout) :: sph
!!      subroutine dealloc_type_spheric_parameter(sph)
!!        type(sph_grids), intent(inout) :: sph
!!
!!      subroutine check_global_spheric_parameter(sph_params, sph_rtp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in)  :: sph_rtp
!!      subroutine check_type_spheric_para_gl_part(sph)
!!        type(sph_grids), intent(in) :: sph
!!      subroutine check_type_spheric_parameter(my_rank, sph)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_grids), intent(in) :: sph
!!@endverbatim
!!
!!@n @param  my_rank     Running rank ID
!!
      module t_spheric_parameter
!
      use m_precision
      use m_spheric_constants
!
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
!
      implicit none
!
!    Group names for spherical shell dynamos
!
!>      Group name for ICB
      character(len=kchara), parameter :: ICB_nod_grp_name = 'ICB'
!>      Group name for CMB
      character(len=kchara), parameter :: CMB_nod_grp_name = 'CMB'
!>      Group name for innermost radius
      character(len=kchara), parameter                                  &
     &                      :: CTR_nod_grp_name = 'to_Center'
!
!>      Element Group name for inner core
      character(len=kchara), parameter                                  &
     &                      :: IC_ele_grp_name = 'inner_core'
!>      Element Group name for outer core
      character(len=kchara), parameter                                  &
     &                      :: OC_ele_grp_name = 'outer_core'
!>      Element Group name for outer core
      character(len=kchara), parameter                                  &
     &                      :: MT_ele_grp_name = 'external'
!
!>      Surface Group name for ICB
      character(len=kchara), parameter :: ICB_sf_grp_name = 'ICB_surf'
!>      Surface Group name for CMB
      character(len=kchara), parameter :: CMB_sf_grp_name = 'CMB_surf'
!>      Group name for innermost radius
      character(len=kchara), parameter                                  &
     &                      :: CTR_sf_grp_name = 'to_Center_surf'
!
!>  Structure for spherical shell paramteres
      type sph_shell_parameters
!>        integer flag for FEM mesh type
!!@n      iflag_MESH_same:     same grid point as Gauss-Legendre points
!!@n      iflag_MESH_w_pole:   Gauss-Legendre points with poles
!!@n      iflag_MESH_w_center: Gauss-Legendre points with center and poles
        integer (kind=kint) :: iflag_shell_mode =   iflag_MESH_same
!>        radial grid type flag
!!@n      igrid_Chebyshev =    2 :: Chebyshev collocation points
!!@n      igrid_non_equidist = 1 :: non-equi-distance
!!@n      igrid_equidistance = 0 :: equi-distance
        integer (kind=kint) :: iflag_radial_grid =  igrid_non_equidist
!
!>        Truncation for spherical harmonics
        integer(kind = kint) :: l_truncation
!>        m-folding symmetry for longitudinal direction
        integer(kind = kint) :: m_folding = 1
!
!    global parameteres for radius
!
!>        global radial ID for innermost point
        integer(kind = kint) :: nlayer_2_center
!>        global radial ID for ICB
        integer(kind = kint) :: nlayer_ICB
!>        global radial ID for CMB
        integer(kind = kint) :: nlayer_CMB
!>        global radial ID for mid-depth of the outer core
        integer(kind = kint) :: nlayer_mid_OC
!
!>        radius for ICB @f$ r_{i} @f$
        real(kind = kreal) :: radius_ICB
!>        radius for CMB @f$ r_{o} @f$
        real(kind = kreal) :: radius_CMB
!>        Earth's radius @f$ Re @f$
        real(kind = kreal) :: R_earth(0:2)
      end type sph_shell_parameters
!
!
!>  Structure of grid and spectr data for spherical spectr method
      type sph_grids
!>  Structure of grid and spectr data for spherical spectr method
        type(sph_shell_parameters) :: sph_params
!
!>        structure of index table for @f$ f(r,\theta,\phi) @f$
        type(sph_rtp_grid) :: sph_rtp
!>        structure of index table for @f$ f(r,\theta,m) @f$
        type(sph_rtm_grid) :: sph_rtm
!>        structure of index table for @f$ f(r,l,m) @f$
        type(sph_rlm_grid) :: sph_rlm
!>        structure of index table for @f$ f(r,j) @f$
        type(sph_rj_grid) ::  sph_rj
      end type sph_grids
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_parameter(sph)
!
      type(sph_grids), intent(inout) :: sph
!
!
      call alloc_type_spheric_param_rtp(sph%sph_rtp)
      call alloc_type_spheric_param_rtm(sph%sph_rtm)
      call alloc_type_spheric_param_rlm(sph%sph_rlm)
      call alloc_type_spheric_param_rj(sph%sph_rj)
!
      end subroutine alloc_type_spheric_parameter
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_spheric_parameter(sph)
!
      type(sph_grids), intent(inout) :: sph
!
!
      call dealloc_type_spheric_param_rtp(sph%sph_rtp)
      call dealloc_type_spheric_param_rtm(sph%sph_rtm)
      call dealloc_type_spheric_param_rlm(sph%sph_rlm)
      call dealloc_spheric_param_rj(sph%sph_rj)
!
      end subroutine dealloc_type_spheric_parameter
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_global_spheric_parameter(sph_params, sph_rtp)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in)  :: sph_rtp
!
!
      write(*,*) 'truncation degree:        ', sph_params%l_truncation
      write(*,*) 'm-folding symmetry:       ', sph_params%m_folding
      write(*,*) 'number of grid for f(r,t,p): ',                       &
     &            sph_rtp%nidx_global_rtp(1:3)
!
      end subroutine check_global_spheric_parameter
!
! ----------------------------------------------------------------------
!
      subroutine check_type_spheric_para_gl_part(sph)
!
      type(sph_grids), intent(in) :: sph
!
      write(50,*) 'l_truncation ', sph%sph_params%l_truncation
      write(50,*) 'm_folding ',    sph%sph_params%m_folding
      write(50,*) 'nidx_global_rtm ', sph%sph_rtm%nidx_global_rtm(1:3)
      write(50,*) 'nidx_global_rlm ', sph%sph_rlm%nidx_global_rlm(1:2)
      write(50,*) 'nidx_global_rj ',  sph%sph_rj%nidx_global_rj(1:2)
!
!
      end subroutine check_type_spheric_para_gl_part
!
! ----------------------------------------------------------------------
!
      subroutine check_type_spheric_parameter(my_rank, sph)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_grids), intent(in) :: sph
!
      call check_type_spheric_param_rtp(my_rank, sph%sph_rtp)
      call check_type_spheric_param_rtm(my_rank, sph%sph_rtm)
      call check_type_spheric_param_rlm(my_rank, sph%sph_rlm)
      call check_type_spheric_param_rj(my_rank, sph%sph_rj)
!
      end subroutine check_type_spheric_parameter
!
! -----------------------------------------------------------------------
!
      end module t_spheric_parameter
