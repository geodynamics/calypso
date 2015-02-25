!>@file   m_spheric_parameter.f90
!!@brief  module m_spheric_parameter
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine allocate_spheric_parameter
!!      subroutine deallocate_spheric_parameter
!!
!!      subroutine allocate_radius_1d_gl
!!      subroutine allocate_spheric_param_rtp
!!      subroutine allocate_spheric_param_rtm
!!      subroutine allocate_spheric_param_rlm
!!      subroutine allocate_spheric_param_rj
!!
!!      subroutine allocate_sph_1d_index_rtp
!!      subroutine allocate_sph_1d_index_rtm
!!      subroutine allocate_sph_1d_index_rlm
!!      subroutine allocate_sph_1d_index_rj
!!
!!      subroutine deallocate_radius_1d_gl
!!      subroutine deallocate_spheric_param_rtp
!!      subroutine deallocate_spheric_param_rtm
!!      subroutine deallocate_spheric_param_rlm
!!      subroutine deallocate_spheric_param_rj
!!
!!      subroutine deallocate_sph_1d_index_rtp
!!      subroutine deallocate_sph_1d_index_rtm
!!      subroutine deallocate_sph_1d_index_rlm
!!      subroutine deallocate_sph_1d_index_rj
!!
!!      subroutine check_global_spheric_parameter
!!      subroutine check_spheric_parameter(my_rank)
!!      subroutine check_spheric_param_rtp(my_rank)
!!      subroutine check_spheric_param_rtm(my_rank)
!!      subroutine check_spheric_param_rlm(my_rank)
!!      subroutine check_spheric_param_rj(my_rank)
!!
!!      integer(kind = kint) function find_local_sph_mode_address(l, m)
!!        integer(kind = 4), intent(in) :: l, m
!!      integer(kind = kint) function local_sph_data_address(kr, j_lc)
!!
!!@endverbatim
!!
!!@n @param  my_rank     Running rank ID
!!@n @param   l          Sphrical harmonics degree
!!@n @param   m          Sphrical harmonics order
!!
!
      module m_spheric_parameter
!
      use m_precision
      use m_spheric_constants
!
      implicit none
!
!>      integer flag for FEM mesh type
!!@n    iflag_MESH_same:     same grid point as Gauss-Legendre points
!!@n    iflag_MESH_w_pole:   Gauss-Legendre points with poles
!!@n    iflag_MESH_w_center: Gauss-Legendre points with center and poles
      integer (kind=kint) :: iflag_shell_mode =  iflag_MESH_same
!>      integer flag for center point in spectr data
!!@n    This flag should have same value for all processes
!!@n    0: No center point
!!@n    1: Center point is there
      integer (kind=kint) :: iflag_rj_center =  0
!>      radial grid type flag
!!@n    igrid_Chebyshev =    2 :: Chebyshev collocation points
!!@n    igrid_non_euqidist = 1 :: non-equi-distance
!!@n    igrid_euqidistance = 0 :: equi-distance
      integer (kind=kint) :: iflag_radial_grid = igrid_non_euqidist
!
!>      local spectr index for @f$ l = m = 0 @f$ at center
!!@n    if center does not exist in subdomain, inod_rj_center = 0.
      integer (kind=kint) :: inod_rj_center =   0
!
!>      local spectr index for @f$ l = m = 0 @f$
!!@n    If @f$ l = m = 0 @f$ mode does not exist in subdomain, 
!!@n    idx_rj_degree_zero = 0.
      integer (kind=kint) :: idx_rj_degree_zero =   0
!
!>        local spectr index for @f$ l = 1@f$ and  @f$ m = -1, 0, 1@f$.
!!        for @f$ f(r,j) @f$
!!@n        If spectr data do not exist in subdomain,
!!@n        idx_rj_degree_one(m) = 0.
      integer (kind=kint) :: idx_rj_degree_one(-1:1) = (/0,0,0/)
!
!>      Start address for @f$ m = 0 @f$ for @f$ f(r,\theta,m) @f$
      integer (kind=kint) :: ist_rtm_order_zero = 0
!>      Start address for @f$ l=1, m=-1 @f$ for @f$ f(r,\theta,m) @f$
      integer (kind=kint) :: ist_rtm_order_1s =   0
!>      Start address for @f$ l=1, m= 1 @f$ for @f$ f(r,\theta,m) @f$
      integer (kind=kint) :: ist_rtm_order_1c =   0
!
!>      local spectr index for @f$ l = 1@f$ and  @f$ m = -1, 0, 1@f$.
!!@n    If spectr data do not exist in subdomain,
!!@n    idx_rj_degree_one(m) = 0.
      integer(kind = kint) :: l_truncation

!    global parameteres for radius
!
!>      global radial ID for innermost point
      integer(kind = kint) :: nlayer_2_center
!>      global radial ID for ICB
      integer(kind = kint) :: nlayer_ICB
!>      global radial ID for CMB
      integer(kind = kint) :: nlayer_CMB
!>      global radial ID for mid-depth of the outer core
      integer(kind = kint) :: nlayer_mid_OC
!
!>      radius for ICB @f$ r_{i} @f$
      real(kind = kreal) :: r_ICB
!>      radius for CMB @f$ r_{o} @f$
      real(kind = kreal) :: r_CMB
!>      Earth's radius @f$ Re @f$
      real(kind = kreal) :: R_earth(0:2)
!
!>      global radius data @f$ r(k) @f$
      real(kind = kreal), allocatable :: radius_1d_gl(:)
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
!
!>      Surface Group name for ICB
      character(len=kchara), parameter :: ICB_sf_grp_name = 'ICB_surf'
!>      Surface Group name for CMB
      character(len=kchara), parameter :: CMB_sf_grp_name = 'CMB_surf'
!>      Group name for innermost radius
      character(len=kchara), parameter                                  &
     &                      :: CTR_sf_grp_name = 'to_Center_surf'
!
!   global parameters
!
!>      number of global 1d data points for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: nidx_global_rtp(3)
!>      number of global 1d data points for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: nidx_global_rtm(3)
!>      number of global 1d data points for @f$ f(r,l,m) @f$
      integer(kind = kint) :: nidx_global_rlm(2)
!>      number of global 1d data points for @f$ f(r,j) @f$
      integer(kind = kint) :: nidx_global_rj(2)
!
!>      number of subdomains
      integer(kind = kint) :: ndomain_sph
!>      number of 1d subdomains for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: ndomain_rtp(3)
!>      number of 1d subdomains for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: ndomain_rtm(3)
!>      number of 1d subdomains for @f$ f(r,l,m) @f$
      integer(kind = kint) :: ndomain_rlm(2)
!>      number of 1d subdomains for @f$ f(r,j) @f$
      integer(kind = kint) :: ndomain_rj(2)
!
!>      1d start address of global data for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: ist_rtp(3)
!>      1d start address of global data for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: ist_rtm(3)
!>      1d start address of global data for @f$ f(r,l,m) @f$
      integer(kind = kint) :: ist_rlm(2)
!>      1d start address of global data for @f$ f(r,j) @f$
      integer(kind = kint) :: ist_rj(2)
!
!>      1d end address of global data for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: ied_rtp(3)
!>      1d end address of global data for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: ied_rtm(3)
!>      1d end address of global data for @f$ f(r,l,m) @f$
      integer(kind = kint) :: ied_rlm(2)
!>      1d end address of global data for @f$ f(r,j) @f$
      integer(kind = kint) :: ied_rj(2)
!
!    local parameters
!
!>      1d subdomain ID for @f$ f(r,\theta,\phi) @f$ (start from 0)
      integer(kind = kint) :: sph_rank_rtp(3)
!>      1d subdomain ID for @f$ f(r,\theta,m) @f$ (start from 0)
      integer(kind = kint) :: sph_rank_rtm(3)
!>      1d subdomain ID for @f$ f(r,l,m) @f$ (start from 0)
      integer(kind = kint) :: sph_rank_rlm(2)
!>      1d subdomain ID for @f$ f(r,j) @f$ (start from 0)
      integer(kind = kint) :: sph_rank_rj(2)
!
!>      number of data points for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: nnod_rtp
!>      number of data points for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: nnod_rtm
!>      number of data points for @f$ f(r,l,m) @f$
      integer(kind = kint) :: nnod_rlm
!>      number of data points for @f$ f(r,j) @f$
      integer(kind = kint) :: nnod_rj
!
!>      number of data points for poles
      integer(kind = kint) :: nnod_rtp_pole
!
!>      number of 1d data points for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: nidx_rtp(3)
!>      number of 1d data points for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: nidx_rtm(3)
!>      number of 1d data points for @f$ f(r,l,m) @f$
      integer(kind = kint) :: nidx_rlm(2)
!>      number of 1d data points for @f$ f(r,j) @f$
      integer(kind = kint) :: nidx_rj(2)
!
!>      number of increments for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: istep_rtp(3)
!>      number of increments for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: istep_rtm(3)
!>      number of increments for @f$ f(r,l,m) @f$
      integer(kind = kint) :: istep_rlm(2)
!>      number of increments for @f$ f(r,j) @f$
      integer(kind = kint) :: istep_rj(2)
!
!
!>      global address for each direction @f$ f(r,\theta,\phi) @f$
      integer(kind = kint), allocatable :: idx_global_rtp(:,:)
!>      global address for each direction @f$ f(r,\theta,m) @f$
      integer(kind = kint), allocatable :: idx_global_rtm(:,:)
!>      global address for each direction @f$ f(r,l,m) @f$
      integer(kind = kint), allocatable :: idx_global_rlm(:,:)
!>      global address for each direction @f$ f(r,j) @f$
      integer(kind = kint), allocatable :: idx_global_rj(:,:)
!
!
!>      radial global address @f$ f(r,\theta,\phi) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rtp_r(:)
!>      meridional global address @f$ f(r,\theta,\phi) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rtp_t(:)
!>      zonal global address @f$ f(r,\theta,\phi) @f$
!!@n        idx_gl_1d_rtp_p(m,1): zonal grid point ID
!!@n        idx_gl_1d_rtp_p(m,2): Fourier spectr mode
      integer(kind = kint), allocatable :: idx_gl_1d_rtp_p(:,:)
!
!>      radial global address @f$ f(r,\theta,m) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rtm_r(:)
!>      meridional global address @f$ f(r,\theta,m) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rtm_t(:)
!>      zonal global address @f$ f(r,\theta,m) @f$
!!@n        idx_gl_1d_rtm_m(m,1): global ID for Fourier transform
!!@n        idx_gl_1d_rtm_m(m,2): Fourier spectr mode
      integer(kind = kint), allocatable :: idx_gl_1d_rtm_m(:,:)
!
!>      radial global address @f$ f(r,l,m) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rlm_r(:)
!>      spherical harmonics mode for  @f$ f(r,l,m) @f$
!!@n        idx_gl_1d_rlm_j(j,1): global ID for spherical harmonics
!!@n        idx_gl_1d_rlm_j(j,2): spherical hermonincs degree
!!@n        idx_gl_1d_rlm_j(j,3): spherical hermonincs order
      integer(kind = kint), allocatable :: idx_gl_1d_rlm_j(:,:)
!
!>      radial global address @f$ f(r,j) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rj_r(:)
!>      spherical harmonics mode for  @f$ f(r,j) @f$
!!@n        idx_gl_1d_rj_j(j,1): global ID for spherical harmonics
!!@n        idx_gl_1d_rj_j(j,2): spherical hermonincs degree
!!@n        idx_gl_1d_rj_j(j,3): spherical hermonincs order
      integer(kind = kint), allocatable :: idx_gl_1d_rj_j(:,:)
!
!>      1d radius data for @f$ f(r,\theta,\phi) @f$
      real(kind = kreal), allocatable :: radius_1d_rtp_r(:)
!>      1d radius data for @f$ f(r,\theta,m) @f$
      real(kind = kreal), allocatable :: radius_1d_rtm_r(:)
!>      1d radius data for @f$ f(r,l,m) @f$
      real(kind = kreal), allocatable :: radius_1d_rlm_r(:)
!>      1d radius data for @f$ f(r,j) @f$
      real(kind = kreal), allocatable :: radius_1d_rj_r(:)
!
!>      1d @f$1 / r @f$ for @f$ f(r,\theta,\phi) @f$
      real(kind = kreal), allocatable :: a_r_1d_rtp_r(:)
!>      1d @f$1 / r @f$ for @f$ f(r,\theta,m) @f$
      real(kind = kreal), allocatable :: a_r_1d_rtm_r(:)
!>      1d @f$1 / r @f$ for @f$ f(r,l,m) @f$
      real(kind = kreal), allocatable :: a_r_1d_rlm_r(:)
!>      1d @f$1 / r @f$ for @f$ f(r,j) @f$
      real(kind = kreal), allocatable :: a_r_1d_rj_r(:)
!
!>      1d @f$1 / r @f$ for @f$ f(r,j) @f$
!!@n@see  set_radius_func_cheby or set_radius_func_cheby
      real(kind = kreal), allocatable :: ar_1d_rj(:,:)
!
!>      1d radius between grids for @f$ f(r,j) @f$
      real(kind = kreal), allocatable :: r_ele_rj(:)
!>      1d @f$1 / r @f$ between grids for @f$ f(r,j) @f$
      real(kind = kreal), allocatable :: ar_ele_rj(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_spheric_parameter
!
      call allocate_spheric_param_rtp
      call allocate_spheric_param_rtm
      call allocate_spheric_param_rlm
      call allocate_spheric_param_rj
!
      end subroutine allocate_spheric_parameter
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_parameter
!
      call deallocate_spheric_param_rtp
      call deallocate_spheric_param_rtm
      call deallocate_spheric_param_rlm
      call deallocate_spheric_param_rj
!
      end subroutine deallocate_spheric_parameter
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_radius_1d_gl
!
      integer(kind = kint) :: num
!
      num = nidx_global_rtp(1)
      allocate(radius_1d_gl(num))
      if(num .gt. 0) radius_1d_gl = 0.0d0
!
      end subroutine allocate_radius_1d_gl
!
! ----------------------------------------------------------------------
!
      subroutine allocate_spheric_param_rtp
!
      allocate(idx_global_rtp(nnod_rtp,3))
      if(nnod_rtp .gt. 0) idx_global_rtp = 0
!
      end subroutine allocate_spheric_param_rtp
!
! ----------------------------------------------------------------------
!
      subroutine allocate_spheric_param_rtm
!
      allocate(idx_global_rtm(nnod_rtm,3))
      if(nnod_rtm .gt. 0) idx_global_rtm = 0
!
      end subroutine allocate_spheric_param_rtm
!
! ----------------------------------------------------------------------
!
      subroutine allocate_spheric_param_rlm
!
      allocate(idx_global_rlm(nnod_rlm,2))
      if(nnod_rlm .gt. 0) idx_global_rlm = 0
!
      end subroutine allocate_spheric_param_rlm
!
! ----------------------------------------------------------------------
!
      subroutine allocate_spheric_param_rj
!
      allocate(idx_global_rj(nnod_rj,2))
      if(nnod_rj .gt. 0) idx_global_rj =  0
!
      end subroutine allocate_spheric_param_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_index_rtp
!
      integer(kind = kint) :: num
!
      num = nidx_rtp(1)
      allocate(idx_gl_1d_rtp_r(num))
      allocate(radius_1d_rtp_r(num))
      allocate(a_r_1d_rtp_r(num))
      num = nidx_rtp(2)
      allocate(idx_gl_1d_rtp_t(num))
      num = nidx_rtp(3)
      allocate(idx_gl_1d_rtp_p(num,2))
!
      if(nidx_rtp(3) .gt. 0) idx_gl_1d_rtp_p = 0
      if(nidx_rtp(2) .gt. 0) idx_gl_1d_rtp_t = 0
      if(nidx_rtp(1) .gt. 0) then
        idx_gl_1d_rtp_r = 0
        radius_1d_rtp_r = 0.0d0
        a_r_1d_rtp_r = 0.0d0
      end if
!
      end subroutine allocate_sph_1d_index_rtp
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_index_rtm
!
      integer(kind = kint) :: num
!
      num = nidx_rtm(1)
      allocate(idx_gl_1d_rtm_r(num))
      allocate(radius_1d_rtm_r(num))
      allocate(a_r_1d_rtm_r(num))
      num = nidx_rtm(2)
      allocate(idx_gl_1d_rtm_t(num))
      num = nidx_rtm(3)
      allocate(idx_gl_1d_rtm_m(num,2))
!
      if(nidx_rtm(3) .gt. 0) idx_gl_1d_rtm_m = 0
      if(nidx_rtm(2) .gt. 0) idx_gl_1d_rtm_t = 0
      if(nidx_rtm(1) .gt. 0) then
        idx_gl_1d_rtm_r = 0
        radius_1d_rtm_r = 0.0d0
        a_r_1d_rtm_r = 0.0d0
      end if
!
      end subroutine allocate_sph_1d_index_rtm
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_index_rlm
!
      integer(kind = kint) :: num
!
      num = nidx_rlm(1)
      allocate(idx_gl_1d_rlm_r(num))
      allocate(radius_1d_rlm_r(num))
      allocate(a_r_1d_rlm_r(num))
      num = nidx_rlm(2)
      allocate(idx_gl_1d_rlm_j(num,3))
!
      if(nidx_rlm(2) .gt. 0) idx_gl_1d_rlm_j = 0
      if(nidx_rlm(1) .gt. 0) then
        idx_gl_1d_rlm_r = 0
        radius_1d_rlm_r = 0.0d0
        a_r_1d_rlm_r =    0.0d0
      end if
!
      end subroutine allocate_sph_1d_index_rlm
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_index_rj
!
      integer(kind = kint) :: num
!
      num = nidx_rj(1)
      allocate(idx_gl_1d_rj_r(num))
      allocate(radius_1d_rj_r(num))
      allocate(a_r_1d_rj_r(num))
!
      allocate(ar_1d_rj(num,3))
      allocate(r_ele_rj(num))
      allocate(ar_ele_rj(num,3))
!
      num = nidx_rj(2)
      allocate(idx_gl_1d_rj_j(num,3))
!
      if(nidx_rj(2) .gt. 0) idx_gl_1d_rj_j = 0
      if(nidx_rj(1) .gt. 0) then
        idx_gl_1d_rj_r = 0
        radius_1d_rj_r = 0.0d0
        a_r_1d_rj_r = 0.0d0
!
        ar_1d_rj = 0.0d0
        r_ele_rj = 0.0d0
        ar_ele_rj = 0.0d0
      end if
!
      end subroutine allocate_sph_1d_index_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_radius_1d_gl
!
      deallocate(radius_1d_gl)
!
      end subroutine deallocate_radius_1d_gl
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_param_rtp
!
      deallocate(idx_global_rtp)
!
      end subroutine deallocate_spheric_param_rtp
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_param_rtm
!
      deallocate(idx_global_rtm)
!
      end subroutine deallocate_spheric_param_rtm
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_param_rlm
!
      deallocate(idx_global_rlm)
!
      end subroutine deallocate_spheric_param_rlm
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_param_rj
!
      deallocate(idx_global_rj)
!
      end subroutine deallocate_spheric_param_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_index_rtp
!
      deallocate(radius_1d_rtp_r)
      deallocate(a_r_1d_rtp_r)
      deallocate(idx_gl_1d_rtp_r)
      deallocate(idx_gl_1d_rtp_t)
      deallocate(idx_gl_1d_rtp_p)
!
      end subroutine deallocate_sph_1d_index_rtp
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_index_rtm
!
      deallocate(radius_1d_rtm_r)
      deallocate(a_r_1d_rtm_r)
      deallocate(idx_gl_1d_rtm_r)
      deallocate(idx_gl_1d_rtm_t)
      deallocate(idx_gl_1d_rtm_m)
!
      end subroutine deallocate_sph_1d_index_rtm
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_index_rlm
!
      deallocate(radius_1d_rlm_r)
      deallocate(a_r_1d_rlm_r)
      deallocate(idx_gl_1d_rlm_r)
      deallocate(idx_gl_1d_rlm_j)
!
      end subroutine deallocate_sph_1d_index_rlm
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_index_rj
!
      deallocate(radius_1d_rj_r)
      deallocate(a_r_1d_rj_r)
!
      deallocate(ar_1d_rj, r_ele_rj, ar_ele_rj)
!
      deallocate(idx_gl_1d_rj_r)
      deallocate(idx_gl_1d_rj_j)
!
      end subroutine deallocate_sph_1d_index_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_global_spheric_parameter
!
!
      write(*,*) 'truncation degree:           ', l_truncation
      write(*,*) 'number of grid for f(r,t,p): ', nidx_global_rtp(1:3)
      write(*,*) 'subdomain for f(r,t,p):      ', ndomain_rtp(1:3)
      write(*,*) 'subdomain for f(r,t,m):      ', ndomain_rtm(1:3)
      write(*,*) 'subdomain for f(r,l,m):      ', ndomain_rlm(1:2)
      write(*,*) 'subdomain for spectr f(r,j): ', ndomain_rj(1:2)
!
      end subroutine check_global_spheric_parameter
!
! ----------------------------------------------------------------------
!
      subroutine check_spheric_parameter(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      call check_spheric_param_rtp(my_rank)
      call check_spheric_param_rtm(my_rank)
      call check_spheric_param_rlm(my_rank)
      call check_spheric_param_rj(my_rank)
!
      end subroutine check_spheric_parameter
!
! -----------------------------------------------------------------------
!
      subroutine check_spheric_param_rtp(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rtp ', sph_rank_rtp(1:3)
      write(my_rank+50,*) 'nidx_rtp ', nidx_rtp(1:3)
      write(my_rank+50,*) 'nnod_rtp ', nnod_rtp
!
      write(my_rank+50,*)  'i, idx_global_rtp(r,t,p)'
      do i = 1, nnod_rtp
        write(my_rank+50,*) i, idx_global_rtp(i,1:3)
      end do
!
      end subroutine check_spheric_param_rtp
!
! -----------------------------------------------------------------------
!
      subroutine check_spheric_param_rtm(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rtm ', sph_rank_rtm(1:3)
      write(my_rank+50,*) 'nidx_rtm ', nidx_rtm(1:3)
      write(my_rank+50,*) 'nnod_rtm ', nnod_rtm
!
      write(my_rank+50,*) 'i, idx_global_rtm(r,t,p)'
      do i = 1, nnod_rtm
        write(my_rank+50,*) i, idx_global_rtm(i,1:3)
      end do
!
      end subroutine check_spheric_param_rtm
!
! -----------------------------------------------------------------------
!
      subroutine check_spheric_param_rlm(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rlm ', sph_rank_rlm(1:2)
      write(my_rank+50,*) 'nidx_rlm ', nidx_rlm(1:2)
      write(my_rank+50,*) 'nnod_rlm ', nnod_rlm
!
      write(my_rank+50,*) 'i, idx_global_rlm(r,j)'
      do i = 1, nnod_rlm
        write(my_rank+50,*) i, idx_global_rlm(i,1:2)
      end do
!
      end subroutine check_spheric_param_rlm
!
! -----------------------------------------------------------------------
!
      subroutine check_spheric_param_rj(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rj ',  sph_rank_rj(1:2)
      write(my_rank+50,*) 'nidx_rj  ',  nidx_rj(1:2)
      write(my_rank+50,*) 'nnod_rj ',  nnod_rj
!
      write(my_rank+50,*) 'i, idx_global_rj(r,j)'
      do i = 1, nnod_rj
        write(my_rank+50,*) i, idx_global_rj(i,1:2)
      end do
!
      end subroutine check_spheric_param_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function find_local_sph_mode_address(l, m)
!
      integer(kind = 4), intent(in) :: l, m
!
      integer(kind = kint) :: j
!
!
      find_local_sph_mode_address = 0
      do j = 1, nidx_rj(2)
        if (   int(idx_gl_1d_rj_j(j,2)) .eq. l                          &
     &   .and. int(idx_gl_1d_rj_j(j,3)) .eq. m) then
          find_local_sph_mode_address = j
          return
        end if
      end do
!
      end function find_local_sph_mode_address
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function local_sph_data_address(kr, j_lc)
!
      integer(kind = kint), intent(in) :: kr, j_lc
!
!
      local_sph_data_address = j_lc + (kr-1)*nidx_rj(2)
!
      end function local_sph_data_address
!
!-----------------------------------------------------------------------
!
      end module m_spheric_parameter
