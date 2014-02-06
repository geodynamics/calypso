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
!!        igrid_non_euqidist = 1 :: non-equi-distance
!!        igrid_euqidistance = 0 :: equi-distance
!!
!!      subroutine alloc_type_spheric_parameter(sph)
!!        type(sph_grids), intent(inout) :: sph
!!      subroutine dealloc_type_spheric_parameter(sph)
!!        type(sph_grids), intent(inout) :: sph
!!
!!      subroutine alloc_type_radius_1d_gl(sph)
!!        type(sph_grids), intent(inout) :: sph
!!      subroutine alloc_type_spheric_param_rtp(rtp)
!!        type(sph_rtp_grid), intent(inout) :: rtp
!!      subroutine alloc_type_spheric_param_rtm(rtm)
!!        type(sph_rtm_grid), intent(inout) :: rtm
!!      subroutine alloc_type_spheric_param_rlm(rlm)
!!        type(sph_rlm_grid), intent(inout) :: rlm
!!      subroutine alloc_type_spheric_param_rj(rj)
!!        type(sph_rj_grid), intent(inout) :: rj
!!
!!      subroutine alloc_type_sph_1d_index_rtp(rtp)
!!        type(sph_rtp_grid), intent(inout) :: rtp
!!      subroutine alloc_type_sph_1d_index_rtm(rtm)
!!        type(sph_rtm_grid), intent(inout) :: rtm
!!      subroutine alloc_type_sph_1d_index_rlm(rlm)
!!        type(sph_rlm_grid), intent(inout) :: rlm
!!      subroutine alloc_type_sph_1d_index_rj(rj)
!!        type(sph_rj_grid), intent(inout) :: rj
!!
!!      subroutine dealloc_type_radius_1d_gl(sph)
!!        type(sph_grids), intent(inout) :: sph
!!      subroutine dealloc_type_spheric_param_rtp(rtp)
!!        type(sph_rtp_grid), intent(inout) :: rtp
!!      subroutine dealloc_type_spheric_param_rtm(rtm)
!!        type(sph_rtm_grid), intent(inout) :: rtm
!!      subroutine dealloc_type_spheric_param_rlm(rlm)
!!        type(sph_rlm_grid), intent(inout) :: rlm
!!      subroutine dealloc_type_spheric_param_rj(rj)
!!        type(sph_rj_grid), intent(inout) :: rj
!!
!!      subroutine dealloc_type_sph_1d_index_rtp(rtp)
!!        type(sph_rtp_grid), intent(inout) :: rtp
!!      subroutine dealloc_type_sph_1d_index_rtm(rtm)
!!        type(sph_rtm_grid), intent(inout) :: rtm
!!      subroutine dealloc_type_sph_1d_index_rlm(rlm)
!!        type(sph_rlm_grid), intent(inout) :: rlm
!!      subroutine dealloc_type_sph_1d_index_rj(rj)
!!        type(sph_rj_grid), intent(inout) :: rj
!!
!!      subroutine check_type_spheric_para_gl_part(sph)
!!        type(sph_grids), intent(in) :: sph
!!      subroutine check_type_spheric_parameter(my_rank, sph)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_grids), intent(in) :: sph
!!      subroutine check_type_spheric_param_rtp(my_rank, rtp)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_rtp_grid), intent(in) :: rtp
!!      subroutine check_type_spheric_param_rtm(my_rank, rtm)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_rtm_grid), intent(in) :: rtm
!!      subroutine check_type_spheric_param_rlm(my_rank, rlm)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_rlm_grid), intent(in) :: rlm
!!      subroutine check_type_spheric_param_rj(my_rank, rj)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_rj_grid), intent(in) :: rj
!!@endverbatim
!!
!!@n @param  my_rank     Running rank ID
!!
      module t_spheric_parameter
!
      use m_precision
      use m_spheric_constants
!
      implicit none
!
!>        structure of index table for @f$ f(r,\theta,\phi) @f$
      type sph_rtp_grid
!>        number of global 1d data points for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: nidx_global_rtp(3)
!>        number of 1d subdomains for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: ndomain_rtp(3)
!>        1d subdomain ID for @f$ f(r,\theta,\phi) @f$ (start from 0)
        integer(kind = kint) :: sph_rank_rtp(3)
!
!>        number of data points for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: nnod_rtp
!>        number of data points for pole and center
        integer(kind = kint) :: nnod_rtp_pole
!
!>        number of 1d data points for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: nidx_rtp(3)
!>        1d start address of global data for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: ist_rtp(3)
!>        1d end address of global data for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: ied_rtp(3)
!
!>        global data address @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: inod_global_rtp(:)
!>        global address for each direction @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: idx_global_rtp(:,:)
!
!>        radial global address for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtp_r(:)
!>        meridional global address for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtp_t(:)
!>        zonal global address for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtp_p(:,:)
!
!>        1d radius data for @f$ f(r,\theta,\phi) @f$
        real(kind = kreal), pointer :: radius_1d_rtp_r(:)
!>        1 / radius_1d_rtp_r
        real(kind = kreal), pointer :: a_r_1d_rtp_r(:)
      end type sph_rtp_grid
!
!
!>        structure of index table for @f$ f(r,\theta,m) @f$
      type sph_rtm_grid
!>        number of global 1d data points for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: nidx_global_rtm(3)
!>        number of 1d subdomains for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: ndomain_rtm(3)
!>        1d subdomain ID for @f$ f(r,\theta,m) @f$ (start from 0)
        integer(kind = kint) :: sph_rank_rtm(3)
!
!>        number of data points for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: nnod_rtm
!>        number of 1d data points for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: nidx_rtm(3)
!>        1d start address of global data for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: ist_rtm(3)
!>        1d end address of global data for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: ied_rtm(3)
!
!>        global data address @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: inod_global_rtm(:)
!>        global address for each direction @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: idx_global_rtm(:,:)
!
!>        radial global address for @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtm_r(:)
!>        meridional global address for @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtm_t(:)
!>        Zonal wave number for @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtm_m(:,:)
!
!>        1d radius data for @f$ f(r,\theta,m) @f$
        real(kind = kreal), pointer :: radius_1d_rtm_r(:)
!>        1 / radius_1d_rtm_r
        real(kind = kreal), pointer :: a_r_1d_rtm_r(:)
      end type sph_rtm_grid
!
!
!>        structure of index table for @f$ f(r,l,m) @f$
      type sph_rlm_grid
!>        number of global 1d data points for @f$ f(r,l,m) @f$
        integer(kind = kint) :: nidx_global_rlm(2)
!>        number of 1d subdomains for @f$ f(r,l,m) @f$
        integer(kind = kint) :: ndomain_rlm(2)
!>        1d subdomain ID for @f$ f(r,l,m) @f$ (start from 0)
        integer(kind = kint) :: sph_rank_rlm(2)
!
!>        number of data points for @f$ f(r,l,m) @f$
        integer(kind = kint) :: nnod_rlm
!>        number of 1d data points for @f$ f(r,l,m) @f$
        integer(kind = kint) :: nidx_rlm(2)
!>        1d start address of global data for @f$ f(r,l,m) @f$
        integer(kind = kint) :: ist_rlm(2)
!>        1d end address of global data for @f$ f(r,l,m) @f$
        integer(kind = kint) :: ied_rlm(2)
!
!>        global data address @f$ f(r,l,m) @f$
        integer(kind = kint), pointer :: inod_global_rlm(:)
!>        global address for each direction @f$ f(r,l,m) @f$
        integer(kind = kint), pointer :: idx_global_rlm(:,:)
!
!>        radial global address for @f$ f(r,l,m) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rlm_r(:)
!>        spherical harmonics mode for  @f$ f(r,l,m) @f$
!!@n        idx_gl_1d_rj_j(j,1): global ID for spherical harmonics
!!@n        idx_gl_1d_rj_j(j,2): spherical hermonincs degree
!!@n        idx_gl_1d_rj_j(j,3): spherical hermonincs order
        integer(kind = kint), pointer :: idx_gl_1d_rlm_j(:,:)
!
!>        1d radius data for @f$ f(r,l,m) @f$
        real(kind = kreal), pointer :: radius_1d_rlm_r(:)
!>        1 / radius_1d_rlm_r
        real(kind = kreal), pointer :: a_r_1d_rlm_r(:)
      end type sph_rlm_grid
!
!
!>        structure of index table for @f$ f(r,j) @f$
      type sph_rj_grid
!>        number of global 1d data points for @f$ f(r,j) @f$
        integer(kind = kint) :: nidx_global_rj(2)
!>        number of 1d subdomains for @f$ f(r,j) @f$
        integer(kind = kint) :: ndomain_rj(2)
!>        1d subdomain ID for @f$ f(r,j) @f$ (start from 0)
        integer(kind = kint) :: sph_rank_rj(2)
!
!>        local spectr index for @f$ l = m = 0 @f$
!!@n      If @f$ l = m = 0 @f$ mode does not exist in subdomain, 
!!@n      idx_rj_degree_zero = 0.
        integer (kind=kint) :: idx_rj_degree_zero
!>        local spectr index for @f$ l = 1@f$ and  @f$ m = -1, 0, 1@f$.
!!        for @f$ f(r,j) @f$
!!@n        If spectr data do not exist in subdomain,
!!@n        idx_rj_degree_one(m) = 0.
        integer (kind=kint) :: idx_rj_degree_one(-1:1)
!
!>        Start address for @f$ m = 0 @f$ for @f$ f(r,\theta,m) @f$
        integer (kind=kint) :: ist_rtm_order_zero =   0
!
!>        number of data points for @f$ f(r,j) @f$
        integer(kind = kint) :: nnod_rj
!>        number of 1d data points for @f$ f(r,j) @f$
        integer(kind = kint) :: nidx_rj(2)
!>        1d start address of global data for @f$ f(r,j) @f$
        integer(kind = kint) :: ist_rj(2)
!>        1d end address of global data for @f$ f(r,j) @f$
        integer(kind = kint) :: ied_rj(2)
!
!>        global data address @f$ f(r,j) @f$
        integer(kind = kint), pointer :: inod_global_rj(:)
!>        global address for each direction @f$ f(r,j) @f$
        integer(kind = kint), pointer :: idx_global_rj(:,:)
!
!>        radial global address @f$ f(r,j) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rj_r(:)
!>        spherical harmonics mode for  @f$ f(r,j) @f$
!!@n        idx_gl_1d_rj_j(j,1): global ID for spherical harmonics
!!@n        idx_gl_1d_rj_j(j,2): spherical hermonincs degree
!!@n        idx_gl_1d_rj_j(j,3): spherical hermonincs order
        integer(kind = kint), pointer :: idx_gl_1d_rj_j(:,:)
!
!>        1d radius data for @f$ f(r,j) @f$
        real(kind = kreal), pointer :: radius_1d_rj_r(:)
!>        1d @f$1 / r @f$ for @f$ f(r,j) @f$
        real(kind = kreal), pointer :: a_r_1d_rj_r(:)
!
!>        1d @f$1 / r @f$ for @f$ f(r,j) @f$
!!@n@see  set_radius_func_cheby or set_radius_func_cheby
        real(kind = kreal), pointer :: ar_1d_rj(:,:)
!
!>        1d radius between grids for @f$ f(r,j) @f$
        real(kind = kreal), pointer :: r_ele_rj(:)
!>        1d @f$1 / r @f$ between grids for @f$ f(r,j) @f$
        real(kind = kreal), pointer :: ar_ele_rj(:,:)
      end type sph_rj_grid
!
!
!>  Structure of grid and spectr data for spherical spectr method
      type sph_grids
!>        integer flag for FEM mesh type
!!@n      iflag_MESH_same:     same grid point as Gauss-Legendre points
!!@n      iflag_MESH_w_pole:   Gauss-Legendre points with poles
!!@n      iflag_MESH_w_center: Gauss-Legendre points with center and poles
        integer (kind=kint) :: iflag_shell_mode =   iflag_MESH_same
!>        radial grid type flag
!!@n      igrid_Chebyshev =    2 :: Chebyshev collocation points
!!@n      igrid_non_euqidist = 1 :: non-equi-distance
!!@n      igrid_euqidistance = 0 :: equi-distance
        integer (kind=kint) :: iflag_radial_grid =  igrid_non_euqidist
!
!>        number of subdomains
        integer(kind = kint) :: ndomain_sph
!>        local spectr index for @f$ l = 1@f$ and  @f$ m = -1, 0, 1@f$.
!!@n      If spectr data do not exist in subdomain,
!!@n      idx_rj_degree_one(m) = 0.
        integer(kind = kint) :: l_truncation
!>        global radius data @f$ r(k) @f$
        real(kind = kreal), pointer :: radius_1d_gl(:)
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
      call dealloc_type_spheric_param_rj(sph%sph_rj)
!
      end subroutine dealloc_type_spheric_parameter
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_type_radius_1d_gl(sph)
!
      type(sph_grids), intent(inout) :: sph
      integer(kind = kint) :: num
!
      num = sph%sph_rtp%nidx_global_rtp(1)
      allocate(sph%radius_1d_gl(num))
      if(num .gt. 0) sph%radius_1d_gl = 0.0d0
!
      end subroutine alloc_type_radius_1d_gl
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_param_rtp(rtp)
!
      type(sph_rtp_grid), intent(inout) :: rtp
!
      allocate(rtp%inod_global_rtp(rtp%nnod_rtp))
      allocate(rtp%idx_global_rtp(rtp%nnod_rtp,3))
!
      if(rtp%nnod_rtp .gt. 0) then
        rtp%inod_global_rtp = 0
        rtp%idx_global_rtp = 0
      end if
!
      end subroutine alloc_type_spheric_param_rtp
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_param_rtm(rtm)
!
      type(sph_rtm_grid), intent(inout) :: rtm
!
      allocate(rtm%inod_global_rtm(rtm%nnod_rtm))
      allocate(rtm%idx_global_rtm(rtm%nnod_rtm,3))
!
      if(rtm%nnod_rtm .gt. 0) then
        rtm%inod_global_rtm = 0
        rtm%idx_global_rtm = 0
      end if
!
      end subroutine alloc_type_spheric_param_rtm
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_param_rlm(rlm)
!
      type(sph_rlm_grid), intent(inout) :: rlm
!
      allocate(rlm%inod_global_rlm(rlm%nnod_rlm))
      allocate(rlm%idx_global_rlm(rlm%nnod_rlm,2))
!
      if(rlm%nnod_rlm .gt. 0) then
        rlm%inod_global_rlm = 0
        rlm%idx_global_rlm = 0
      end if
!
      end subroutine alloc_type_spheric_param_rlm
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_param_rj(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
!
      allocate(rj%inod_global_rj(rj%nnod_rj))
      allocate(rj%idx_global_rj(rj%nnod_rj,2))
!
      if(rj%nnod_rj .gt. 0) then
        rj%inod_global_rj =  0
        rj%idx_global_rj =  0
      end if
!
      end subroutine alloc_type_spheric_param_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_type_sph_1d_index_rtp(rtp)
!
      type(sph_rtp_grid), intent(inout) :: rtp
      integer(kind = kint) :: num
!
      num = rtp%nidx_rtp(1)
      allocate(rtp%idx_gl_1d_rtp_r(num))
      allocate(rtp%radius_1d_rtp_r(num))
      allocate(rtp%a_r_1d_rtp_r(num))
      num = rtp%nidx_rtp(2)
      allocate(rtp%idx_gl_1d_rtp_t(num))
      num = rtp%nidx_rtp(3)
      allocate(rtp%idx_gl_1d_rtp_p(num,2))
!
      if(rtp%nidx_rtp(3) .gt. 0) rtp%idx_gl_1d_rtp_p = 0
      if(rtp%nidx_rtp(2) .gt. 0) rtp%idx_gl_1d_rtp_t = 0
      if(rtp%nidx_rtp(1) .gt. 0) then
        rtp%idx_gl_1d_rtp_r = 0
        rtp%radius_1d_rtp_r = 0.0d0
        rtp%a_r_1d_rtp_r = 0.0d0
      end if
!
      end subroutine alloc_type_sph_1d_index_rtp
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_sph_1d_index_rtm(rtm)
!
      type(sph_rtm_grid), intent(inout) :: rtm
      integer(kind = kint) :: num
!
      num = rtm%nidx_rtm(1)
      allocate(rtm%idx_gl_1d_rtm_r(num))
      allocate(rtm%radius_1d_rtm_r(num))
      allocate(rtm%a_r_1d_rtm_r(num))
      num = rtm%nidx_rtm(2)
      allocate(rtm%idx_gl_1d_rtm_t(num))
      num = rtm%nidx_rtm(3)
      allocate(rtm%idx_gl_1d_rtm_m(num,2))
!
      if(rtm%nidx_rtm(3) .gt. 0) rtm%idx_gl_1d_rtm_m = 0
      if(rtm%nidx_rtm(2) .gt. 0) rtm%idx_gl_1d_rtm_t = 0
      if(rtm%nidx_rtm(1) .gt. 0) then
        rtm%idx_gl_1d_rtm_r = 0
        rtm%radius_1d_rtm_r = 0.0d0
        rtm%a_r_1d_rtm_r = 0.0d0
      end if
!
      end subroutine alloc_type_sph_1d_index_rtm
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_sph_1d_index_rlm(rlm)
!
      type(sph_rlm_grid), intent(inout) :: rlm
      integer(kind = kint) :: num
!
      num = rlm%nidx_rlm(1)
      allocate(rlm%idx_gl_1d_rlm_r(num))
      allocate(rlm%radius_1d_rlm_r(num))
      allocate(rlm%a_r_1d_rlm_r(num))
      num = rlm%nidx_rlm(2)
      allocate(rlm%idx_gl_1d_rlm_j(num,3))
!
      if(rlm%nidx_rlm(2) .gt. 0) rlm%idx_gl_1d_rlm_j = 0
      if(rlm%nidx_rlm(1) .gt. 0) then
        rlm%idx_gl_1d_rlm_r = 0
        rlm%radius_1d_rlm_r = 0.0d0
        rlm%a_r_1d_rlm_r    = 0.0d0
      end if
!
      end subroutine alloc_type_sph_1d_index_rlm
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_sph_1d_index_rj(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
      integer(kind = kint) :: num
!
      num = rj%nidx_rj(1)
      allocate(rj%idx_gl_1d_rj_r(num))
      allocate(rj%radius_1d_rj_r(num))
      allocate(rj%a_r_1d_rj_r(num))
!
      allocate(rj%ar_1d_rj(num,3))
      allocate(rj%r_ele_rj(num))
      allocate(rj%ar_ele_rj(num,3))
!
      num = rj%nidx_rj(2)
      allocate(rj%idx_gl_1d_rj_j(num,3))
!
      rj%idx_rj_degree_zero = 0
      rj%idx_rj_degree_one(-1:1) = 0
!
      if(rj%nidx_rj(2) .gt. 0) rj%idx_gl_1d_rj_j = 0
      if(rj%nidx_rj(1) .gt. 0) then
        rj%idx_gl_1d_rj_r = 0
        rj%radius_1d_rj_r = 0.0d0
        rj%a_r_1d_rj_r = 0.0d0
!
        rj%ar_1d_rj = 0.0d0
        rj%r_ele_rj = 0.0d0
        rj%ar_ele_rj = 0.0d0
      end if
!
      end subroutine alloc_type_sph_1d_index_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_radius_1d_gl(sph)
!
      type(sph_grids), intent(inout) :: sph
!
      deallocate(sph%radius_1d_gl)
!
      end subroutine dealloc_type_radius_1d_gl
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_spheric_param_rtp(rtp)
!
      type(sph_rtp_grid), intent(inout) :: rtp
!
      deallocate(rtp%inod_global_rtp, rtp%idx_global_rtp)
!
      end subroutine dealloc_type_spheric_param_rtp
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_spheric_param_rtm(rtm)
!
      type(sph_rtm_grid), intent(inout) :: rtm
!
      deallocate(rtm%inod_global_rtm, rtm%idx_global_rtm)
!
      end subroutine dealloc_type_spheric_param_rtm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_spheric_param_rlm(rlm)
!
      type(sph_rlm_grid), intent(inout) :: rlm
!
      deallocate(rlm%inod_global_rlm, rlm%idx_global_rlm)
!
      end subroutine dealloc_type_spheric_param_rlm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_spheric_param_rj(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
!
      deallocate(rj%inod_global_rj, rj%idx_global_rj)
!
      end subroutine dealloc_type_spheric_param_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_1d_index_rtp(rtp)
!
      type(sph_rtp_grid), intent(inout) :: rtp
!
      deallocate(rtp%radius_1d_rtp_r)
      deallocate(rtp%a_r_1d_rtp_r)
      deallocate(rtp%idx_gl_1d_rtp_r)
      deallocate(rtp%idx_gl_1d_rtp_t)
      deallocate(rtp%idx_gl_1d_rtp_p)
!
      end subroutine dealloc_type_sph_1d_index_rtp
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_1d_index_rtm(rtm)
!
      type(sph_rtm_grid), intent(inout) :: rtm
!
!
      deallocate(rtm%radius_1d_rtm_r)
      deallocate(rtm%a_r_1d_rtm_r)
      deallocate(rtm%idx_gl_1d_rtm_r)
      deallocate(rtm%idx_gl_1d_rtm_t)
      deallocate(rtm%idx_gl_1d_rtm_m)
!
      end subroutine dealloc_type_sph_1d_index_rtm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_1d_index_rlm(rlm)
!
      type(sph_rlm_grid), intent(inout) :: rlm
!
!
      deallocate(rlm%radius_1d_rlm_r)
      deallocate(rlm%a_r_1d_rlm_r   )
      deallocate(rlm%idx_gl_1d_rlm_r)
      deallocate(rlm%idx_gl_1d_rlm_j)
!
      end subroutine dealloc_type_sph_1d_index_rlm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_1d_index_rj(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
!
!
      deallocate(rj%radius_1d_rj_r, rj%a_r_1d_rj_r)
      deallocate(rj%ar_1d_rj, rj%r_ele_rj, rj%ar_ele_rj)
      deallocate(rj%idx_gl_1d_rj_r, rj%idx_gl_1d_rj_j)
!
      end subroutine dealloc_type_sph_1d_index_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_type_spheric_para_gl_part(sph)
!
      type(sph_grids), intent(in) :: sph
!
      write(50,*) 'l_truncation ', sph%l_truncation
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
      subroutine check_type_spheric_param_rtp(my_rank, rtp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rtp_grid), intent(in) :: rtp
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rtp ', rtp%sph_rank_rtp(1:3)
      write(my_rank+50,*) 'nidx_rtp ', rtp%nidx_rtp(1:3)
      write(my_rank+50,*) 'nnod_rtp ', rtp%nnod_rtp
!
      write(my_rank+50,*)  'i, inod_global_rtp, idx_global_rtp(r,t,p)'
      do i = 1, rtp%nnod_rtp
        write(my_rank+50,*)                                             &
     &             i, rtp%inod_global_rtp(i), rtp%idx_global_rtp(i,1:3)
      end do
!
      end subroutine check_type_spheric_param_rtp
!
! -----------------------------------------------------------------------
!
      subroutine check_type_spheric_param_rtm(my_rank, rtm)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rtm_grid), intent(in) :: rtm
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rtm ', rtm%sph_rank_rtm(1:3)
      write(my_rank+50,*) 'nidx_rtm ', rtm%nidx_rtm(1:3)
      write(my_rank+50,*) 'nnod_rtm ', rtm%nnod_rtm
!
      write(my_rank+50,*) 'i, inod_global_rtm, idx_global_rtm(r,t,p)'
      do i = 1, rtm%nnod_rtm
        write(my_rank+50,*)                                             &
     &             i, rtm%inod_global_rtm(i), rtm%idx_global_rtm(i,1:3)
      end do
!
      end subroutine check_type_spheric_param_rtm
!
! -----------------------------------------------------------------------
!
      subroutine check_type_spheric_param_rlm(my_rank, rlm)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rlm_grid), intent(in) :: rlm
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rlm ', rlm%sph_rank_rlm(1:2)
      write(my_rank+50,*) 'nidx_rlm ', rlm%nidx_rlm(1:2)
      write(my_rank+50,*) 'nnod_rlm ', rlm%nnod_rlm
!
      write(my_rank+50,*) 'i, inod_global_rlm, idx_global_rlm(r,j)'
      do i = 1, rlm%nnod_rlm
        write(my_rank+50,*)                                             &
     &             i, rlm%inod_global_rlm(i), rlm%idx_global_rlm(i,1:2)
      end do
!
      end subroutine check_type_spheric_param_rlm
!
! -----------------------------------------------------------------------
!
      subroutine check_type_spheric_param_rj(my_rank, rj)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rj ',  rj%sph_rank_rj(1:2)
      write(my_rank+50,*) 'nidx_rj  ',  rj%nidx_rj(1:2)
      write(my_rank+50,*) 'nnod_rj ',  rj%nnod_rj
!
      write(my_rank+50,*) 'i, inod_global_rj, idx_global_rj(r,j)'
      do i = 1, rj%nnod_rj
        write(my_rank+50,*)                                             &
     &             i, rj%inod_global_rj(i), rj%idx_global_rj(i,1:2)
      end do
!
      end subroutine check_type_spheric_param_rj
!
! -----------------------------------------------------------------------
!
      end module t_spheric_parameter
