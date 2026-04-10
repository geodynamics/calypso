!>@file   spherical_indices_picker.f90
!!@brief  module spherical_indices_picker
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!-----------------------------------------------------------------------
!!   Wrapper functions to get spherical shell indices from structure.
!!     Informations are stored in structure sph_grids 'sph'
!!    Do not edit.
!!-----------------------------------------------------------------------
!!      integer function find_local_sph_mode_address(sph, l, m)
!!        type(sph_grids), intent(in) :: sph
!!        integer, intent(in) :: l, m
!!      integer(kind = kint) function local_sph_data_address(sph,       &
!!     &                                                     kr, j_lc)
!!        type(sph_grids), intent(in) :: sph
!!        integer, intent(in) :: kr, j_lc
!!      real(kind = kreal) function radius_1d_rj_r(sph, kr)
!!        type(sph_grids), intent(in) :: sph
!!        integer, intent(in) :: kr
!!
!!      real(kind = kreal) function r_ICB(sph)
!!      real(kind = kreal) function r_CMB(sph)
!!      integer(kind= kint) function nlayer_ICB(sph)
!!      integer(kind= kint) function nlayer_CMB(sph)
!!      integer(kind = kint) function inod_rj_center(sph)
!!      integer(kind = kint) function idx_rj_degree_zero(sph)
!!      integer(kind = kint) function nnod_rj(sph)
!!      integer(kind = kint) function num_rj_radial_point(sph)
!!      integer(kind = kint) function num_rj_horiz_point(sph)
!!        type(sph_grids), intent(in) :: sph
!!      integer(kind = kint) function nidx_rj(sph, nd)
!!        type(sph_grids), intent(in) :: sph
!!        integer, intent(in) :: nd
!!
!!-----------------------------------------------------------------------
!!
!!      j_lc = find_local_sph_mode_address(sph, l, m) ::
!!         Return local spherical harmonics mode address j_lc for Y(l,m)
!!         If requested mode does not exist in the process, 0 is set
!!
!!     inod = local_sph_data_address(sph, k, j_lc) :: 
!!         Return address of sphectrum data
!!     radius_1d_rj_r(sph, k) :: Radius at global grid address k
!!     nlayer_ICB(sph) :: radial ID for ICB
!!     nlayer_CMB(sph) :: radial ID for CMB
!!     r_ICB(sph) :: ICB radius in grid data
!!     r_CMB(sph) :: CMB radius in grid data
!!     inod_rj_center(sph) :: Local data ID for center data
!!         If spectrum data does not have center
!!              inod_rj_center(sph) = 0
!!     nidx_rj(sph,1) :: Number of radial grids
!!     nidx_rj(sph,2) :: Number of modes in each process
!!     nnod_rj(sph) :: Number of local data points
!!-----------------------------------------------------------------------
!!@endverbatim
!
!
      module spherical_indices_picker
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      integer function find_local_sph_mode_address(sph, l, m)
!
      type(sph_grids), intent(in) :: sph
      integer, intent(in) :: l, m
!
!
      find_local_sph_mode_address                                       &
     &      = find_local_sph_address(sph%sph_rj, l, m)
!
      end function find_local_sph_mode_address
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function local_sph_data_address(sph,         &
     &                                                     kr, j_lc)
!
      type(sph_grids), intent(in) :: sph
      integer, intent(in) :: kr, j_lc
!
!
      local_sph_data_address                                            &
     &      = local_sph_node_address(sph%sph_rj, kr, j_lc)
!
      end function local_sph_data_address
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function radius_1d_rj_r(sph, kr)
!
      type(sph_grids), intent(in) :: sph
      integer, intent(in) :: kr
!
      radius_1d_rj_r = sph%sph_rj%radius_1d_rj_r(kr)
!
      end function radius_1d_rj_r
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function r_ICB(sph)
!
      type(sph_grids), intent(in) :: sph
!
      r_ICB = sph%sph_params%radius_ICB
!
      end function r_ICB
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function r_CMB(sph)
!
      type(sph_grids), intent(in) :: sph
!
      r_CMB = sph%sph_params%radius_CMB
!
      end function r_CMB
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nlayer_ICB(sph)
!
      type(sph_grids), intent(in) :: sph
!
      nlayer_ICB = sph%sph_params%nlayer_ICB
!
      end function nlayer_ICB
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nlayer_CMB(sph)
!
      type(sph_grids), intent(in) :: sph
!
      nlayer_CMB = sph%sph_params%nlayer_CMB
!
      end function nlayer_CMB
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      integer(kind = kint) function inod_rj_center(sph)
!
      type(sph_grids), intent(in) :: sph
!
      inod_rj_center = sph%sph_rj%inod_rj_center
!
      end function inod_rj_center
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function idx_rj_degree_zero(sph)
!
      type(sph_grids), intent(in) :: sph
!
      idx_rj_degree_zero = sph%sph_rj%idx_rj_degree_zero
!
      end function idx_rj_degree_zero
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nidx_rj(sph, nd)
!
      type(sph_grids), intent(in) :: sph
      integer, intent(in) :: nd
!
      nidx_rj = sph%sph_rj%nidx_rj(nd)
!
      end function nidx_rj
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nnod_rj(sph)
!
      type(sph_grids), intent(in) :: sph
!
      nnod_rj = sph%sph_rj%nnod_rj
!
      end function nnod_rj
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function num_rj_radial_point(sph)
!
      type(sph_grids), intent(in) :: sph
!
      num_rj_radial_point = sph%sph_rj%nidx_rj(1)
!
      end function num_rj_radial_point
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function num_rj_horiz_point(sph)
!
      type(sph_grids), intent(in) :: sph
!
      num_rj_horiz_point = sph%sph_rj%nidx_rj(2)
!
      end function num_rj_horiz_point
!
!-----------------------------------------------------------------------
!
      end module spherical_indices_picker
