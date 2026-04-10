!>@file   sph_boundary_data_picker.f90
!!@brief  module sph_boundary_data_picker
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Module to pick boudary condition data
!!
!!@verbatim
!!      integer(kind = kint) function sph_inner_boundary_r_grid(sph_bc)
!!      integer(kind = kint) function sph_outer_boundary_r_grid(sph_bc)
!!      real(kind = kreal) function sph_inner_boundary_radius(sph_bc)
!!      real(kind = kreal) function sph_outer_boundary_radius(sph_bc)
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!
!!      real(kind = kreal) function sph_inner_boundary_scalar_coef(bcs, &
!!     &                                                          j_idx)
!!      real(kind = kreal) function sph_outer_boundary_scalar_coef(bcs, &
!!     &                                                          j_idx)
!!        type(sph_scalar_boundary_data), intent(in) :: bcs
!!        integer(kind = kint), intent(in) :: j_idx
!!@endverbatim
!
      module sph_boundary_data_picker
!
      use m_precision
      use m_constants
!
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function sph_inner_boundary_r_grid(sph_bc)
      type(sph_boundary_type), intent(in) :: sph_bc
!
      sph_inner_boundary_r_grid = sph_bc%kr_in
!
      end function sph_inner_boundary_r_grid
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function sph_outer_boundary_r_grid(sph_bc)
      type(sph_boundary_type), intent(in) :: sph_bc
!
      sph_outer_boundary_r_grid = sph_bc%kr_out
!
      end function sph_outer_boundary_r_grid
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function sph_inner_boundary_radius(sph_bc)
      type(sph_boundary_type), intent(in) :: sph_bc
!
      sph_inner_boundary_radius = sph_bc%r_ICB(0)
!
      end function sph_inner_boundary_radius
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function sph_outer_boundary_radius(sph_bc)
      type(sph_boundary_type), intent(in) :: sph_bc
!
      sph_outer_boundary_radius = sph_bc%r_CMB(0)
!
      end function sph_outer_boundary_radius
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      real(kind = kreal) function sph_inner_boundary_scalar_coef(bcs,   &
     &                                                          j_idx)
      type(sph_scalar_boundary_data), intent(in) :: bcs
      integer(kind = kint), intent(in) :: j_idx
!
      sph_inner_boundary_scalar_coef = bcs%ICB_Sspec%S_BC(j_idx)
!
      end function sph_inner_boundary_scalar_coef
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function sph_outer_boundary_scalar_coef(bcs,   &
     &                                                          j_idx)
      type(sph_scalar_boundary_data), intent(in) :: bcs
      integer(kind = kint), intent(in) :: j_idx
!
      sph_outer_boundary_scalar_coef = bcs%CMB_Sspec%S_BC(j_idx)
!
      end function sph_outer_boundary_scalar_coef
!
!-----------------------------------------------------------------------
!
      end module sph_boundary_data_picker
