!>@file   t_sph_trans_arrays_MHD.f90
!!@brief  module t_sph_trans_arrays_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!!@verbatim
!!      subroutine alloc_sph_trans_address(sph_rtp, WK)
!!      subroutine dealloc_sph_trans_address(WK)
!!      subroutine alloc_sph_trans_area_snap(sph_rtp, WK)
!!      subroutine dealloc_sph_trans_area_snap(WK)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!@endverbatim
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
      module t_sph_trans_arrays_MHD
!
      use m_precision
!
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
!
!
      implicit none
!
!>      strucutre of spherical transform data addresses
      type address_4_sph_trans
!>        strucutre of backward spherical transform data addresses
        type(spherical_transform_data) :: backward
!>        strucutre of forward spherical transform data addresses
        type(spherical_transform_data) :: forward
!
!>        addresses of fields for backward transform
        type(phys_address) :: b_trns
!>        addresses of forces for forward transform
        type(phys_address) :: f_trns
      end type address_4_sph_trans
!
!>      strucutres for spherical transform for MHD dynamo
      type works_4_sph_trans_MHD
!>        strucutres for spherical transform for MHD
        type(address_4_sph_trans) :: trns_MHD
!
!>        strucutres for spherical transform for snapshot output
        type(address_4_sph_trans) :: trns_snap
!>        strucutres for spherical transform for energy flux output
        type(address_4_sph_trans) :: trns_eflux
!>        strucutres for spherical transform for intermediate snapshot
        type(address_4_sph_trans) :: trns_difv
!
!>        Work structures for various spherical harmonics trasform
        type(spherical_trns_works) :: WK_sph
!
!>        Gunat integrals of Coriolis term
        type(gaunt_coriolis_rlm) :: gt_cor
!>        Structure of Coriolis terms in@f$ f(r,l,m) @f$.
        type(coriolis_rlm_data) :: cor_rlm
      end type works_4_sph_trans_MHD
!
      private :: alloc_nonlinear_data, dealloc_nonlinear_data
      private :: alloc_nonlinear_pole, dealloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trans_address(sph_rtp, WK)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      call alloc_nonlinear_data(sph_rtp, wk%trns_MHD)
      call alloc_nonlinear_pole(sph_rtp, WK%trns_MHD)
!
      end subroutine alloc_sph_trans_address
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trans_address(WK)
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      call dealloc_nonlinear_pole(WK%trns_MHD)
      call dealloc_nonlinear_data(WK%trns_MHD)
!
      end subroutine dealloc_sph_trans_address
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trans_area_snap(sph_rtp, WK)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      call alloc_nonlinear_data(sph_rtp, WK%trns_snap)
      call alloc_nonlinear_pole(sph_rtp, WK%trns_snap)
!
      call alloc_nonlinear_data(sph_rtp, wk%trns_difv)
      call alloc_nonlinear_pole(sph_rtp, wk%trns_difv)
!
      call alloc_nonlinear_data(sph_rtp, WK%trns_eflux)
      call alloc_nonlinear_pole(sph_rtp, WK%trns_eflux)
!
      end subroutine alloc_sph_trans_area_snap
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trans_area_snap(WK)
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      call dealloc_nonlinear_pole(WK%trns_difv)
      call dealloc_nonlinear_data(WK%trns_difv)
!
      call dealloc_nonlinear_pole(WK%trns_snap)
      call dealloc_nonlinear_data(WK%trns_snap)
!
      call dealloc_nonlinear_pole(WK%trns_eflux)
      call dealloc_nonlinear_data(WK%trns_eflux)
!
      end subroutine dealloc_sph_trans_area_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_data(sph_rtp, trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call alloc_sph_trns_field_data(sph_rtp, trns%backward)
      call alloc_sph_trns_field_data(sph_rtp, trns%forward)
!
      end subroutine alloc_nonlinear_data
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_pole(sph_rtp, trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call alloc_sph_trns_pole_data(sph_rtp, trns%backward)
      call alloc_sph_trns_pole_data(sph_rtp, trns%forward)
!
      end subroutine alloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_data(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call dealloc_sph_trns_field_dats(trns%backward)
      call dealloc_sph_trns_field_dats(trns%forward)
!
      end subroutine dealloc_nonlinear_data
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_pole(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call dealloc_sph_trns_pole_data(trns%backward)
      call dealloc_sph_trns_pole_data(trns%forward)
!
      end subroutine dealloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      end module t_sph_trans_arrays_MHD
