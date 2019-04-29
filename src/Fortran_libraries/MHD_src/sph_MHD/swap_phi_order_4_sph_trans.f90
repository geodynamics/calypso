!>@file   swap_phi_order_4_sph_trans.f90
!!@brief  module swap_phi_order_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2014
!
!>@brief  Swap array for phi componene
!!
!!@verbatim
!!      subroutine swap_phi_from_MHD_trans(sph_rtp, trns_MHD)
!!      subroutine swap_phi_to_MHD_trans(sph_rtp, trns_MHD)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!@endverbatim
!
      module swap_phi_order_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_rtp_data
      use t_addresses_sph_transform
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine swap_phi_from_MHD_trans(sph_rtp, trns_MHD)
!
      use m_FFT_selector
      use swap_phi_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      if(iflag_FFT .ne. iflag_FFTW) return
      call swap_phi_order_from_trans(trns_MHD%backward%ncomp,           &
     &    sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, trns_MHD%backward%fld_rtp)
      call swap_phi_order_from_trans(trns_MHD%forward%ncomp,            &
     &    sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, trns_MHD%forward%fld_rtp)
!
      end subroutine swap_phi_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_to_MHD_trans(sph_rtp, trns_MHD)
!
      use m_FFT_selector
      use swap_phi_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      if(iflag_FFT .ne. iflag_FFTW) return
      call swap_phi_order_to_trans(trns_MHD%backward%ncomp,             &
     &    sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, trns_MHD%backward%fld_rtp)
      call swap_phi_order_to_trans(trns_MHD%forward%ncomp,              &
     &    sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, trns_MHD%forward%fld_rtp)
!
      end subroutine swap_phi_to_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module swap_phi_order_4_sph_trans
