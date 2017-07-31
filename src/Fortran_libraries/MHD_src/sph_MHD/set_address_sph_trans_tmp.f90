!>@file   set_address_sph_trans_tmp.f90
!!@brief  module set_address_sph_trans_tmp
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_temporal_trans(ipol, iphys, trns_tmp,  &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_tmp
!!      subroutine check_address_trans_sph_tmp                          &
!!     &          (ipol, idpdr, itor, iphys, trns_tmp)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(in) :: trns_tmp
!!@endverbatim
!
      module set_address_sph_trans_tmp
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: b_trans_address_vector_tmp
      private :: b_trans_address_scalar_tmp
      private :: f_trans_address_vector_tmp
      private :: f_trans_address_scalar_tmp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_temporal_trans(ipol, iphys, trns_tmp,    &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_addresses_sph_transform
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
      trns_tmp%nvector_rj_2_rtp = 0
!      call b_trans_address_vector_tmp                                  &
!     &   (ipol, iphys, trns_tmp%nvector_rj_2_rtp, trns_tmp%b_trns)
      trns_tmp%nscalar_rj_2_rtp = 0
!      call b_trans_address_scalar_tmp(ipol, iphys,                     &
!     &    trns_tmp%nvector_rj_2_rtp, trns_tmp%nscalar_rj_2_rtp,        &
!     &    trns_tmp%b_trns)
      trns_tmp%ntensor_rj_2_rtp = 0
!
      trns_tmp%nvector_rtp_2_rj = 0
!      call f_trans_address_vector_tmp                                  &
!     &   (ipol, iphys, trns_tmp%nvector_rtp_2_rj, trns_tmp%f_trns)
      call f_trans_address_scalar_tmp(ipol, iphys,                      &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns)
      trns_tmp%ntensor_rtp_2_rj = 0
!
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns_tmp%nscalar_rj_2_rtp + 6*trns_tmp%ntensor_rj_2_rtp
      trns_tmp%ncomp_rj_2_rtp                                           &
     &      = 3*trns_tmp%nvector_rj_2_rtp + nscltsr_rj_2_rtp
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns_tmp%nscalar_rtp_2_rj + 6*trns_tmp%ntensor_rtp_2_rj
      trns_tmp%ncomp_rtp_2_rj                                           &
     &      = 3*trns_tmp%nvector_rtp_2_rj + nscltsr_rtp_2_rj
!
      ncomp_sph_trans = max(ncomp_sph_trans, trns_tmp%ncomp_rtp_2_rj)
      ncomp_sph_trans = max(ncomp_sph_trans, trns_tmp%ncomp_rj_2_rtp)
!
      nvector_sph_trans                                                 &
     &      = max(nvector_sph_trans, trns_tmp%nvector_rj_2_rtp)
      nvector_sph_trans                                                 &
     &      = max(nvector_sph_trans, trns_tmp%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rj_2_rtp)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rtp_2_rj)
!
      end subroutine set_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      subroutine check_address_trans_sph_tmp                            &
     &          (ipol, idpdr, itor, iphys, trns_tmp)
!
      use t_addresses_sph_transform
      use check_address_sph_trans
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(in) :: trns_tmp
!
!
      write(*,*) 'addresses of spherical transform for gradients'
!
      call check_add_trans_sph_MHD                                      &
     &   (ipol, idpdr, itor, iphys, trns_tmp%b_trns, trns_tmp%f_trns,   &
     &    trns_tmp%ncomp_rj_2_rtp, trns_tmp%nvector_rj_2_rtp,           &
     &    trns_tmp%nscalar_rj_2_rtp, trns_tmp%ncomp_rtp_2_rj,           &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj)
!
      end subroutine check_address_trans_sph_tmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_vector_tmp                             &
     &         (ipol, iphys, nvector_tmp_rj_2_rtp, bt_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(inout) :: nvector_tmp_rj_2_rtp
      type(phys_address), intent(inout) :: bt_trns
!
!
      nvector_tmp_rj_2_rtp = 0
      call add_vec_trans_flag_snap(ipol%i_grad_vx, iphys%i_grad_vx,     &
     &    nvector_tmp_rj_2_rtp, bt_trns%i_grad_vx)
!
      end subroutine b_trans_address_vector_tmp
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_tmp(ipol, iphys,                &
     &          nvector_tmp_rj_2_rtp, nscalar_tmp_rj_2_rtp, bt_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(in) :: nvector_tmp_rj_2_rtp
      integer(kind = kint), intent(inout) :: nscalar_tmp_rj_2_rtp
      type(phys_address), intent(inout) :: bt_trns
!
!
      nscalar_tmp_rj_2_rtp = 0
      call add_scl_trans_flag_snap(ipol%i_temp, iphys%i_temp,           &
     &    nvector_tmp_rj_2_rtp, nscalar_tmp_rj_2_rtp, bt_trns%i_temp)
!
      end subroutine b_trans_address_scalar_tmp
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_vector_tmp                             &
     &         (ipol, iphys, nvector_tmp_rtp_2_rj, ft_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(phys_address), intent(inout) :: ft_trns
      integer(kind = kint), intent(inout) :: nvector_tmp_rtp_2_rj
!
!
      nvector_tmp_rtp_2_rj = 0
      call add_vec_trans_flag_snap(ipol%i_coriolis, iphys%i_coriolis,   &
     &    nvector_tmp_rtp_2_rj, ft_trns%i_coriolis)
!
      end subroutine f_trans_address_vector_tmp
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_tmp(ipol, iphys,                &
     &          nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj, ft_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(in) :: nvector_tmp_rtp_2_rj
      integer(kind = kint), intent(inout) :: nscalar_tmp_rtp_2_rj
      type(phys_address), intent(inout) :: ft_trns
!
!
      nscalar_tmp_rtp_2_rj = 0
      call add_scl_trans_flag_snap(ipol%i_grad_vx, iphys%i_grad_vx,     &
     &    nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                   &
     &    ft_trns%i_grad_vx)
      call add_scl_trans_flag_snap(ipol%i_grad_vy, iphys%i_grad_vy,     &
     &    nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                   &
     &    ft_trns%i_grad_vy)
      call add_scl_trans_flag_snap(ipol%i_grad_vz, iphys%i_grad_vz,     &
     &    nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                   &
     &    ft_trns%i_grad_vz)
!
      end subroutine f_trans_address_scalar_tmp
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_tmp
