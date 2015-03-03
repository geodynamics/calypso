!>@file   m_addresses_trans_sph_tmp.f90
!!@brief  module m_addresses_trans_sph_tmp
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine allocate_tmp_trans_rtp
!!      subroutine deallocate_tmp_trans_rtp
!!
!!      subroutine set_addresses_temporal_trans
!!      subroutine check_addresses_temporal_trans
!!@endverbatim
!
      module m_addresses_trans_sph_tmp
!
      use m_precision
!
      use t_phys_address
!
      implicit none
!
!>      number of components for backward spherical harmonics transform
      integer(kind = kint) :: ncomp_tmp_rj_2_rtp = 0
!>      number of components
!!      for backward vector spherical harmonics transform
      integer(kind = kint) :: nvector_tmp_rj_2_rtp = 0
!>      number of scalars for backward spherical harmonics transform
      integer(kind = kint) :: nscalar_tmp_rj_2_rtp = 0
!>      number of tensors for backward spherical harmonics transform
      integer(kind = kint) :: ntensor_tmp_rj_2_rtp = 0
!
!>      number of components for forward spherical harmonics transform
      integer(kind = kint) :: ncomp_tmp_rtp_2_rj = 0
!>      number of components
!!      for forward vector spherical harmonics transform
      integer(kind = kint) :: nvector_tmp_rtp_2_rj = 0
!>      number of scalars for forward spherical harmonics transform
      integer(kind = kint) :: nscalar_tmp_rtp_2_rj = 0
!>      number of tensors for forward spherical harmonics transform
      integer(kind = kint) :: ntensor_tmp_rtp_2_rj = 0
!
!>    addresses for fields to backward transform
      type(phys_address), save :: bt_trns
!
!>    addresses for forces to forward transform
      type(phys_address), save :: ft_trns
!
!>      field data to evaluate nonliear terms in grid space
      real(kind = kreal), allocatable :: flt_rtp(:,:)
!>      Nonoliear terms data in grid space
      real(kind = kreal), allocatable :: frt_rtp(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_tmp_trans_rtp
!
      use m_spheric_parameter
!
!
      allocate(flt_rtp(nnod_rtp,ncomp_tmp_rj_2_rtp))
      allocate(frt_rtp(nnod_rtp,ncomp_tmp_rtp_2_rj))
      if(ncomp_tmp_rj_2_rtp .gt. 0) flt_rtp = 0.0d0
      if(ncomp_tmp_rtp_2_rj .gt. 0) frt_rtp = 0.0d0
!
      end subroutine allocate_tmp_trans_rtp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_tmp_trans_rtp
!
      deallocate(flt_rtp, frt_rtp)
!
      end subroutine deallocate_tmp_trans_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_addresses_temporal_trans
!
      use m_work_4_sph_trans
      use m_node_phys_address
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
!
!
      nvector_tmp_rtp_2_rj = 0
!      call add_vec_trans_flag(ipol%i_coriolis, iphys%i_coriolis,       &
!     &    nvector_tmp_rtp_2_rj, fs_trns%i_coriolis)
      ncomp_tmp_rtp_2_rj = 3*nvector_tmp_rtp_2_rj
!
      nscalar_tmp_rtp_2_rj = 0
      call add_scalar_trans_flag(ipol%i_grad_vx, iphys%i_grad_vx,       &
     &    ncomp_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                     &
     &    ft_trns%i_grad_vx)
      call add_scalar_trans_flag(ipol%i_grad_vy, iphys%i_grad_vy,       &
     &    ncomp_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                     &
     &    ft_trns%i_grad_vy)
      call add_scalar_trans_flag(ipol%i_grad_vz, iphys%i_grad_vz,       &
     &    ncomp_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                     &
     &    ft_trns%i_grad_vz)
      ncomp_tmp_rtp_2_rj = ncomp_tmp_rtp_2_rj + nscalar_tmp_rtp_2_rj
!
!
      nvector_tmp_rj_2_rtp = 0
!      call add_vec_trans_flag(ipol%i_grad_vx, iphys%i_grad_vx,         &
!     &    nvector_tmp_rj_2_rtp, bt_trns%i_grad_vx)
      ncomp_tmp_rj_2_rtp = 3*nvector_tmp_rj_2_rtp
!
      nscalar_tmp_rj_2_rtp = 0
!      call add_transform_flag(ipol%i_temp, iphys%i_temp,               &
!     &    ncomp_tmp_rj_2_rtp, nscalar_tmp_rj_2_rtp, bs_trns%i_temp)
      ncomp_tmp_rj_2_rtp = ncomp_tmp_rj_2_rtp + nscalar_tmp_rj_2_rtp
!
!
      ncomp_sph_trans = max(ncomp_sph_trans, ncomp_tmp_rtp_2_rj)
      ncomp_sph_trans = max(ncomp_sph_trans, ncomp_tmp_rj_2_rtp)
!
      nvector_sph_trans = max(nvector_sph_trans, nvector_tmp_rj_2_rtp)
      nvector_sph_trans = max(nvector_sph_trans, nvector_tmp_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans,                        &
     &                 (nscalar_tmp_rj_2_rtp+6*ntensor_tmp_rj_2_rtp))
      nscalar_sph_trans = max(nscalar_sph_trans,                        &
     &                 (nscalar_tmp_rtp_2_rj+6*ntensor_tmp_rtp_2_rj))
!
      end subroutine set_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      subroutine check_addresses_temporal_trans
!
      use m_work_4_sph_trans
      use m_node_phys_address
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      write(*,*) 'ncomp_tmp_rj_2_rtp', ncomp_tmp_rj_2_rtp
      write(*,*) 'ncomp_tmp_rtp_2_rj', ncomp_tmp_rtp_2_rj
!
      write(*,*) 'nvector_tmp_rj_2_rtp', nvector_tmp_rj_2_rtp
!      if(bt_trns%i_grad_vx .gt. 0) write(*,*)                          &
!     &            'bt_trns%i_grad_vx', bt_trns%i_grad_vx,              &
!     &            ipol%i_grad_vx, iphys%i_grad_vx
      write(*,*)
!
      write(*,*) 'nscalar_tmp_rj_2_rtp', nscalar_tmp_rj_2_rtp
!      if(bt_trns%i_temp .gt. 0) write(*,*)                             &
!     &            'bt_trns%i_temp', bt_trns%i_temp,                    &
!     &            ipol%i_temp, iphys%i_temp
      write(*,*)
!
!
      write(*,*) 'nvector_tmp_rtp_2_rj', nvector_tmp_rtp_2_rj
!      if(ft_trns%i_coriolis .gt. 0) write(*,*)                         &
!     &            'ft_trns%i_coriolis',  ft_trns%i_coriolis,           &
!     &            ipol%i_coriolis, iphys%i_coriolis
!
!
      write(*,*) 'nscalar_tmp_rtp_2_rj', nscalar_tmp_rtp_2_rj
      if(ft_trns%i_grad_vx .gt. 0) write(*,*)                           &
     &            'ft_trns%i_grad_vx', ft_trns%i_grad_vx,               &
     &            ipol%i_grad_vx, iphys%i_velo
      if(ft_trns%i_grad_vy .gt. 0) write(*,*)                           &
     &            'ft_trns%i_grad_vy', ft_trns%i_grad_vy,               &
     &            ipol%i_grad_vy, iphys%i_velo+1
      if(ft_trns%i_grad_vz .gt. 0) write(*,*)                           &
     &            'ft_trns%i_grad_vz', ft_trns%i_grad_vz,               &
     &            ipol%i_grad_vz, iphys%i_velo+2
        write(*,*)
!
      end subroutine check_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_sph_tmp
