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
      type(phys_address), save :: btmp_trns
!
!>    addresses for forces to forward transform
      type(phys_address), save :: ftmp_trns
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_temporal_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
!
!
      nvector_tmp_rtp_2_rj = 0
!      call add_vec_trans_flag(ipol%i_coriolis, irtp%i_coriolis,        &
!     &    nvector_tmp_rtp_2_rj, fsnap_trns%i_coriolis)
      ncomp_tmp_rtp_2_rj = 3*nvector_tmp_rtp_2_rj
!
      nscalar_tmp_rtp_2_rj = 0
      call add_scalar_trans_flag(ipol%i_grad_vx, irtp%i_grad_vx,        &
     &    ncomp_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                     &
     &    ftmp_trns%i_grad_vx)
      call add_scalar_trans_flag(ipol%i_grad_vy, irtp%i_grad_vy,        &
     &    ncomp_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                     &
     &    ftmp_trns%i_grad_vy)
      call add_scalar_trans_flag(ipol%i_grad_vz, irtp%i_grad_vz,        &
     &    ncomp_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                     &
     &    ftmp_trns%i_grad_vz)
      ncomp_tmp_rtp_2_rj = ncomp_tmp_rtp_2_rj + nscalar_tmp_rtp_2_rj
!
!
      nvector_tmp_rj_2_rtp = 0
      call add_vec_trans_flag(ipol%i_grad_vx, irtp%i_grad_vx,           &
     &    nvector_tmp_rj_2_rtp, btmp_trns%i_grad_vx)
      call add_vec_trans_flag(ipol%i_grad_vy, irtp%i_grad_vy,           &
     &    nvector_tmp_rj_2_rtp, btmp_trns%i_grad_vy)
      call add_vec_trans_flag(ipol%i_grad_vz, irtp%i_grad_vz,           &
     &    nvector_tmp_rj_2_rtp, btmp_trns%i_grad_vz)
      ncomp_tmp_rj_2_rtp = 3*nvector_tmp_rj_2_rtp
!
      nscalar_tmp_rj_2_rtp = 0
!      call add_transform_flag(ipol%i_temp, irtp%i_temp,                &
!     &    ncomp_tmp_rj_2_rtp, nscalar_tmp_rj_2_rtp, bsnap_trns%i_temp)
      ncomp_tmp_rj_2_rtp = ncomp_tmp_rj_2_rtp + nscalar_tmp_rj_2_rtp
!
!
      ncomp_sph_trans = max(ncomp_sph_trans, ncomp_tmp_rtp_2_rj)
      ncomp_sph_trans = max(ncomp_sph_trans, ncomp_tmp_rj_2_rtp)
!
      end subroutine set_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      subroutine check_addresses_temporal_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      write(*,*) 'ncomp_tmp_rj_2_rtp', ncomp_tmp_rj_2_rtp
      write(*,*) 'ncomp_tmp_rtp_2_rj', ncomp_tmp_rtp_2_rj
!
      write(*,*) 'nvector_tmp_rj_2_rtp', nvector_tmp_rj_2_rtp
      if(btmp_trns%i_grad_vx .gt. 0) write(*,*)                         &
     &            'btmp_trns%i_grad_vx', btmp_trns%i_grad_vx,           &
     &            ipol%i_grad_vx, irtp%i_grad_vx
      if(btmp_trns%i_grad_vy .gt. 0) write(*,*)                         &
     &            'btmp_trns%i_grad_vy', btmp_trns%i_grad_vy,           &
     &            ipol%i_grad_vy, irtp%i_grad_vy
      if(btmp_trns%i_grad_vz .gt. 0) write(*,*)                         &
     &            'btmp_trns%i_grad_vz', btmp_trns%i_grad_vz,           &
     &            ipol%i_grad_vz, irtp%i_grad_vz
      write(*,*)
!
      write(*,*) 'nscalar_tmp_rj_2_rtp', nscalar_tmp_rj_2_rtp
!      if(btmp_trns%i_temp .gt. 0) write(*,*)                           &
!     &            'btmp_trns%i_temp', btmp_trns%i_temp,                &
!     &            ipol%i_temp, irtp%i_temp
      write(*,*)
!
!
      write(*,*) 'nvector_tmp_rtp_2_rj', nvector_tmp_rtp_2_rj
!      if(ftmp_trns%i_coriolis .gt. 0) write(*,*)                       &
!     &            'ftmp_trns%i_coriolis',  ftmp_trns%i_coriolis,       &
!     &            ipol%i_coriolis, irtp%i_coriolis
!
!
      write(*,*) 'nscalar_tmp_rtp_2_rj', nscalar_tmp_rtp_2_rj
      if(ftmp_trns%i_grad_vx .gt. 0) write(*,*)                         &
     &            'ftmp_trns%i_grad_vx', ftmp_trns%i_grad_vx,           &
     &            ipol%i_grad_vx, irtp%i_velo
      if(ftmp_trns%i_grad_vy .gt. 0) write(*,*)                         &
     &            'ftmp_trns%i_grad_vy', ftmp_trns%i_grad_vy,           &
     &            ipol%i_grad_vy, irtp%i_velo+1
      if(ftmp_trns%i_grad_vz .gt. 0) write(*,*)                         &
     &            'ftmp_trns%i_grad_vz', ftmp_trns%i_grad_vz,           &
     &            ipol%i_grad_vz, irtp%i_velo+2
        write(*,*)
!
      end subroutine check_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_sph_tmp
