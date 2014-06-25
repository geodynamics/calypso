!>@file   copy_temporal_4_sph_trans.f90
!!@brief  module copy_temporal_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer
!!        for temporally step
!!
!!@verbatim
!!  routines for backward transform
!!      subroutine copy_tmp_vec_spec_to_trans
!!      subroutine copy_tmp_scl_spec_to_trans
!!
!!      subroutine copy_tmp_vec_fld_from_trans
!!      subroutine copy_tmp_scl_fld_from_trans
!!
!!  routines for forward transform
!!      subroutine copy_tmp_scl_fld_to_trans
!!      subroutine copy_tmp_scl_spec_from_trans
!!
!!      subroutine copy_tmp_vec_fld_to_trans
!!      subroutine copy_tmp_vec_spec_from_trans
!!@endverbatim
!
      module copy_temporal_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use m_sph_phys_address
      use m_addresses_trans_sph_tmp
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_vec_spec_to_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
      call copy_vec_spec_to_trans(ncomp_tmp_rj_2_rtp,                   &
     &      ipol%i_grad_vx, btmp_trns%i_grad_vx)
      call copy_vec_spec_to_trans(ncomp_tmp_rj_2_rtp,                   &
     &      ipol%i_grad_vy, btmp_trns%i_grad_vy)
      call copy_vec_spec_to_trans(ncomp_tmp_rj_2_rtp,                   &
     &      ipol%i_grad_vz, btmp_trns%i_grad_vz)
!$omp end parallel
!
      end subroutine copy_tmp_vec_spec_to_trans
!
!-----------------------------------------------------------------------
!
!      subroutine copy_tmp_scl_spec_to_trans
!
!      use copy_spectr_4_sph_trans
!
!
!!$omp parallel
!      call copy_scalar_spec_to_trans(ncomp_tmp_rj_2_rtp,               &
!     &      ipol%i_temp, btmp_trns%i_temp)
!!$omp end parallel
!
!      end subroutine copy_tmp_scl_spec_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_vec_fld_from_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_vec_fld_from_trans(ncomp_tmp_rj_2_rtp,                  &
     &    irtp%i_grad_vx, btmp_trns%i_grad_vx)
      call copy_vec_fld_from_trans(ncomp_tmp_rj_2_rtp,                  &
     &    irtp%i_grad_vy, btmp_trns%i_grad_vy)
      call copy_vec_fld_from_trans(ncomp_tmp_rj_2_rtp,                  &
     &    irtp%i_grad_vz, btmp_trns%i_grad_vz)
!$omp end parallel
!
      end subroutine copy_tmp_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!
!      subroutine copy_tmp_scl_fld_from_trans
!
!      use copy_sph_field_4_sph_trans
!
!
!!$omp parallel
!        call copy_scalar_fld_from_trans(ncomp_tmp_rj_2_rtp,            &
!     &      irtp%i_temp, btmp_trns%i_temp)
!!$omp end parallel
!
!      end subroutine copy_tmp_scl_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!      subroutine copy_tmp_vec_fld_to_trans
!
!      use copy_sph_field_4_sph_trans
!
!
!!$omp parallel
!      call copy_vec_fld_to_trans(ncomp_tmp_rtp_2_rj,                   &
!     &      irtp%i_coriolis, ftmp_trns%i_coriolis)
!!$omp end parallel
!
!      end  subroutine copy_tmp_vec_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_scl_fld_to_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_fld_to_trans(ncomp_tmp_rtp_2_rj,                 &
     &      irtp%i_grad_vx, ftmp_trns%i_grad_vx)
      call copy_scalar_fld_to_trans(ncomp_tmp_rtp_2_rj,                 &
     &      irtp%i_grad_vy, ftmp_trns%i_grad_vy)
      call copy_scalar_fld_to_trans(ncomp_tmp_rtp_2_rj,                 &
     &      irtp%i_grad_vz, ftmp_trns%i_grad_vz)
!$omp end parallel
!
      end  subroutine copy_tmp_scl_fld_to_trans
!
!-----------------------------------------------------------------------
!
!      subroutine copy_tmp_vec_spec_from_trans
!
!      use copy_spectr_4_sph_trans
!
!
!!$omp parallel
!      call copy_vec_spec_from_trans(ncomp_tmp_rtp_2_rj,                &
!     &    ipol%i_coriolis, ftmp_trns%i_coriolis)
!!$omp end parallel
!
!      end  subroutine copy_tmp_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_scl_spec_from_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_spec_from_trans(ncomp_tmp_rtp_2_rj,              &
     &      ipol%i_grad_vx, ftmp_trns%i_grad_vx)
      call copy_scalar_spec_from_trans(ncomp_tmp_rtp_2_rj,              &
     &      ipol%i_grad_vy, ftmp_trns%i_grad_vy)
      call copy_scalar_spec_from_trans(ncomp_tmp_rtp_2_rj,              &
     &      ipol%i_grad_vz, ftmp_trns%i_grad_vz)
!$omp end parallel
!
      end  subroutine copy_tmp_scl_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_temporal_4_sph_trans
