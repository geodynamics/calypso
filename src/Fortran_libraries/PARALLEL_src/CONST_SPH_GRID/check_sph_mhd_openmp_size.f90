!>@file   check_sph_mhd_openmp_size.f90
!!@brief  module check_sph_mhd_openmp_size
!!
!!@author H. Matsui
!!@date Programmed in July, 2007 
!
!> @brief Check openMP size by subdomain resolution
!!
!!@verbatim
!!      subroutine s_check_sph_mhd_openmp_size(sph)
!!        type(legendre_trns_works), intent(in) :: WK_leg
!!        type(sph_grids), intent(in) :: sph
!!@endverbatim
!
      module check_sph_mhd_openmp_size
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_spheric_parameter
      use t_legendre_trans_select
!
      implicit none
!
      private :: check_legendre_openmp_size
      private :: check_zonal_FFT_openmp_size
      private :: check_sph_rj_openmp_size
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_check_sph_mhd_openmp_size(WK_leg, sph)
!
      type(legendre_trns_works), intent(in) :: WK_leg
      type(sph_grids), intent(in) :: sph
!
!
      call check_legendre_openmp_size(WK_leg%id_legendre, sph%sph_rtm)
      call check_zonal_FFT_openmp_size(sph%sph_rtp)
      call check_sph_rj_openmp_size(sph%sph_rj)
!
      end subroutine s_check_sph_mhd_openmp_size
!
! -----------------------------------------------------------------------
!
      subroutine check_legendre_openmp_size(id_legendre, sph_rtm)
!
      use m_error_IDs
!
      integer(kind = kint), intent(in) :: id_legendre
      type(sph_rtm_grid), intent(in) :: sph_rtm
!
!
      if     (id_legendre .eq. iflag_leg_sym_mat_jt                     &
     &   .or. id_legendre .eq. iflag_leg_sym_dgemm_jt                   &
     &   .or. id_legendre .eq. iflag_leg_sym_mat_tj                     &
     &   .or. id_legendre .eq. iflag_leg_sym_dgemm_tj                   &
     &   .or. id_legendre .eq. iflag_on_the_fly_matprod                 &
     &   .or. id_legendre .eq. iflag_on_the_fly_matmul                  &
     &   .or. id_legendre .eq. iflag_on_the_fly_dgemm)  return
      if(sph_rtm%nidx_rtm(1) .ge. np_smp) return
!
      write(e_message,'(2a,i5)')                                        &
     &     ' OpenMP threads needs to be less than number of',           &
     &     ' radial grid in physical space', sph_rtm%nidx_rtm(1)
      call calypso_mpi_abort(ierr_P_SMP, e_message)
!
      end subroutine check_legendre_openmp_size
!
! -----------------------------------------------------------------------
!
      subroutine check_zonal_FFT_openmp_size(sph_rtp)
!
      use m_error_IDs
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
!
      if((sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(2)) .ge. np_smp) return
!
      write(e_message,'(2a,i5)')                                        &
     &     ' OpenMP threads needs to be less than number of',           &
     &     ' product of radial and meridinal grid in physical space',   &
     &      (sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(2))
      call calypso_mpi_abort(ierr_P_SMP, e_message)
!
      end subroutine check_zonal_FFT_openmp_size
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_rj_openmp_size(sph_rj)
!
      use m_error_IDs
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      if(sph_rj%nidx_rj(2) .ge. np_smp) return
      write(e_message,'(a,i5)')                                         &
     &     'OpenMP threads needs to be less than ', sph_rj%nidx_rj(2)
      call calypso_mpi_abort(ierr_P_SMP, e_message)
!
      end subroutine check_sph_rj_openmp_size
!
! -----------------------------------------------------------------------
!
      end module check_sph_mhd_openmp_size
