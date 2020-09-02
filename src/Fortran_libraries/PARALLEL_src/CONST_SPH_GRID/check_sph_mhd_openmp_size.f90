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
!!        type(sph_grids), intent(in) :: sph
!!@endverbatim
!
      module check_sph_mhd_openmp_size
!
      use m_precision
!
      use calypso_mpi
      use t_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_check_sph_mhd_openmp_size(sph)
!
      use m_machine_parameter
      use m_error_IDs
!
      type(sph_grids), intent(in) :: sph
!
!
      if(sph%sph_rtp%nidx_rtp(1) .lt. np_smp) then
        write(e_message,'(a,i5)')                                       &
     &     'OpenMP threads needs to be less than ',                     &
     &     sph%sph_rtp%nidx_rtp(1)
        call calypso_mpi_abort(ierr_P_SMP, e_message)
      end if
!
      if(sph%sph_rtp%nidx_rtp(2) .lt. np_smp) then
        write(e_message,'(a,i5)')                                       &
     &     'OpenMP threads needs to be less than ',                     &
     &     sph%sph_rtp%nidx_rtp(2)
        call calypso_mpi_abort(ierr_P_SMP, e_message)
      end if
!
      if(sph%sph_rtm%nidx_rtm(3) .lt. np_smp) then
        write(e_message,'(a,i5)')                                      &
     &     'OpenMP threads needs to be less than ',                    &
     &      sph%sph_rtm%nidx_rtm(2)
        call calypso_mpi_abort(ierr_P_SMP, e_message)
      end if
!
      if(sph%sph_rj%nidx_rj(2)*sph%sph_rj%nidx_rj(2) .lt. np_smp) then
        write(e_message,'(a,i5)')                                      &
     &     'OpenMP threads needs to be less than ',                    &
     &     (sph%sph_rj%nidx_rj(2) * sph%sph_rj%nidx_rj(2))
        call calypso_mpi_abort(ierr_P_SMP, e_message)
      end if
!
      end subroutine s_check_sph_mhd_openmp_size
!
! -----------------------------------------------------------------------
!
      end module check_sph_mhd_openmp_size
