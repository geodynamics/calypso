!>@file   cal_tave_sph_ene_spectr.f90
!!        module cal_tave_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  May, 2008
!!
!
!> @brief Evaluate time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine sum_average_ene_spectr                               &
!!     &         (time_sph, pre_time, nri_sph, ltr_sph, ncomp, spectr_l,&
!!     &          ave_spec_l, spectr_pre_l)
!!      subroutine sum_deviation_ene_spectr(time_sph, pre_time,         &
!!     &          nri_sph, ltr_sph, ncomp, spectr_l, ave_spec_l,        &
!!     &          sigma_spec_l, spectr_pre_l)
!!
!!      subroutine copy_ene_spectr_2_pre                                &
!!     &         (time_sph, pre_time, nri_sph, ltr_sph, ncomp, spectr_l,&
!!     &          ave_spec_l, spectr_pre_l)
!!      subroutine copy_deviation_ene_2_pre(time_sph, pre_time,         &
!!     &          nri_sph, ltr_sph, ncomp, spectr_l, ave_spec_l,        &
!!     &          sigma_spec_l, spectr_pre_l)
!!
!!      subroutine divide_average_ene_spectr(time_sph, time_ini,        &
!!     &          nri_sph, ltr_sph, ncomp, ave_spec_l, spectr_IO)
!!      subroutine divide_deviation_ene_spectr(time_sph, time_ini,      &
!!     &          nri_sph, ltr_sph, ncomp, sigma_spec_l, spectr_IO)
!!@endverbatim
!!
!!@n @param istep  time step number
!!@n @param icou   counter for snapshots
!!@n @param ierr   error flag
!
      module cal_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sum_average_ene_spectr                                 &
     &         (time_sph, pre_time, nri_sph, ltr_sph, ncomp, spectr_l,  &
     &          ave_spec_l, spectr_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout)                                 &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_pre_l(ncomp, 0:ltr_sph, nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
      real(kind= kreal) :: tmp_l
!
!
!$omp parallel private(kr,lth,nd,tmp_l)
      do kr = 1, nri_sph
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp
            tmp_l =  spectr_l(nd,lth,kr)
!
            ave_spec_l(nd,lth,kr) =  ave_spec_l(nd,lth,kr)              &
     &           + half * (tmp_l + spectr_pre_l(nd,lth,kr))             &
     &            * (time_sph - pre_time)
            spectr_pre_l(nd,lth,kr) =  tmp_l
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      pre_time = time_sph
!
      end subroutine sum_average_ene_spectr
!
!   --------------------------------------------------------------------
!
      subroutine sum_deviation_ene_spectr(time_sph, pre_time,           &
     &          nri_sph, ltr_sph, ncomp, spectr_l, ave_spec_l,          &
     &          sigma_spec_l, spectr_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(in)                                    &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout)                                 &
     &                   :: sigma_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_pre_l(ncomp, 0:ltr_sph, nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
      real(kind= kreal) :: tmp_l
!
!
!$omp parallel private(kr,lth,nd,tmp_l)
      do kr = 1, nri_sph
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp
            tmp_l =  (spectr_l(nd,lth,kr) - ave_spec_l(nd,lth,kr))**2
!
            sigma_spec_l(nd,lth,kr) =  sigma_spec_l(nd,lth,kr)          &
     &           + half * (tmp_l + spectr_pre_l(nd,lth,kr))             &
     &            * (time_sph - pre_time)
            spectr_pre_l(nd,lth,kr) =  tmp_l
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      pre_time = time_sph
!
      end subroutine sum_deviation_ene_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine  copy_ene_spectr_2_pre                                 &
     &         (time_sph, pre_time, nri_sph, ltr_sph, ncomp, spectr_l,  &
     &          ave_spec_l, spectr_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout)                                 &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_pre_l(ncomp, 0:ltr_sph, nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp
!
            ave_spec_l(nd,lth,kr) =  0.0d0
            spectr_pre_l(nd,lth,kr) =  spectr_l(nd,lth,kr)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      pre_time = time_sph
!
      end subroutine copy_ene_spectr_2_pre
!
!   --------------------------------------------------------------------
!
      subroutine copy_deviation_ene_2_pre(time_sph, pre_time,           &
     &          nri_sph, ltr_sph, ncomp, spectr_l, ave_spec_l,          &
     &          sigma_spec_l, spectr_pre_l)
!
      real(kind = kreal), intent(in) :: time_sph
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(in)                                    &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout) :: pre_time
      real(kind = kreal), intent(inout)                                 &
     &                   :: sigma_spec_l(ncomp, 0:ltr_sph, nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_pre_l(ncomp, 0:ltr_sph, nri_sph)
!
      integer(kind = kint) :: kr, nd, lth
      real(kind= kreal) :: tmp_l
!
!
!$omp parallel private(kr,lth,nd,tmp_l)
      do kr = 1, nri_sph
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp
            tmp_l =  (spectr_l(nd,lth,kr) - ave_spec_l(nd,lth,kr))**2
!
            sigma_spec_l(nd,lth,kr) = 0.0d0
            spectr_pre_l(nd,lth,kr) =  tmp_l
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      pre_time = time_sph
!
      end subroutine copy_deviation_ene_2_pre
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine divide_average_ene_spectr(time_sph, time_ini,          &
     &          nri_sph, ltr_sph, ncomp, ave_spec_l, spectr_IO)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: ave_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ncomp, 0:ltr_sph, nri_sph)
!
!
!$omp parallel workshare
      spectr_IO(1:ncomp,0:ltr_sph,1:nri_sph)                            &
     &         = ave_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)                &
     &          / (time_sph - time_ini)
!$omp end parallel workshare
!
      end subroutine divide_average_ene_spectr
!
!   --------------------------------------------------------------------
!
      subroutine divide_deviation_ene_spectr(time_sph, time_ini,        &
     &          nri_sph, ltr_sph, ncomp, sigma_spec_l, spectr_IO)
!
      real(kind = kreal), intent(in) :: time_sph, time_ini
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: sigma_spec_l(ncomp, 0:ltr_sph, nri_sph)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ncomp, 0:ltr_sph, nri_sph)
!
!
!$omp parallel workshare
      spectr_IO(1:ncomp,0:ltr_sph,1:nri_sph)                            &
     &      = sqrt(sigma_spec_l(1:ncomp,0:ltr_sph,1:nri_sph)            &
     &       / (time_sph - time_ini))
!$omp end parallel workshare
!
      end subroutine divide_deviation_ene_spectr
!
!   --------------------------------------------------------------------
!
      end module cal_tave_sph_ene_spectr
