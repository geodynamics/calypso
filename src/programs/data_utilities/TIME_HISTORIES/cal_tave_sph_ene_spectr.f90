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
!!      subroutine sum_average_ene_sph
!!      subroutine sum_deviation_ene_sph
!!      subroutine divide_average_ene_sph
!!      subroutine divide_deviation_ene_sph
!!
!!      subroutine output_tave_ene_sph_data
!!      subroutine output_tsigma_ene_sph_data
!!
!!      subroutine count_degree_on_layer_data
!!      subroutine count_degree_one_layer_data
!!      subroutine count_degree_on_volume_data
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
      use m_sph_ene_spectra
      use m_tave_sph_ene_spectr
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sum_average_ene_sph
!
      integer(kind = kint) :: kr, nd, lth
      real(kind= kreal) :: tmp, tmp_l, tmp_m, tmp_lm
!
!
!$omp parallel private(kr,lth,nd,tmp,tmp_l,tmp_m,tmp_lm)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_spec
          tmp = spectr_t(nd,kr)
          ave_spec_t(nd,kr) = ave_spec_t(nd,kr)                         &
     &           + half * (tmp + spectr_pre_t(nd,kr))                   &
     &            * (time_sph - pre_time)
          spectr_pre_t(nd,kr) = tmp
        end do
!$omp end do nowait
!
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp_sph_spec
            tmp_l =  spectr_l(nd,lth,kr)
            tmp_m =  spectr_m(nd,lth,kr)
            tmp_lm = spectr_lm(nd,lth,kr)
!
            ave_spec_l(nd,lth,kr) =  ave_spec_l(nd,lth,kr)              &
     &           + half * (tmp_l + spectr_pre_l(nd,lth,kr))             &
     &            * (time_sph - pre_time)
            ave_spec_m(nd,lth,kr) =  ave_spec_m(nd,lth,kr)              &
     &           + half * (tmp_m + spectr_pre_l(nd,lth,kr))             &
     &            * (time_sph - pre_time)
            ave_spec_lm(nd,lth,kr) = ave_spec_lm(nd,lth,kr)             &
     &           + half * (tmp_lm + spectr_pre_l(nd,lth,kr))            &
     &            * (time_sph - pre_time)
!
            spectr_pre_l(nd,lth,kr) =  tmp_l
            spectr_pre_m(nd,lth,kr) =  tmp_m
            spectr_pre_lm(nd,lth,kr) = tmp_lm
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      pre_time = time_sph
!
      end subroutine sum_average_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine sum_deviation_ene_sph
!
      integer(kind = kint) :: kr, nd, lth
      real(kind= kreal) :: tmp, tmp_l, tmp_m, tmp_lm
!
!
!$omp parallel private(kr,lth,nd,tmp,tmp_l,tmp_m,tmp_lm)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_spec
          tmp = (spectr_t(nd,kr) - ave_spec_t(nd,kr))**2
          sigma_spec_t(nd,kr) = sigma_spec_t(nd,kr)                     &
     &           + half * (tmp + spectr_pre_t(nd,kr))                   &
     &            * (time_sph - pre_time)
          spectr_pre_t(nd,kr) = tmp
        end do
!$omp end do nowait
!
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp_sph_spec
            tmp_l =  (spectr_l(nd,lth,kr) - ave_spec_l(nd,lth,kr))**2
            tmp_m =  (spectr_m(nd,lth,kr) - ave_spec_m(nd,lth,kr))**2
            tmp_lm = (spectr_lm(nd,lth,kr) - ave_spec_lm(nd,lth,kr))**2
!
            sigma_spec_l(nd,lth,kr) =  sigma_spec_l(nd,lth,kr)          &
     &           + half * (tmp_l + spectr_pre_l(nd,lth,kr))             &
     &            * (time_sph - pre_time)
            sigma_spec_m(nd,lth,kr) =  sigma_spec_m(nd,lth,kr)          &
     &           + half * (tmp_m + spectr_pre_l(nd,lth,kr))             &
     &            * (time_sph - pre_time)
            sigma_spec_lm(nd,lth,kr) = sigma_spec_lm(nd,lth,kr)         &
     &           + half * (tmp_lm + spectr_pre_l(nd,lth,kr))            &
     &            * (time_sph - pre_time)
!
            spectr_pre_l(nd,lth,kr) =  tmp_l
            spectr_pre_m(nd,lth,kr) =  tmp_m
            spectr_pre_lm(nd,lth,kr) = tmp_lm
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      pre_time = time_sph
!
      end subroutine sum_deviation_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine divide_average_ene_sph
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_spec
          ave_spec_t(nd,kr) = ave_spec_t(nd,kr) / (time_sph - time_ini)
        end do
!$omp end do nowait
!
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp_sph_spec
            ave_spec_l(nd,lth,kr) =  ave_spec_l(nd,lth,kr)              &
     &                              /  (time_sph - time_ini)
            ave_spec_m(nd,lth,kr) =  ave_spec_m(nd,lth,kr)              &
     &                              /  (time_sph - time_ini)
            ave_spec_lm(nd,lth,kr) = ave_spec_lm(nd,lth,kr)             &
     &                              /  (time_sph - time_ini)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine divide_average_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine divide_deviation_ene_sph
!
      integer(kind = kint) :: kr, nd, lth
!
!
!$omp parallel private(kr,lth,nd)
      do kr = 1, nri_sph
!$omp do
        do nd = 1, ncomp_sph_spec
          sigma_spec_t(nd,kr) = sqrt(sigma_spec_t(nd,kr)                &
     &                                 / (time_sph - time_ini))
        end do
!$omp end do nowait
!
        do lth = 0, ltr_sph
!$omp do
          do nd = 1, ncomp_sph_spec
            sigma_spec_l(nd,lth,kr)  =  sqrt(sigma_spec_l(nd,lth,kr)    &
     &                                 / (time_sph - time_ini))
            sigma_spec_m(nd,lth,kr)  =  sqrt(sigma_spec_m(nd,lth,kr)    &
     &                                 / (time_sph - time_ini))
            sigma_spec_lm(nd,lth,kr) =  sqrt(sigma_spec_lm(nd,lth,kr)   &
     &                                 / (time_sph - time_ini))
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine divide_deviation_ene_sph
!
!   --------------------------------------------------------------------
!
      subroutine output_tave_ene_sph_data
!
!
      call open_tave_ene_spec_data
!
      if(iflag_sph_ene_file .eq. 1) then
        call write_tave_vol_sph_data(nri_sph, ltr_sph, ncomp_sph_spec,  &
     &      ave_spec_t, ave_spec_l, ave_spec_m, ave_spec_lm)
      else
        call write_tave_layer_sph_data(nri_sph, ltr_sph,                &
     &      ncomp_sph_spec, ave_spec_t, ave_spec_l, ave_spec_m,         &
     &      ave_spec_lm)
      end if
!
      call close_ene_spec_data
!
      end subroutine output_tave_ene_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine output_tsigma_ene_sph_data
!
!
      call open_tsigma_ene_spec_data
!
      if(iflag_sph_ene_file .eq. 1) then
        call write_tave_vol_sph_data(nri_sph, ltr_sph, ncomp_sph_spec,  &
     &    sigma_spec_t, sigma_spec_l, sigma_spec_m, sigma_spec_lm)
      else
        call write_tave_layer_sph_data(nri_sph, ltr_sph,                &
     &      ncomp_sph_spec, sigma_spec_t, sigma_spec_l, sigma_spec_m,   &
     &      sigma_spec_lm)
      end if
!
      call close_ene_spec_data
!
      end subroutine output_tsigma_ene_sph_data
!
!   --------------------------------------------------------------------
!
      end module cal_tave_sph_ene_spectr
