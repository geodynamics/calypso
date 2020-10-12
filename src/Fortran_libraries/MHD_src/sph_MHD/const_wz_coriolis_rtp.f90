!>@file   const_wz_coriolis_rtp.f90
!!@brief  module const_wz_coriolis_rtp
!!
!!@author H. Matsui
!!@date Programmed in May, 2013
!
!>@brief  Evaluate Coriolis term on spherical grid
!!
!!@verbatim
!!      subroutine sel_wz_coriolis_rtp                                  &
!!     &         (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
!!      subroutine sel_wz_div_coriolis_rtp                              &
!!     &         (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!
!!      subroutine cal_wz_coriolis_pole                                 &
!!     &         (nnod_pole, coef_cor, velo_pole, coriolis_pole)
!!@endverbatim
!!
!!@n @param irj_fld   Address for spectr data
!
      module const_wz_coriolis_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rtp_data
      use t_schmidt_poly_on_rtm
!
      implicit none
!
      private :: cal_wz_coriolis_rtp, cal_wz_coriolis_prt
      private :: cal_wz_div_coriolis_rtp, cal_wz_div_coriolis_prt
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_wz_coriolis_rtp                                    &
     &         (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: velo_rtp(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: coriolis_rtp(sph_rtp%nnod_rtp,3)
!
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call cal_wz_coriolis_prt                                        &
     &     (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
      else
        call cal_wz_coriolis_rtp                                        &
     &     (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
      end if
!
      end subroutine sel_wz_coriolis_rtp
!
! -----------------------------------------------------------------------
!
      subroutine sel_wz_div_coriolis_rtp                                &
     &         (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: velo_rtp(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: coriolis_rtp(sph_rtp%nnod_rtp,3)
!
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call cal_wz_div_coriolis_prt                                    &
     &     (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
      else
        call cal_wz_div_coriolis_rtp                                    &
     &     (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
      end if
!
      end subroutine sel_wz_div_coriolis_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_wz_coriolis_pole                                   &
     &         (nnod_pole, coef_cor, velo_pole, coriolis_pole)
!
      integer(kind = kint), intent(in) :: nnod_pole
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: velo_pole(nnod_pole,3)
!
      real(kind = kreal), intent(inout) :: coriolis_pole(nnod_pole,3)
!
      integer(kind = kint) :: inod
      real(kind = kreal), parameter :: omega(3) = (/zero, zero, one/)
!
!
!$omp parallel do private(inod)
      do inod = 1, nnod_pole
            coriolis_pole(inod,1) = - coef_cor                          &
!     &                         * ( omega(2)*velo_pole(inod,3)          &
     &                         * ( -omega(3)*velo_pole(inod,2) )
            coriolis_pole(inod,2) = - coef_cor                          &
     &                         * ( omega(3)*velo_pole(inod,1) )
!     &                           - omega(1)*velo_pole(inod,3) )
            coriolis_pole(inod,3) = zero
!            coriolis_pole(inod,3) = - coef_cor                         &
!     &                         * ( omega(1)*velo_pole(inod,2)          &
!     &                           - omega(2)*velo_pole(inod,1) )
      end do
!$omp end parallel do
!
      end subroutine cal_wz_coriolis_pole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_wz_coriolis_rtp                                    &
     &         (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: velo_rtp(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: coriolis_rtp(sph_rtp%nnod_rtp,3)
!
      integer(kind = kint) :: mphi, l_rtp, kr, inod
      real(kind = kreal) :: omega(3)
!
!
!$omp parallel do private(mphi,l_rtp,kr,inod,omega)
      do mphi = 1, sph_rtp%nidx_rtp(3)
        do l_rtp = 1, sph_rtp%nidx_rtp(2)
          omega(1) =  cos( leg%g_colat_rtp(l_rtp) )
          omega(2) = -sin( leg%g_colat_rtp(l_rtp) )
          omega(3) = zero
!
          do kr = 1, sph_rtp%nidx_rtp(1)
            inod = kr + (l_rtp-1) * sph_rtp%istep_rtp(2)                &
     &                + (mphi-1)  * sph_rtp%istep_rtp(3)
!
            coriolis_rtp(inod,1) = - coef_cor                           &
     &                         * ( omega(2)*velo_rtp(inod,3)            &
     &                           - omega(3)*velo_rtp(inod,2) )
            coriolis_rtp(inod,2) = - coef_cor                           &
     &                         * ( omega(3)*velo_rtp(inod,1)            &
     &                           - omega(1)*velo_rtp(inod,3) )
            coriolis_rtp(inod,3) = - coef_cor                           &
     &                         * ( omega(1)*velo_rtp(inod,2)            &
     &                           - omega(2)*velo_rtp(inod,1) )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_wz_coriolis_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_wz_coriolis_prt                                    &
     &         (sph_rtp, leg, coef_cor, velo_rtp, coriolis_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: velo_rtp(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: coriolis_rtp(sph_rtp%nnod_rtp,3)
!
      integer(kind = kint) :: mphi, l_rtp, kr, inod
      real(kind = kreal) :: omega(3)
!
!
!$omp parallel do private(mphi,l_rtp,kr,inod,omega)
      do l_rtp = 1, sph_rtp%nidx_rtp(2)
        omega(1) =  cos( leg%g_colat_rtp(l_rtp) )
        omega(2) = -sin( leg%g_colat_rtp(l_rtp) )
        omega(3) = zero
!
        do kr = 1, sph_rtp%nidx_rtp(1)
          do mphi = 1, sph_rtp%nidx_rtp(3)
            inod = mphi + (l_rtp-1) * sph_rtp%istep_rtp(2)              &
     &                  + (kr-1) *    sph_rtp%istep_rtp(1)
!
            coriolis_rtp(inod,1) = - coef_cor                           &
     &                         * ( omega(2)*velo_rtp(inod,3)            &
     &                           - omega(3)*velo_rtp(inod,2) )
            coriolis_rtp(inod,2) = - coef_cor                           &
     &                         * ( omega(3)*velo_rtp(inod,1)            &
     &                           - omega(1)*velo_rtp(inod,3) )
            coriolis_rtp(inod,3) = - coef_cor                           &
     &                         * ( omega(1)*velo_rtp(inod,2)            &
     &                           - omega(2)*velo_rtp(inod,1) )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_wz_coriolis_prt
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_wz_div_coriolis_rtp                                &
     &         (sph_rtp, leg, coef_cor, velo_rtp, div_coriolis_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: velo_rtp(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: div_coriolis_rtp(sph_rtp%nnod_rtp)
!
      integer(kind = kint) :: mphi, l_rtp, kr, inod
      real(kind = kreal) :: omega(3)
!
!
!$omp parallel do private(mphi,l_rtp,kr,inod,omega)
      do mphi = 1, sph_rtp%nidx_rtp(3)
        do l_rtp = 1, sph_rtp%nidx_rtp(2)
          omega(1) =  cos( leg%g_colat_rtp(l_rtp) )
          omega(2) = -sin( leg%g_colat_rtp(l_rtp) )
          omega(3) = zero
!
          do kr = 1, sph_rtp%nidx_rtp(1)
            inod = kr + (l_rtp-1) * sph_rtp%istep_rtp(2)                &
     &                + (mphi-1)  * sph_rtp%istep_rtp(3)
!
            div_coriolis_rtp(inod)                                      &
     &          = coef_cor * ( omega(1)*velo_rtp(inod,1)                &
     &                       - omega(2)*velo_rtp(inod,2) )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_wz_div_coriolis_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_wz_div_coriolis_prt                                &
     &         (sph_rtp, leg, coef_cor, velo_rtp, div_coriolis_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(legendre_4_sph_trans), intent(in) :: leg
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: velo_rtp(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: div_coriolis_rtp(sph_rtp%nnod_rtp)
!
      integer(kind = kint) :: mphi, l_rtp, kr, inod
      real(kind = kreal) :: omega(3)
!
!
!$omp parallel do private(mphi,l_rtp,kr,inod,omega)
      do l_rtp = 1, sph_rtp%nidx_rtp(2)
        omega(1) =  cos( leg%g_colat_rtp(l_rtp) )
        omega(2) = -sin( leg%g_colat_rtp(l_rtp) )
        omega(3) = zero
!
        do kr = 1, sph_rtp%nidx_rtp(1)
          do mphi = 1, sph_rtp%nidx_rtp(3)
            inod = mphi + (l_rtp-1) * sph_rtp%istep_rtp(2)              &
     &                  + (kr-1) *    sph_rtp%istep_rtp(1)
!
            div_coriolis_rtp(inod)                                      &
     &          = coef_cor * ( omega(1)*velo_rtp(inod,1)                &
     &                       - omega(2)*velo_rtp(inod,2) )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_wz_div_coriolis_prt
!
! -----------------------------------------------------------------------
!
      end module const_wz_coriolis_rtp
