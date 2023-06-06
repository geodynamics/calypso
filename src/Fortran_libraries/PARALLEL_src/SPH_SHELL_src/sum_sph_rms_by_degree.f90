!>@file   sum_sph_rms_by_degree.f90
!!@brief      module sum_sph_rms_by_degree
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!> @brief  Evaluate mean square by spherical hermonics coefficients
!!
!!@verbatim
!!      subroutine set_sum_table_4_sph_spectr                           &
!!     &      (l_truncation, nidx_rj, idx_gl_1d_rj_j,                   &
!!     &       num_mode_sum_l, num_mode_sum_m, num_mode_sum_lm,         &
!!     &       istack_mode_sum_l, istack_mode_sum_m, istack_mode_sum_lm,&
!!     &       item_mode_sum_l, item_mode_sum_m, item_mode_sum_lm)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        integer(kind = kint), intent(in)                              &
!!     &           :: idx_gl_1d_rj_j(nidx_rj(2),3)
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: num_mode_sum_l(0:l_truncation)
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: num_mode_sum_m(0:l_truncation)
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: num_mode_sum_lm(0:l_truncation)
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: istack_mode_sum_l(-1:l_truncation)
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: istack_mode_sum_m(-1:l_truncation)
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: istack_mode_sum_lm(-1:l_truncation)
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: item_mode_sum_l(nidx_rj(2))
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: item_mode_sum_m(nidx_rj(2))
!!        integer(kind = kint), intent(inout)                           &
!!     &           :: item_mode_sum_lm(nidx_rj(2))
!!
!!      subroutine sum_sph_v_rms_by_degree(ltr, nidx_j, istack_sum,     &
!!     &          item_mode_4_sum, ncomp, rms_sph_vol_j, rms_sph_vlc)
!!        integer(kind = kint), intent(in) :: ltr, nidx_j
!!        integer(kind = kint), intent(in) :: ncomp
!!        integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
!!        integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_j)
!!        real(kind = kreal), intent(in) :: rms_sph_vol_j(nidx_j,3)
!!        real(kind = kreal), intent(inout) :: rms_sph_vlc(0:ltr,ncomp)
!!      subroutine sum_sph_l_rms_by_degree                              &
!!     &         (pwr, ltr, nidx_rj, istack_sum, item_mode_4_sum,       &
!!     &          ncomp, rms_sph_rj, rms_sph_lc)
!!        integer(kind = kint), intent(in) :: ltr
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        type(sph_mean_squares), intent(in) :: pwr
!!        integer(kind = kint), intent(in) :: ncomp
!!        integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
!!        integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_rj(2))
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: rms_sph_rj(0:nidx_rj(1),nidx_rj(2),3)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: rms_sph_lc(pwr%nri_rms,0:ltr,ncomp)
!!@endverbatim
!
      module sum_sph_rms_by_degree
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sum_table_4_sph_spectr                             &
     &      (l_truncation, nidx_rj, idx_gl_1d_rj_j,                     &
     &       num_mode_sum_l, num_mode_sum_m, num_mode_sum_lm,           &
     &       istack_mode_sum_l, istack_mode_sum_m, istack_mode_sum_lm,  &
     &       item_mode_sum_l, item_mode_sum_m, item_mode_sum_lm)
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
!
      integer(kind = kint), intent(inout)                               &
     &           :: num_mode_sum_l(0:l_truncation)
      integer(kind = kint), intent(inout)                               &
     &           :: num_mode_sum_m(0:l_truncation)
      integer(kind = kint), intent(inout)                               &
     &           :: num_mode_sum_lm(0:l_truncation)
!
      integer(kind = kint), intent(inout)                               &
     &           :: istack_mode_sum_l(-1:l_truncation)
      integer(kind = kint), intent(inout)                               &
     &           :: istack_mode_sum_m(-1:l_truncation)
      integer(kind = kint), intent(inout)                               &
     &           :: istack_mode_sum_lm(-1:l_truncation)
!
      integer(kind = kint), intent(inout)                               &
     &           :: item_mode_sum_l(nidx_rj(2))
      integer(kind = kint), intent(inout)                               &
     &           :: item_mode_sum_m(nidx_rj(2))
      integer(kind = kint), intent(inout)                               &
     &           :: item_mode_sum_lm(nidx_rj(2))
!
      integer(kind = kint) :: j, lg, mg, lm
      integer(kind = kint) :: icou, lcou, mcou
!
!
      num_mode_sum_l(0:l_truncation) =  0
      num_mode_sum_m(0:l_truncation) =  0
      num_mode_sum_lm(0:l_truncation) = 0
      do j = 1, nidx_rj(2)
        lg = idx_gl_1d_rj_j(j,2)
        mg = idx_gl_1d_rj_j(j,3)
        mg = abs(mg)
        lm = lg - mg
        num_mode_sum_l(lg) =  num_mode_sum_l(lg) +  1
        num_mode_sum_m(mg) =  num_mode_sum_m(mg) +  1
        num_mode_sum_lm(lm) = num_mode_sum_lm(lm) + 1
      end do
!
      istack_mode_sum_l(-1) =  0
      istack_mode_sum_m(-1) =  0
      istack_mode_sum_lm(-1) = 0
      do lm = 0, l_truncation
        istack_mode_sum_l(lm) = istack_mode_sum_l(lm-1)                 &
     &                         + num_mode_sum_l(lm)
        istack_mode_sum_m(lm) = istack_mode_sum_m(lm-1)                 &
     &                         + num_mode_sum_m(lm)
        istack_mode_sum_lm(lm) = istack_mode_sum_lm(lm-1)               &
     &                         + num_mode_sum_lm(lm)
      end do
!
      do lm = 0, l_truncation
        lcou = istack_mode_sum_l(lm-1)
        mcou = istack_mode_sum_m(lm-1)
        icou = istack_mode_sum_lm(lm-1)
        do j = 1, nidx_rj(2)
          lg = idx_gl_1d_rj_j(j,2)
          mg = idx_gl_1d_rj_j(j,3)
          mg = abs(mg)
          if (lg .eq. lm) then
            lcou = lcou + 1
            item_mode_sum_l(lcou) = j
          end if
          if (mg .eq. lm) then
            mcou = mcou + 1
            item_mode_sum_m(mcou) = j
          end if
          if ((lg-mg) .eq. lm) then
            icou = icou + 1
            item_mode_sum_lm(icou) = j
          end if
        end do
      end do
!
      end subroutine set_sum_table_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_sph_v_rms_by_degree(ltr, nidx_j, istack_sum,       &
     &          item_mode_4_sum, ncomp, rms_sph_vol_j, rms_sph_vlc)
!
      integer(kind = kint), intent(in) :: ltr, nidx_j
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_j)
      real(kind = kreal), intent(in) :: rms_sph_vol_j(nidx_j,3)
!
      real(kind = kreal), intent(inout) :: rms_sph_vlc(0:ltr,ncomp)
!
      integer(kind = kint) :: lm, j, l0, icomp
      integer(kind = kint) :: lst, led
!
!
!$omp parallel private(icomp)
      do icomp = 1, ncomp
!$omp do private(lm,lst,led,l0,j)
        do lm = 0, ltr
          lst = istack_sum(lm-1) + 1
          led = istack_sum(lm)
          do l0 = lst, led
            j = item_mode_4_sum(l0)
            rms_sph_vlc(lm,icomp) = rms_sph_vlc(lm,icomp)               &
     &                                + rms_sph_vol_j(j,icomp)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_sph_v_rms_by_degree
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_l_rms_by_degree                                &
     &         (pwr, ltr, nidx_rj, istack_sum, item_mode_4_sum,         &
     &          ncomp, rms_sph_rj, rms_sph_lc)
!
      use t_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: nidx_rj(2)
      type(sph_mean_squares), intent(in) :: pwr
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_rj(2))
      real(kind = kreal), intent(in)                                    &
     &                   :: rms_sph_rj(0:nidx_rj(1),nidx_rj(2),3)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: rms_sph_lc(pwr%nri_rms,0:ltr,ncomp)
!
      integer(kind = kint) :: lm, k, k_in, k_out, j, l0, icomp
      integer(kind = kint) :: lst, led
!
!
!$omp parallel private(icomp)
      do icomp = 1, ncomp
!$omp do private(k,k_in,k_out,lm,lst,led,l0,j)
        do k = 1, pwr%nri_rms
          k_in =  pwr%kr_4_rms(k,1)
          k_out = pwr%kr_4_rms(k,2)
          do lm = 0, ltr
            lst = istack_sum(lm-1) + 1
            led = istack_sum(lm)
            do l0 = lst, led
              j = item_mode_4_sum(l0)
              rms_sph_lc(k,lm,icomp) = rms_sph_lc(k,lm,icomp)           &
     &            +        pwr%c_gl_itp(k) * rms_sph_rj(k_in,j,icomp)   &
     &            + (one - pwr%c_gl_itp(k)) * rms_sph_rj(k_out,j,icomp)
            end do
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_sph_l_rms_by_degree
!
! -----------------------------------------------------------------------
!
      end module sum_sph_rms_by_degree
