!>@file   set_legendre_matrices.f90
!!@brief  module set_legendre_matrices
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2014
!
!>@brief  set legendre polynomials into matrices
!!
!!@verbatim
!!      subroutine set_trans_legendre_rtm(nth_rtm, jmax_rlm,            &
!!     &          P_rtm, dPdt_rtm, P_jl, dPdt_jl)
!!
!!      subroutine set_sym_legendre_stack                               &
!!     &         (mphi_rtm, lstack_rlm, lstack_even_rlm)
!!      subroutine set_symmetric_legendre_lj(nth_rtm, mphi_rtm,         &
!!     &           jmax_rlm, nth_hemi_rtm, lstack_rlm, lstack_even_rlm, &
!!     &           P_rtm, dPdt_rtm, Ps_rtm, dPsdt_rtm)
!!      subroutine set_symmetric_legendre_jl(nth_rtm, mphi_rtm,         &
!!     &           jmax_rlm, nth_hemi_rtm, lstack_rlm, lstack_even_rlm, &
!!     &           P_rtm, dPdt_rtm, Ps_jl, dPsdt_jl)
!!@endverbatim
!
      module set_legendre_matrices
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
      subroutine set_trans_legendre_rtm(nth_rtm, jmax_rlm,              &
     &          P_rtm, dPdt_rtm, P_jl, dPdt_jl)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      real(kind= kreal), intent(inout) :: P_jl(jmax_rlm,nth_rtm)
      real(kind= kreal), intent(inout) :: dPdt_jl(jmax_rlm,nth_rtm)
!
!
      integer(kind = kint) :: l_rtm, j_rlm
!
!
!$omp parallel do private(j_rlm,l_rtm)
      do j_rlm = 1, jmax_rlm
        do l_rtm = 1, nth_rtm
          P_jl(j_rlm,l_rtm) =     P_rtm(l_rtm,j_rlm)
          dPdt_jl(j_rlm,l_rtm) =  dPdt_rtm(l_rtm,j_rlm)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_trans_legendre_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sym_legendre_stack                                 &
     &         (mphi_rtm, lstack_rlm, lstack_even_rlm)
!
      integer(kind = kint), intent(in) :: mphi_rtm
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: lstack_even_rlm(0:mphi_rtm)
!
      integer(kind = kint) :: mp_rlm, jst, nj_rlm
!
!
      do mp_rlm = 1, mphi_rtm
        jst = lstack_rlm(mp_rlm-1)
        nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
        lstack_even_rlm(mp_rlm) = jst + (nj_rlm+1) / 2
      end do
!
      end subroutine set_sym_legendre_stack
!
! -----------------------------------------------------------------------
!
      subroutine set_symmetric_legendre_lj(nth_rtm, mphi_rtm,           &
     &           jmax_rlm, nth_hemi_rtm, lstack_rlm, lstack_even_rlm,   &
     &           P_rtm, dPdt_rtm, Ps_rtm, dPsdt_rtm)
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: nth_hemi_rtm
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
      integer(kind = kint), intent(in) :: lstack_even_rlm(0:mphi_rtm)
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      real(kind= kreal), intent(inout) :: Ps_rtm(nth_hemi_rtm,jmax_rlm)
      real(kind= kreal), intent(inout)                                  &
     &                  :: dPsdt_rtm(nth_hemi_rtm,jmax_rlm)
!
      integer(kind = kint) :: l_rtm, j_rlm
      integer(kind = kint) :: mp_rlm, jst, n_jk_e, n_jk_o, jj
!
!
!$omp parallel do private(jst,j_rlm,l_rtm,jj,n_jk_e,n_jk_o)
      do mp_rlm = 1, mphi_rtm
        jst = lstack_rlm(mp_rlm-1)
        n_jk_e = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
        n_jk_o = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
        do jj = 1, n_jk_e
          j_rlm = 2*jj + jst - 1
          do l_rtm = 1, nth_hemi_rtm
            Ps_rtm(l_rtm,jj+jst) =     P_rtm(l_rtm,j_rlm)
            dPsdt_rtm(l_rtm,jj+jst) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
!
        do jj = 1, n_jk_o
          j_rlm = 2*jj + jst
          do l_rtm = 1, nth_hemi_rtm
            Ps_rtm(l_rtm,jj+jst+n_jk_e) =     P_rtm(l_rtm,j_rlm)
            dPsdt_rtm(l_rtm,jj+jst+n_jk_e) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_symmetric_legendre_lj
!
! -----------------------------------------------------------------------
!
      subroutine set_symmetric_legendre_jl(nth_rtm, mphi_rtm,           &
     &           jmax_rlm, nth_hemi_rtm, lstack_rlm, lstack_even_rlm,   &
     &           P_rtm, dPdt_rtm, Ps_jl, dPsdt_jl)
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: nth_hemi_rtm
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
      integer(kind = kint), intent(in) :: lstack_even_rlm(0:mphi_rtm)
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      real(kind= kreal), intent(inout) :: Ps_jl(jmax_rlm,nth_hemi_rtm)
      real(kind= kreal), intent(inout)                                  &
     &                  :: dPsdt_jl(jmax_rlm,nth_hemi_rtm)
!
      integer(kind = kint) :: l_rtm, j_rlm
      integer(kind = kint) :: mp_rlm, jst, n_jk_e, n_jk_o, jj
!
!
!$omp parallel do private(jst,j_rlm,l_rtm,jj,n_jk_e,n_jk_o)
      do mp_rlm = 1, mphi_rtm
        jst = lstack_rlm(mp_rlm-1)
        n_jk_e = lstack_even_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
        n_jk_o = lstack_rlm(mp_rlm) - lstack_even_rlm(mp_rlm)
!
        do jj = 1, n_jk_e
          j_rlm = 2*jj + jst - 1
          do l_rtm = 1, nth_hemi_rtm
            Ps_jl(jj+jst,l_rtm) =     P_rtm(l_rtm,j_rlm)
            dPsdt_jl(jj+jst,l_rtm) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
!
        do jj = 1, n_jk_o
          j_rlm = 2*jj + jst
          do l_rtm = 1, nth_hemi_rtm
            Ps_jl(jj+jst+n_jk_e,l_rtm) =     P_rtm(l_rtm,j_rlm)
            dPsdt_jl(jj+jst+n_jk_e,l_rtm) =  dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_symmetric_legendre_jl
!
! -----------------------------------------------------------------------
!
      end module set_legendre_matrices
