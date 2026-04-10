!>@file   set_filter_BC_to_center.f90
!!@brief  module set_filter_BC_to_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at CMB
!!
!!@verbatim
!!      subroutine sph_scalar_filter_to_center(l_truncation, sph_rj,    &
!!     &                                       bc_mag, BC_Sspec)
!!      subroutine sph_vector_filter_to_center(l_truncation, sph_rj,    &
!!     &                                       bc_mag, BC_Vspec)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        real(kind = kreal), intent(in) :: bc_mag
!!        type(sph_scalar_BC_coef), intent(inout) :: BC_Sspec
!!        type(sph_vector_BC_coef), intent(inout) :: BC_Vspec
!!@endverbatim
!!
      module set_filter_BC_to_center
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      implicit none
!
      real(kind = kreal), parameter :: r_pwr = 0.5d0
!
      private :: truncation_posiion_to_center
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_scalar_filter_to_center(l_truncation, sph_rj,      &
     &                                       bc_mag, BC_Sspec)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: bc_mag
!
      type(sph_scalar_BC_coef), intent(inout) :: BC_Sspec
!
      real(kind = kreal), allocatable :: r_coef(:)
      integer(kind = kint) :: j, l
!
!
      allocate(r_coef(0:l_truncation))
      r_coef(0:l_truncation) = 0.0d0
!
      call truncation_posiion_to_center(l_truncation, sph_rj,           &
     &                                  bc_mag, r_pwr, r_coef)
!
      do j = 1, sph_rj%nidx_rj(2)
        l = sph_rj%idx_gl_1d_rj_j(j,2)
        BC_Sspec%S_BC(j) = r_coef(l)
      end do
      deallocate(r_coef)
!
      end subroutine sph_scalar_filter_to_center
!
! -----------------------------------------------------------------------
!
      subroutine sph_vector_filter_to_center(l_truncation, sph_rj,      &
     &                                       bc_mag, BC_Vspec)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: bc_mag
!
      type(sph_vector_BC_coef), intent(inout) :: BC_Vspec
!
      real(kind = kreal), allocatable :: r_coef(:)
      integer(kind = kint) :: j, l
!
!
      allocate(r_coef(0:l_truncation))
      r_coef(0:l_truncation) = 0.0d0
!
      call truncation_posiion_to_center(l_truncation, sph_rj,           &
     &                                  bc_mag, r_pwr, r_coef)
!
      do j = 1, sph_rj%nidx_rj(2)
        l = sph_rj%idx_gl_1d_rj_j(j,2)
        BC_Vspec%Vp_BC(j) = r_coef(l)
        BC_Vspec%Dp_BC(j) = r_coef(l)
        BC_Vspec%Vt_BC(j) = r_coef(l)
      end do
      deallocate(r_coef)
!
      end subroutine sph_vector_filter_to_center
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine truncation_posiion_to_center                           &
     &         (l_truncation, sph_rj, bc_mag, r_pwr, r_coef)
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: bc_mag, r_pwr
!
      real(kind = kreal), intent(inout) :: r_coef(0:l_truncation)
!
      integer(kind = kint) :: k, l, kst
      real(kind = kreal) :: r_trn, rin
!
!      integer(kind = kint) :: l_ltr
!
!
!      write(*,*) 'filter radius', bc_mag
!      r_trn = one
!      l_ltr = min(l_truncation, int(r_trn))
! 
!      do k = 1, sph_rj%nidx_rj(1)
!        r_trn = one + dble(l_truncation - 1)                         &
!     &                 * sqrt(sph_rj%radius_1d_rj_r(k)/ bc_mag)
!        l_ltr = min(l_truncation, int(r_trn))
!        write(*,*) k, sph_rj%radius_1d_rj_r(k), l_ltr, r_trn
!      end do
!
      r_coef(0) = 0.0d0
!
      kst = 1
      do l = 1, l_truncation
        r_trn = (dble(l - 1) / dble(l_truncation - 1))**(one / r_pwr)   &
     &         * bc_mag
        do k = kst, sph_rj%nidx_rj(1)
          if(r_trn .lt. sph_rj%radius_1d_rj_r(k)) then
            kst = k - 1
            if(k .eq. 1) then
              rin = 0.0d0
            else
              rin = sph_rj%radius_1d_rj_r(k-1)
            end if
            r_coef(l) = dble(k-1)                                       &
     &               + (r_trn - rin) / (sph_rj%radius_1d_rj_r(k) - rin)
!
            exit
          end if
        end do
!        write(*,*) l, r_trn, r_coef(l), kst,                          &
!     &        sph_rj%radius_1d_rj_r(kst), sph_rj%radius_1d_rj_r(kst+1)
        if(kst .eq. 0) kst = 1
      end do
!
      end subroutine truncation_posiion_to_center
!
! -----------------------------------------------------------------------
!
      end module set_filter_BC_to_center
