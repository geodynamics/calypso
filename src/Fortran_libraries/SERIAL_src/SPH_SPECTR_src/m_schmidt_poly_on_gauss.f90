!
!      module m_schmidt_poly_on_gauss
!
!     Written by H. Matsui on June, 2007
!
!>@file   m_schmidt_poly_on_gauss.f90
!!@brief  module m_schmidt_poly_on_gauss
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Array for Legendre polyonoials and its derivatives
!!
!!@verbatim
!!      subroutine allocate_stack_smdt_med_smp(np_smp)
!!      subroutine deallocate_stack_smdt_med_smp
!!
!!      subroutine allocate_gauss_colat_med
!!      subroutine allocate_schmidt_poly_med
!!      subroutine allocate_fully_legendre_med
!!      subroutine allocate_legendre_med
!!
!!      subroutine deallocate_gauss_colat_med
!!      subroutine deallocate_schmidt_poly_med
!!      subroutine deallocate_fully_legendre_med
!!      subroutine deallocate_legendre_med
!!
!!      subroutine check_gauss_colat_med
!!      subroutine check_schmidt_poly_med(idx)
!!      subroutine check_fully_legendre_med(idx)
!!      subroutine check_legendre_poly_med(idx)
!!@endverbatim
!
      module m_schmidt_poly_on_gauss
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: nth_g
      integer(kind = kint) :: ltr_g, jmax_g
!
      real(kind = kreal), allocatable :: istack_nth_g_smp(:)
      real(kind = kreal), allocatable :: istack_ltr_g_smp(:)
      real(kind = kreal), allocatable :: istack_jmax_g_smp(:)
!
!>      Colatitude of of Gauss integration points (Rad)
      real(kind = kreal), allocatable :: g_point_med(:)
!>      Colatitude of of Gauss integration points (Deg)
      real(kind = kreal), allocatable :: g_colat_med(:)
!>      Weighting of Gauss integration
      real(kind = kreal), allocatable :: weight_med(:)
!
!>      Legendre polynomials with Schmidt normalization
      real(kind = kreal), allocatable :: P_smdt(:,:)
!>      derivative of Legendre polynomials with Schmidt normalization
      real(kind = kreal), allocatable :: dPdt_smdt(:,:)
!
!>      Legendre polynomials with fully normalization
      real(kind = kreal), allocatable :: P_full(:,:)
!>      derivative of Legendre polynomials with fully normalization
      real(kind = kreal), allocatable :: dPdt_full(:,:)
!
!>      Legendre polynomials with no normalization
      real(kind = kreal), allocatable :: P_org(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_stack_smdt_med_smp(np_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
      allocate( istack_nth_g_smp(0:np_smp) )
      allocate( istack_ltr_g_smp(0:np_smp) )
      allocate( istack_jmax_g_smp(0:np_smp) )
!
      istack_nth_g_smp = 0
      istack_ltr_g_smp = 0
      istack_jmax_g_smp = 0
!
      end subroutine allocate_stack_smdt_med_smp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_stack_smdt_med_smp
!
      deallocate( istack_nth_g_smp )
      deallocate( istack_ltr_g_smp )
      deallocate( istack_jmax_g_smp )
!
      end subroutine deallocate_stack_smdt_med_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_gauss_colat_med
!
      allocate( g_point_med(nth_g) )
      allocate( g_colat_med(nth_g) )
      allocate( weight_med(nth_g) )
!
      g_point_med = 0.0d0
      g_colat_med = 0.0d0
      weight_med = 0.0d0
!
      end subroutine allocate_gauss_colat_med
!
! -----------------------------------------------------------------------
!
      subroutine allocate_schmidt_poly_med
!
      allocate( P_smdt(nth_g, 0:jmax_g) )
      allocate( dPdt_smdt(nth_g, 0:jmax_g) )
!
      if(nth_g .eq. 0) return
      P_smdt = 0.0d0
      dPdt_smdt = 0.0d0
!
      end subroutine allocate_schmidt_poly_med
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fully_legendre_med
!
      allocate( P_full(nth_g, 0:jmax_g) )
      allocate( dPdt_full(nth_g, 0:jmax_g) )
!
      if(nth_g .eq. 0) return
      P_full = 0.0d0
      dPdt_full = 0.0d0
!
      end subroutine allocate_fully_legendre_med
!
! -----------------------------------------------------------------------
!
      subroutine allocate_legendre_med
!
      allocate( P_org(nth_g, 0:jmax_g) )
      if(nth_g .gt. 0) P_org = 0.0d0
!
      end subroutine allocate_legendre_med
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_gauss_colat_med
!
      deallocate( g_point_med )
      deallocate( g_colat_med )
      deallocate( weight_med )
!
      end subroutine deallocate_gauss_colat_med
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_schmidt_poly_med
!
      deallocate( P_smdt, dPdt_smdt )
!
      end subroutine deallocate_schmidt_poly_med
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_fully_legendre_med
!
      deallocate( P_full, dPdt_full )
!
      end subroutine deallocate_fully_legendre_med
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_legendre_med
!
      deallocate( P_org )
!
      end subroutine deallocate_legendre_med
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_gauss_colat_med
!
      integer(kind = kint) :: i
!
!
      write(50,*) 'num_gauss_points', nth_g
!
      do i = 1, nth_g
        write(50,'(i5,1p3E25.15e3)')                                    &
     &        i, g_point_med(i), g_colat_med(i), weight_med(i)
      end do
!
      end subroutine check_gauss_colat_med
!
! -----------------------------------------------------------------------
!
      subroutine check_schmidt_poly_med(idx)
!
      integer(kind = kint), intent(in) :: idx(0:jmax_g,2)
      integer(kind = kint) :: i, j
!
!
      write(50,*) 'num_gauss_points, truncation', nth_g, jmax_g
      write(50,*) 'med_no, j, l, m, P, dp/dth'
!
      do j = 0, jmax_g
        do i = 1, nth_g
          write(50,'(4i5,1p3E25.15e3)') i, j, idx(j,1), idx(j,2),       &
     &      P_smdt(i,j), dPdt_smdt(i,j)
        end do
      end do
!
      end subroutine check_schmidt_poly_med
!
! -----------------------------------------------------------------------
!
      subroutine check_fully_legendre_med(idx)
!
      integer(kind = kint), intent(in) :: idx(0:jmax_g,2)
      integer(kind = kint) :: i, j
!
!
      write(50,*) 'Fully normalized Legendre Polynomials'
      write(50,*) 'num_gauss_points, truncation', nth_g, jmax_g
      write(50,*) 'med_no, j, l, m, P, dp/dth'
!
      do j = 0, jmax_g
        do i = 1, nth_g
          write(50,'(4i5,1p3E25.15e3)') i, j, idx(j,1), idx(j,2),       &
     &      P_full(i,j), dPdt_full(i,j)
        end do
      end do
!
      end subroutine check_fully_legendre_med
!
! -----------------------------------------------------------------------
!
      subroutine check_legendre_poly_med(idx)
!
      integer(kind = kint), intent(in) :: idx(0:jmax_g,2)
      integer(kind = kint) :: i, j
!
!
      write(50,*) 'Legendre Polynomials without normalizatio'
      write(50,*) 'num_gauss_points, truncation', nth_g, jmax_g
      write(50,*) 'med_no, j, l, m, P, dp/dth'
!
      do j = 0, jmax_g
        do i = 1, nth_g
          write(50,'(4i5,1p3E25.15e3)') i, j, idx(j,1), idx(j,2),       &
     &      P_org(i,j)
        end do
      end do
!
      end subroutine check_legendre_poly_med
!
! -----------------------------------------------------------------------
!
      end module m_schmidt_poly_on_gauss
