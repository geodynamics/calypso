!>@file   t_schmidt_poly_on_gauss.f90
!!@brief  module t_schmidt_poly_on_gauss
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Array for Legendre polyonoials and its derivatives
!!
!!@verbatim
!!      subroutine alloc_stack_smdt_med_smp(np_smp, leg_d)
!!      subroutine dealloc_stack_smdt_med_smp(leg_d)
!!
!!      subroutine alloc_gauss_colat_med(nth, leg_d)
!!      subroutine alloc_schmidt_poly_med(leg_d)
!!      subroutine alloc_fully_legendre_med(leg_d)
!!      subroutine alloc_legendre_med(leg_d)
!!
!!      subroutine dealloc_gauss_colat_med(leg_d)
!!      subroutine dealloc_schmidt_poly_med(leg_d)
!!      subroutine dealloc_fully_legendre_med(leg_d)
!!      subroutine dealloc_legendre_med(leg_d)
!!        type(gauss_legendre_data), intent(inout) :: leg_d
!!
!!      subroutine check_gauss_colat_med(leg_d)
!!      subroutine check_schmidt_poly_med(idx, leg_d)
!!      subroutine check_fully_legendre_med(idx, leg_d)
!!      subroutine check_legendre_poly_med(idx, leg_d)
!!@endverbatim
!
      module t_schmidt_poly_on_gauss
!
      use m_precision
!
      implicit none
!
      type gauss_legendre_data
        integer(kind = kint) :: nth_g
        integer(kind = kint) :: ltr_g
        integer(kind = kint) :: jmax_g
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
      end type gauss_legendre_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_stack_smdt_med_smp(np_smp, leg_d)
!
      integer(kind = kint), intent(in) :: np_smp
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      allocate( leg_d%istack_nth_g_smp(0:np_smp) )
      allocate( leg_d%istack_ltr_g_smp(0:np_smp) )
      allocate( leg_d%istack_jmax_g_smp(0:np_smp) )
!
      leg_d%istack_nth_g_smp = 0
      leg_d%istack_ltr_g_smp = 0
      leg_d%istack_jmax_g_smp = 0
!
      end subroutine alloc_stack_smdt_med_smp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_stack_smdt_med_smp(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      deallocate( leg_d%istack_nth_g_smp )
      deallocate( leg_d%istack_ltr_g_smp )
      deallocate( leg_d%istack_jmax_g_smp )
!
      end subroutine dealloc_stack_smdt_med_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_colat_med(nth, leg_d)
!
      integer(kind = kint), intent(in) :: nth
      type(gauss_legendre_data), intent(inout) :: leg_d
!
!
      leg_d%nth_g = nth
!
      allocate( leg_d%g_point_med(leg_d%nth_g) )
      allocate( leg_d%g_colat_med(leg_d%nth_g) )
      allocate( leg_d%weight_med(leg_d%nth_g) )
!
      if(leg_d%nth_g .le. 0) return
      leg_d%g_point_med = 0.0d0
      leg_d%g_colat_med = 0.0d0
      leg_d%weight_med = 0.0d0
!
      end subroutine alloc_gauss_colat_med
!
! -----------------------------------------------------------------------
!
      subroutine alloc_schmidt_poly_med(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      allocate( leg_d%P_smdt(leg_d%nth_g, 0:leg_d%jmax_g) )
      allocate( leg_d%dPdt_smdt(leg_d%nth_g, 0:leg_d%jmax_g) )
!
      if(leg_d%nth_g .eq. 0) return
      leg_d%P_smdt = 0.0d0
      leg_d%dPdt_smdt = 0.0d0
!
      end subroutine alloc_schmidt_poly_med
!
! -----------------------------------------------------------------------
!
      subroutine alloc_fully_legendre_med(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      allocate( leg_d%P_full(leg_d%nth_g, 0:leg_d%jmax_g) )
      allocate( leg_d%dPdt_full(leg_d%nth_g, 0:leg_d%jmax_g) )
!
      if(leg_d%nth_g .eq. 0) return
      leg_d%P_full = 0.0d0
      leg_d%dPdt_full = 0.0d0
!
      end subroutine alloc_fully_legendre_med
!
! -----------------------------------------------------------------------
!
      subroutine alloc_legendre_med(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      allocate( leg_d%P_org(leg_d%nth_g, 0:leg_d%jmax_g) )
      if(leg_d%nth_g .gt. 0) leg_d%P_org = 0.0d0
!
      end subroutine alloc_legendre_med
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_colat_med(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      deallocate( leg_d%g_point_med )
      deallocate( leg_d%g_colat_med )
      deallocate( leg_d%weight_med )
!
      end subroutine dealloc_gauss_colat_med
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_schmidt_poly_med(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      deallocate( leg_d%P_smdt, leg_d%dPdt_smdt )
!
      end subroutine dealloc_schmidt_poly_med
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_fully_legendre_med(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      deallocate( leg_d%P_full, leg_d%dPdt_full )
!
      end subroutine dealloc_fully_legendre_med
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_legendre_med(leg_d)
!
      type(gauss_legendre_data), intent(inout) :: leg_d
!
      deallocate( leg_d%P_org )
!
      end subroutine dealloc_legendre_med
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_gauss_colat_med(leg_d)
!
      type(gauss_legendre_data), intent(in) :: leg_d
!
      integer(kind = kint) :: i
!
!
      write(50,*) 'num_gauss_points', leg_d%nth_g
!
      do i = 1, leg_d%nth_g
        write(50,'(i5,1p3E25.15e3)') i, leg_d%g_point_med(i),           &
     &        leg_d%g_colat_med(i), leg_d%weight_med(i)
      end do
!
      end subroutine check_gauss_colat_med
!
! -----------------------------------------------------------------------
!
      subroutine check_schmidt_poly_med(idx, leg_d)
!
      type(gauss_legendre_data), intent(in) :: leg_d
!
      integer(kind = kint), intent(in) :: idx(0:leg_d%jmax_g,2)
      integer(kind = kint) :: i, j
!
!
      write(50,*) 'num_gauss_points, truncation',                       &
     &           leg_d%nth_g, leg_d%jmax_g
      write(50,*) 'med_no, j, l, m, P, dp/dtheta'
!
      do j = 0, leg_d%jmax_g
        do i = 1, leg_d%nth_g
          write(50,'(4i5,1p3E25.15e3)') i, j, idx(j,1), idx(j,2),       &
     &      leg_d%P_smdt(i,j), leg_d%dPdt_smdt(i,j)
        end do
      end do
!
      end subroutine check_schmidt_poly_med
!
! -----------------------------------------------------------------------
!
      subroutine check_fully_legendre_med(idx, leg_d)
!
      type(gauss_legendre_data), intent(in) :: leg_d
!
      integer(kind = kint), intent(in) :: idx(0:leg_d%jmax_g,2)
      integer(kind = kint) :: i, j
!
!
      write(50,*) 'Fully normalized Legendre Polynomials'
      write(50,*) 'num_gauss_points, truncation',                       &
     &           leg_d%nth_g, leg_d%jmax_g
      write(50,*) 'med_no, j, l, m, P, dp/dtheta'
!
      do j = 0, leg_d%jmax_g
        do i = 1, leg_d%nth_g
          write(50,'(4i5,1p3E25.15e3)') i, j, idx(j,1), idx(j,2),       &
     &      leg_d%P_full(i,j), leg_d%dPdt_full(i,j)
        end do
      end do
!
      end subroutine check_fully_legendre_med
!
! -----------------------------------------------------------------------
!
      subroutine check_legendre_poly_med(idx, leg_d)
!
      type(gauss_legendre_data), intent(in) :: leg_d
!
      integer(kind = kint), intent(in) :: idx(0:leg_d%jmax_g,2)
      integer(kind = kint) :: i, j
!
!
      write(50,*) 'Legendre Polynomials without normalizatio'
      write(50,*) 'num_gauss_points, truncation',                       &
     &           leg_d%nth_g, leg_d%jmax_g
      write(50,*) 'med_no, j, l, m, P, dp/dtheta'
!
      do j = 0, leg_d%jmax_g
        do i = 1, leg_d%nth_g
          write(50,'(4i5,1p3E25.15e3)') i, j, idx(j,1), idx(j,2),       &
     &      leg_d%P_org(i,j)
        end do
      end do
!
      end subroutine check_legendre_poly_med
!
! -----------------------------------------------------------------------
!
      end module t_schmidt_poly_on_gauss
