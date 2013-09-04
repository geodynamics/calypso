!
!      module m_schmidt_poly_on_gauss
!
!     Written by H. Matsui on June, 2007
!
!      subroutine allocate_stack_smdt_med_smp(np_smp)
!      subroutine deallocate_stack_smdt_med_smp
!
!      subroutine allocate_gauss_colat_med
!      subroutine allocate_schmidt_poly_med
!
!      subroutine deallocate_gauss_colat_med
!      subroutine deallocate_schmidt_poly_med
!
!      subroutine check_gauss_colat_med
!      subroutine check_schmidt_poly_med(idx)
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
      real(kind = kreal), allocatable :: g_point_med(:)
      real(kind = kreal), allocatable :: g_colat_med(:)
      real(kind = kreal), allocatable :: weight_med(:)
!
      real(kind = kreal), allocatable :: P_smdt(:,:)
      real(kind = kreal), allocatable :: dPdt_smdt(:,:)
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
      P_smdt = 0.0d0
      dPdt_smdt = 0.0d0
!
      end subroutine allocate_schmidt_poly_med
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
      deallocate( P_smdt )
      deallocate( dPdt_smdt )
!
      end subroutine deallocate_schmidt_poly_med
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
      end module m_schmidt_poly_on_gauss
