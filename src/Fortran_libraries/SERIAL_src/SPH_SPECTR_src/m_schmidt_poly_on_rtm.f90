!
!      module m_schmidt_poly_on_rtm
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine allocate_gauss_colat_rtm
!      subroutine allocate_schmidt_poly_rtm
!      subroutine allocate_schmidt_p_rtm_pole
!
!      subroutine deallocate_gauss_colat_rtm
!      subroutine deallocate_schmidt_poly_rtm
!      subroutine deallocate_schmidt_p_rtm_pole
!
!      subroutine check_gauss_colat_rtm(my_rank)
!      subroutine check_schmidt_poly_rtm(my_rank)
!      subroutine check_schmidt_p_rtm_pole(my_rank)
!
      module m_schmidt_poly_on_rtm
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: g_point_rtm(:)
      real(kind = kreal), allocatable :: g_colat_rtm(:)
      real(kind = kreal), allocatable :: weight_rtm(:)
!
      real(kind = kreal), allocatable :: P_rtm(:,:)
      real(kind = kreal), allocatable :: dPdt_rtm(:,:)
!
      real(kind = kreal), allocatable:: g_sph_rlm(:,:)
      real(kind = kreal), allocatable:: g_sph_rj(:,:)
!
      real(kind = kreal), allocatable :: P_pole_rtm(:,:)
      real(kind = kreal), allocatable :: dPdt_pole_rtm(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_gauss_colat_rtm
!
      use m_spheric_parameter
!
      allocate( g_point_rtm(nidx_rtm(2)) )
      allocate( g_colat_rtm(nidx_rtm(2)) )
      allocate( weight_rtm(nidx_rtm(2)) )
!
      g_point_rtm = 0.0d0
      g_colat_rtm = 0.0d0
      weight_rtm = 0.0d0
!
      end subroutine allocate_gauss_colat_rtm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_schmidt_poly_rtm
!
      use m_spheric_parameter
!
      allocate( P_rtm(nidx_rtm(2),nidx_rlm(2)) )
      allocate( dPdt_rtm(nidx_rtm(2),nidx_rlm(2)) )
!
      allocate( g_sph_rlm(nidx_rlm(2),13) )
      allocate( g_sph_rj(nidx_rj(2),13) )
!
      P_rtm = 0.0d0
      dPdt_rtm = 0.0d0
!
      g_sph_rlm = 0.0d0
      g_sph_rj =  0.0d0
!
      end subroutine allocate_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_schmidt_p_rtm_pole
!
      use m_spheric_parameter
!
      allocate( P_pole_rtm(2,nidx_rlm(2)) )
      allocate( dPdt_pole_rtm(2,nidx_rlm(2)) )
!
      P_pole_rtm = 0.0d0
      dPdt_pole_rtm = 0.0d0
!
      end subroutine allocate_schmidt_p_rtm_pole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_gauss_colat_rtm
!
      deallocate( g_point_rtm )
      deallocate( g_colat_rtm )
      deallocate( weight_rtm )
!
      end subroutine deallocate_gauss_colat_rtm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_schmidt_poly_rtm
!
      deallocate( P_rtm )
      deallocate( dPdt_rtm )
!
      deallocate( g_sph_rlm )
      deallocate( g_sph_rj  )
!
      end subroutine deallocate_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_schmidt_p_rtm_pole
!
      deallocate( P_rtm, dPdt_rtm )
!
      end subroutine deallocate_schmidt_p_rtm_pole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_gauss_colat_rtm(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(50+my_rank,*) 'num_gauss_points', nidx_rtm(2)
!
      do i = 1, nidx_rtm(2)
        write(50+my_rank,'(i5,1p3E25.15e3)')                            &
     &        i, g_point_rtm(i), g_colat_rtm(i), weight_rtm(i)
      end do
!
      end subroutine check_gauss_colat_rtm
!
! -----------------------------------------------------------------------
!
      subroutine check_schmidt_poly_rtm(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, j
!
!
      write(50+my_rank,*) 'num_gauss_points, truncation',               &
     &         nidx_rtm(2), nidx_rlm(2)
      write(50+my_rank,*) 'med_no, j, l, m, P, dp/dth'
!
      do j = 1, nidx_rlm(2)
        do i = 1, nidx_rtm(2)
          write(50+my_rank,'(5i5,1p3E25.15e3)') i, j,                   &
     &      idx_gl_1d_rlm_j(j,1:3), P_rtm(i,j), dPdt_rtm(i,j)
        end do
      end do
!
      end subroutine check_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      subroutine check_schmidt_p_rtm_pole(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, j
!
!
      write(50+my_rank,*) 'truncation', nidx_rlm(2)
      write(50+my_rank,*) 'med_no, j, l, m, P, dp/dth'
!
      do j = 1, nidx_rlm(2)
        do i = 1, 2
          write(50+my_rank,'(5i5,1p3E25.15e3)') i, j,                   &
     &      idx_gl_1d_rlm_j(j,1:3), P_pole_rtm(i,j), dPdt_pole_rtm(i,j)
        end do
      end do
!
      end subroutine check_schmidt_p_rtm_pole
!
! -----------------------------------------------------------------------
!
      end module m_schmidt_poly_on_rtm
