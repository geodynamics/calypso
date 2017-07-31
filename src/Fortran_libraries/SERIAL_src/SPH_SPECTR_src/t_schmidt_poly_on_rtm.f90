!>@file   t_schmidt_poly_on_rtm.f90
!!@brief  module t_schmidt_poly_on_rtm
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Parameters for LEgendre transforms
!!
!!@verbatim
!!      subroutine alloc_gauss_colat_rtm(nth_rtm, Plm_WK)
!!      subroutine alloc_schmidt_normalize(jmax_rlm, jmax_rj, Plm_WK)
!!      subroutine alloc_schmidt_poly_rtm(nth_rtm, jmax_rlm, Plm_WK)
!!      subroutine alloc_trans_schmidt_rtm(nth_rtm, jmax_rlm, Plm_WK)
!!      subroutine alloc_schmidt_p_rtm_pole(jmax_rlm, Plm_WK)
!!        type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!!
!!      subroutine dealloc_gauss_colat_rtm(Plm_WK)
!!      subroutine dealloc_schmidt_poly_rtm(Plm_WK)
!!      subroutine dealloc_trans_schmidt_rtm(Plm_WK)
!!      subroutine dealloc_schmidt_p_rtm_pole(Plm_WK)
!!        type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!!
!!      subroutine check_gauss_colat_rtm(my_rank, nth_rtm, Plm_WK)
!!      subroutine check_schmidt_poly_rtm                               &
!!     &         (my_rank, sph_rtm, sph_rlm, Plm_WK)
!!      subroutine check_schmidt_p_rtm_pole(my_rank, sph_rlm, Plm_WK)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(legendre_4_sph_trans), intent(in) :: Plm_WK
!!@endverbatim
!
      module t_schmidt_poly_on_rtm
!
      use m_precision
!
      implicit none
!
!>      Structures for Legendre polynomials for spherical transform
      type legendre_4_sph_trans
        real(kind = kreal), allocatable :: g_point_rtm(:)
        real(kind = kreal), allocatable :: g_colat_rtm(:)
        real(kind = kreal), allocatable :: weight_rtm(:)
!
!>      @f$ 1 / \sin \theta @f$  for Legendre transform
        real(kind = kreal), allocatable :: asin_t_rtm(:)
!
!>        @$f P_{l}{m} @$f at gouss points
        real(kind = kreal), allocatable :: P_rtm(:,:)
!>        @$f dP_{l}{m}/d\theta @$f at gouss points
        real(kind = kreal), allocatable :: dPdt_rtm(:,:)
!
!>        @$f P_{l}{m} @$f at poles
        real(kind = kreal), allocatable :: P_pole_rtm(:,:)
!>        @$f dP_{l}{m}/d\theta @$f at poles
        real(kind = kreal), allocatable :: dPdt_pole_rtm(:,:)
!
!>        Normalization constants for spherical harmonics in (r,l,m)
        real(kind = kreal), allocatable:: g_sph_rlm(:,:)
!>        Normalization constants for spherical harmonics in (r,j)
        real(kind = kreal), allocatable:: g_sph_rj(:,:)
!
!
!>        @$f P_{l}{m} @$f with A(j,theta)
        real(kind = kreal), allocatable :: P_jl(:,:)
!>        @$f dP_{l}{m}/d\theta @$f with A(j,theta)
        real(kind = kreal), allocatable :: dPdt_jl(:,:)
      end type legendre_4_sph_trans
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_colat_rtm(nth_rtm, Plm_WK)
!
      integer(kind = kint), intent(in) :: nth_rtm
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
      allocate( Plm_WK%g_point_rtm(nth_rtm) )
      allocate( Plm_WK%g_colat_rtm(nth_rtm) )
      allocate( Plm_WK%weight_rtm(nth_rtm) )
!
      allocate( Plm_WK%asin_t_rtm(nth_rtm))
!
      Plm_WK%g_point_rtm = 0.0d0
      Plm_WK%g_colat_rtm = 0.0d0
      Plm_WK%weight_rtm = 0.0d0
!
      Plm_WK%asin_t_rtm =  0.0d0
!
      end subroutine alloc_gauss_colat_rtm
!
! -----------------------------------------------------------------------
!
      subroutine alloc_schmidt_normalize(jmax_rlm, jmax_rj, Plm_WK)
!
      integer(kind = kint), intent(in) :: jmax_rlm, jmax_rj
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
!
      allocate( Plm_WK%g_sph_rlm(jmax_rlm,17) )
      allocate( Plm_WK%g_sph_rj(jmax_rj,13) )
!
      Plm_WK%g_sph_rlm = 0.0d0
      Plm_WK%g_sph_rj =  0.0d0
!
      end subroutine alloc_schmidt_normalize
!
! -----------------------------------------------------------------------
!
      subroutine alloc_schmidt_poly_rtm(nth_rtm, jmax_rlm, Plm_WK)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
!
      allocate( Plm_WK%P_rtm(nth_rtm,jmax_rlm) )
      allocate( Plm_WK%dPdt_rtm(nth_rtm,jmax_rlm) )
!
      Plm_WK%P_rtm = 0.0d0
      Plm_WK%dPdt_rtm = 0.0d0
!
      end subroutine alloc_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      subroutine alloc_trans_schmidt_rtm(nth_rtm, jmax_rlm, Plm_WK)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
!
      allocate( Plm_WK%P_jl(jmax_rlm,nth_rtm) )
      allocate( Plm_WK%dPdt_jl(jmax_rlm,nth_rtm) )
!
      Plm_WK%P_jl =  0.0d0
      Plm_WK%dPdt_jl =  0.0d0
!
      end subroutine alloc_trans_schmidt_rtm
!
! -----------------------------------------------------------------------
!
      subroutine alloc_schmidt_p_rtm_pole(jmax_rlm, Plm_WK)
!
      integer(kind = kint), intent(in) :: jmax_rlm
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
!
      allocate( Plm_WK%P_pole_rtm(2,jmax_rlm) )
      allocate( Plm_WK%dPdt_pole_rtm(2,jmax_rlm) )
!
      Plm_WK%P_pole_rtm = 0.0d0
      Plm_WK%dPdt_pole_rtm = 0.0d0
!
      end subroutine alloc_schmidt_p_rtm_pole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_colat_rtm(Plm_WK)
!
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
      deallocate(Plm_WK%g_point_rtm)
      deallocate(Plm_WK%g_colat_rtm)
      deallocate(Plm_WK%weight_rtm)
      deallocate(Plm_WK%asin_t_rtm)
!
      end subroutine dealloc_gauss_colat_rtm
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_schmidt_poly_rtm(Plm_WK)
!
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
      deallocate( Plm_WK%P_rtm, Plm_WK%dPdt_rtm )
      deallocate( Plm_WK%g_sph_rlm, Plm_WK%g_sph_rj)
!
      end subroutine dealloc_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_trans_schmidt_rtm(Plm_WK)
!
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
      deallocate( Plm_WK%P_jl, Plm_WK%dPdt_jl)
!
      end subroutine dealloc_trans_schmidt_rtm
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_schmidt_p_rtm_pole(Plm_WK)
!
      type(legendre_4_sph_trans), intent(inout) :: Plm_WK
!
      deallocate( Plm_WK%P_pole_rtm, Plm_WK%dPdt_pole_rtm )
!
      end subroutine dealloc_schmidt_p_rtm_pole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_gauss_colat_rtm(my_rank, nth_rtm, Plm_WK)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nth_rtm
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
!
      integer(kind = kint) :: i
!
!
      write(50+my_rank,*) 'num_gauss_points', nth_rtm
!
      do i = 1, nth_rtm
        write(50+my_rank,'(i5,1p3E25.15e3)') i, Plm_WK%g_point_rtm(i),  &
     &        Plm_WK%g_colat_rtm(i), Plm_WK%weight_rtm(i)
      end do
!
      end subroutine check_gauss_colat_rtm
!
! -----------------------------------------------------------------------
!
      subroutine check_schmidt_poly_rtm                                 &
     &         (my_rank, sph_rtm, sph_rlm, Plm_WK)
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
!
      integer(kind = kint) :: i, j
!
!
      write(50+my_rank,*) 'num_gauss_points, truncation',               &
     &         sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2)
      write(50+my_rank,*) 'med_no, j, l, m, P, dp/dtheta'
!
      do j = 1, sph_rlm%nidx_rlm(2)
        do i = 1, sph_rtm%nidx_rtm(2)
          write(50+my_rank,'(5i5,1p3E25.15e3)') i, j,                   &
     &      sph_rlm%idx_gl_1d_rlm_j(j,1:3),                             &
     &      Plm_WK%P_rtm(i,j), Plm_WK%dPdt_rtm(i,j)
        end do
      end do
!
      end subroutine check_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      subroutine check_schmidt_p_rtm_pole(my_rank, sph_rlm, Plm_WK)
!
      use t_spheric_rlm_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: Plm_WK
!
      integer(kind = kint) :: i, j
!
!
      write(50+my_rank,*) 'truncation', sph_rlm%nidx_rlm(2)
      write(50+my_rank,*) 'med_no, j, l, m, P, dp/dtheta'
!
      do j = 1, sph_rlm%nidx_rlm(2)
        do i = 1, 2
          write(50+my_rank,'(5i5,1p3E25.15e3)') i, j,                   &
     &      sph_rlm%idx_gl_1d_rlm_j(j,1:3),                             &
     &      Plm_WK%P_pole_rtm(i,j), Plm_WK%dPdt_pole_rtm(i,j)
        end do
      end do
!
      end subroutine check_schmidt_p_rtm_pole
!
! -----------------------------------------------------------------------
!
      end module t_schmidt_poly_on_rtm
