!>@file   m_schmidt_poly_on_rtm.f90
!!@brief  module m_schmidt_poly_on_rtm
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Parameters for LEgendre transforms
!!
!!@verbatim
!!      subroutine allocate_gauss_colat_rtm
!!      subroutine allocate_schmidt_poly_rtm
!!      subroutine allocate_hemi_schmidt_rtm
!!      subroutine allocate_trans_schmidt_rtm
!!      subroutine allocate_schmidt_p_rtm_pole
!!
!!      subroutine deallocate_gauss_colat_rtm
!!      subroutine deallocate_schmidt_poly_rtm
!!      subroutine deallocate_hemi_schmidt_rtm
!!      subroutine deallocate_trans_schmidt_rtm
!!      subroutine deallocate_schmidt_p_rtm_pole
!!
!!      subroutine check_gauss_colat_rtm(my_rank)
!!      subroutine check_schmidt_poly_rtm(my_rank)
!!      subroutine check_schmidt_p_rtm_pole(my_rank)
!!@endverbatim
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
!>        @$f P_{l}{m} @$f at gouss points
      real(kind = kreal), allocatable :: P_rtm(:,:)
!>        @$f dP_{l}{m}/d\theta @$f at gouss points
      real(kind = kreal), allocatable :: dPdt_rtm(:,:)
!
!>        Number of meridional grid points in northern hemisphere
      integer(kind = kint) :: nth_hemi_rtm
!>        @$f P_{l}{m} @$f
!!        at gouss points in northen hemisphere
      real(kind = kreal), allocatable :: Ps_rtm(:,:)
!>        @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!        at gouss points in northen hemisphere
      real(kind = kreal), allocatable :: dPsdt_rtm(:,:)
!
!>        @$f P_{l}{m} @$f
!!        at gouss points in northen hemisphere
      real(kind = kreal), allocatable :: Ps_jl(:,:)
!>        @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!        at gouss points in northen hemisphere
      real(kind = kreal), allocatable :: dPsdt_jl(:,:)
!
!
!>        Normalization constants for spherical harmonics in (r,l,m)
      real(kind = kreal), allocatable:: g_sph_rlm(:,:)
!>        Normalization constants for spherical harmonics in (r,j)
      real(kind = kreal), allocatable:: g_sph_rj(:,:)
!
!>        @$f P_{l}{m} @$f at poles
      real(kind = kreal), allocatable :: P_pole_rtm(:,:)
!>        @$f dP_{l}{m}/d\theta @$f at poles
      real(kind = kreal), allocatable :: dPdt_pole_rtm(:,:)
!
!
!>        @$f P_{l}{m} @$f with A(j,theta)
      real(kind = kreal), allocatable :: P_jl(:,:)
!>        @$f dP_{l}{m}/d\theta @$f with A(j,theta)
      real(kind = kreal), allocatable :: dPdt_jl(:,:)
!
!
!>        @$f P_{l}{m} @$f at gouss points
!!        in northen hemisphere decomposited for SMP
      real(kind = kreal), allocatable :: Ps_rtm_smp(:,:,:)
!>        @$f dP_{l}{m}/d\theta @$f at gouss points 
!!        in northen hemisphere decomposited for SMP
      real(kind = kreal), allocatable :: dPsdt_rtm_smp(:,:,:)
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
      allocate( g_sph_rlm(nidx_rlm(2),17) )
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
      subroutine allocate_hemi_schmidt_rtm
!
      use m_spheric_parameter
!
!
      nth_hemi_rtm = (nidx_rtm(2)+1) / 2
      allocate( Ps_rtm(nth_hemi_rtm,nidx_rlm(2)) )
      allocate( dPsdt_rtm(nth_hemi_rtm,nidx_rlm(2)) )
!
      allocate( Ps_jl(nidx_rlm(2),nth_hemi_rtm) )
      allocate( dPsdt_jl(nidx_rlm(2),nth_hemi_rtm) )
!
      Ps_rtm =    0.0d0
      dPsdt_rtm = 0.0d0
!
      Ps_jl =    0.0d0
      dPsdt_jl = 0.0d0
!
      end subroutine allocate_hemi_schmidt_rtm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_trans_schmidt_rtm
!
      use m_spheric_parameter
!
      allocate( P_jl(nidx_rlm(2),nidx_rtm(2)) )
      allocate( dPdt_jl(nidx_rlm(2),nidx_rtm(2)) )
!
      P_jl =  0.0d0
      dPdt_jl =  0.0d0
!
      end subroutine allocate_trans_schmidt_rtm
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
      deallocate( P_rtm, dPdt_rtm)
      deallocate( g_sph_rlm, g_sph_rj)
!
      end subroutine deallocate_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_hemi_schmidt_rtm
!
      deallocate(Ps_rtm, dPsdt_rtm)
      deallocate(Ps_jl,  dPsdt_jl)
!
      end subroutine deallocate_hemi_schmidt_rtm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_trans_schmidt_rtm
!
      deallocate( P_jl, dPdt_jl)
!
      end subroutine deallocate_trans_schmidt_rtm
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
      write(50+my_rank,*) 'med_no, j, l, m, P, dp/dtheta'
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
      write(50+my_rank,*) 'med_no, j, l, m, P, dp/dtheta'
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
