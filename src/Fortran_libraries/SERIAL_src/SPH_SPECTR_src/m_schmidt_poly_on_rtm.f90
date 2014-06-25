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
!!      subroutine allocate_trans_schmidt_rtm
!!      subroutine allocate_legendre_trans_mat
!!      subroutine allocate_schmidt_p_rtm_pole
!!
!!      subroutine deallocate_gauss_colat_rtm
!!      subroutine deallocate_schmidt_poly_rtm
!!      subroutine deallocate_trans_schmidt_rtm
!!      subroutine deallocate_legendre_trans_mat
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
      real(kind = kreal), allocatable :: P_rtm(:,:)
      real(kind = kreal), allocatable :: dPdt_rtm(:,:)
!
      real(kind = kreal), allocatable:: g_sph_rlm(:,:)
      real(kind = kreal), allocatable:: g_sph_rj(:,:)
!
      real(kind = kreal), allocatable :: P_pole_rtm(:,:)
      real(kind = kreal), allocatable :: dPdt_pole_rtm(:,:)
!
!>        @$f Wt_{\theta} Nv_{l} P_{l}{m} @$f with A(theta,j)
      real(kind = kreal), allocatable :: Pvw_lj(:,:)
!>        @$f Wt_{\theta} Nv_{l} dP_{l}{m}/d\theta @$f with A(theta,j)
      real(kind = kreal), allocatable :: dPvw_lj(:,:)
!>        @$f Wt_{\theta} Nv_{l} m P_{l}{m} / \sin \theta @$f
!!           with A(theta,j)
      real(kind = kreal), allocatable :: Pgvw_lj(:,:)
!
!>        @$f Wt_{\theta} Ns_{l} P_{l}{m} @$f with A(theta,j)
      real(kind = kreal), allocatable :: Pws_lj(:,:)
!
!>        @$f l(l+1) P_{l}{m} @$f with A(theta,j)
      real(kind = kreal), allocatable :: Pg3_lj(:,:)
!>        @$f -m P_{l}{m} / \sin \theta @$f with A(theta,j)
      real(kind = kreal), allocatable :: Pgv_lj(:,:)
!
!
!>        @$f Wt_{\theta} Nv_{l} P_{l}{m} @$f with A(j,theta)
      real(kind = kreal), allocatable :: Pvw_jl(:,:)
!>        @$f Wt_{\theta} Nv_{l} dP_{l}{m}/d\theta @$f with A(j,theta)
      real(kind = kreal), allocatable :: dPvw_jl(:,:)
!>        @$f Wt_{\theta} Nv_{l} m P_{l}{m} / \sin \theta @$f
!!            with A(j,theta)
      real(kind = kreal), allocatable :: Pgvw_jl(:,:)
!
!>        @$f Wt_{\theta} Ns_{l} P_{l}{m} @$f  with A(j,theta)
      real(kind = kreal), allocatable :: Pws_jl(:,:)
!
!>        @$f l(l+1) P_{l}{m} @$f  with A(j,theta)
      real(kind = kreal), allocatable :: Pg3_jl(:,:)
!>        @$f -m P_{l}{m} / \sin \theta @$f with A(j,theta)
      real(kind = kreal), allocatable :: Pgv_jl(:,:)
!
!>        @$f P_{l}{m} @$f with A(j,theta)
      real(kind = kreal), allocatable :: P_jl(:,:)
!>        @$f dP_{l}{m}/d\theta @$f with A(j,theta)
      real(kind = kreal), allocatable :: dPdt_jl(:,:)
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
      subroutine allocate_legendre_trans_mat
!
      use m_spheric_parameter
!
      allocate( Pvw_lj(nidx_rtm(2),nidx_rlm(2)) )
      allocate( dPvw_lj(nidx_rtm(2),nidx_rlm(2)) )
      allocate( Pgvw_lj(nidx_rtm(2),nidx_rlm(2)) )
      allocate( Pws_lj(nidx_rtm(2),nidx_rlm(2)) )
      allocate( Pg3_lj(nidx_rtm(2),nidx_rlm(2)) )
      allocate( Pgv_lj(nidx_rtm(2),nidx_rlm(2)) )
!
      allocate( Pvw_jl(nidx_rlm(2),nidx_rtm(2)) )
      allocate( dPvw_jl(nidx_rlm(2),nidx_rtm(2)) )
      allocate( Pgvw_jl(nidx_rlm(2),nidx_rtm(2)) )
      allocate( Pws_jl(nidx_rlm(2),nidx_rtm(2)) )
      allocate( Pg3_jl(nidx_rlm(2),nidx_rtm(2)) )
      allocate( Pgv_jl(nidx_rlm(2),nidx_rtm(2)) )
!
      Pvw_lj =  0.0d0
      dPvw_lj = 0.0d0
      Pgvw_lj = 0.0d0
      Pws_lj =  0.0d0
      Pg3_lj =  0.0d0
      Pgv_lj =  0.0d0
!
      Pvw_jl =  0.0d0
      dPvw_jl = 0.0d0
      Pgvw_jl = 0.0d0
      Pws_jl =  0.0d0
      Pg3_jl =  0.0d0
      Pgv_jl =  0.0d0
!
      end subroutine allocate_legendre_trans_mat
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
      subroutine deallocate_trans_schmidt_rtm
!
      deallocate( P_jl, dPdt_jl)
!
      end subroutine deallocate_trans_schmidt_rtm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_legendre_trans_mat
!
!
      deallocate( Pvw_lj, dPvw_lj, Pgvw_lj)
      deallocate( Pws_lj, Pg3_lj,  Pgv_lj)
      deallocate( Pvw_jl, dPvw_jl, Pgvw_jl)
      deallocate( Pws_jl, Pg3_jl,  Pgv_jl)
!
      end subroutine deallocate_legendre_trans_mat
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
