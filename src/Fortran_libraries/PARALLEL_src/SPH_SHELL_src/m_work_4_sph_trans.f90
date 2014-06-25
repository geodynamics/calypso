!>@file   m_work_4_sph_trans.f90
!!@brief  module m_work_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  global addresses for spherical transform
!!        communication test
!!
!!@verbatim
!!      subroutine resize_work_4_sph_trans
!!
!!      subroutine allocate_work_4_sph_trans
!!      subroutine deallocate_work_4_sph_trans
!!
!!      subroutine allocate_work_4_zonal_fft
!!      subroutine deallocate_work_4_zonal_fft
!!
!!      subroutine allocate_wk_nod_data_to_sph
!!      subroutine deallocate_wk_nod_data_to_sph
!!
!!      subroutine check_vr_rtp(my_rank, nb)
!!      subroutine check_vr_rtm(my_rank, nb)
!!      subroutine check_sp_rlm(my_rank, nb)
!!
!!   input /outpt data
!!
!!      radial component:      vr_rtp(3*i_rtp-2)
!!      elevetional component: vr_rtp(3*i_rtp-1)
!!      azimuthal component:   vr_rtp(2*i_rtp  )
!!
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!  transform for scalar
!!   input /outpt arrays
!!
!!      field: vr_rtp(i_rtp)
!!      spectr: sp_rj(i_rj)
!!@endverbatim
!!
      module m_work_4_sph_trans
!
      use m_precision
!
      implicit none
!
!>      total number of components for spherical harmonics transform
      integer(kind = kint) :: ncomp_sph_trans
!
!>      field data including pole and center  @f$ f(r,\theta,\phi) @f$ 
      real(kind = kreal), allocatable :: d_nod_rtp(:,:)
!
!>      Spectr data for spherical harmonics transform  @f$ f(r,j) @f$
      real(kind = kreal), allocatable :: sp_rj(:)
!>      field data on Gauss-Legendre points @f$ f(r,\theta,\phi) @f$ 
      real(kind = kreal), allocatable :: vr_rtp(:)
!
!>      Spectr data for Legendre transform  @f$ f(r,l,m) @f$ 
!>@n      Order: sp_rlm(i_comp,i_fld,j_rlm,k_rtm)
!>@n      size: sp_rlm(3*nb*nidx_rlm(2)*nidx_rtm(1))
      real(kind = kreal), allocatable :: sp_rlm(:)
!>      field data for Legendre transform  @f$ f(r,\theta,m) @f$ 
!!@n     Order: vr_rtm(i_comp,i_fld,l_rtm,k_rtm,m_rtm)
!!@n     size:  vr_rtm(3*nb*nidx_rtm(2)*nidx_rtm(1)*nidx_rtm(3))
      real(kind = kreal), allocatable :: vr_rtm(:)
!
!
!>      Spectr harmonics order for Legendre transform
      integer(kind = kint), allocatable :: mdx_p_rlm_rtm(:)
!>      Spectr harmonics order for Legendre transform
      integer(kind = kint), allocatable :: mdx_n_rlm_rtm(:)
!>      @f$ 1 / \sin \theta @f$  for Legendre transform
      real(kind = kreal), allocatable :: asin_theta_1d_rtm(:)
!
!>      @f$ \sin \theta @f$ in sapherical grid (one-dimentional)
      real(kind = kreal), allocatable :: sin_theta_1d_rtp(:)
!>      @f$ \cos \theta @f$ in sapherical grid (one-dimentional)
      real(kind = kreal), allocatable :: cos_theta_1d_rtp(:)
!>      @f$ \cot \theta @f$ in sapherical grid (one-dimentional)
      real(kind = kreal), allocatable :: cot_theta_1d_rtp(:)
!
!
!>      End address of spherical harmonics order for SMP parallelization
      integer(kind = kint), allocatable :: lstack_rlm(:)
!
!>      Data size for Legendre transform to check work area
      integer(kind = kint), private :: iflag_sph_trans = -1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_work_4_sph_trans
!
      if (ncomp_sph_trans .gt. iflag_sph_trans) then
        call deallocate_work_4_sph_trans
      end if
!
      if (iflag_sph_trans .le. 0)  call allocate_work_4_sph_trans
!
      end subroutine resize_work_4_sph_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_sph_trans
!
      use m_spheric_parameter
!
!
      allocate(lstack_rlm(0:nidx_rtm(3)))
!
      allocate(mdx_p_rlm_rtm(nidx_rlm(2)))
      allocate(mdx_n_rlm_rtm(nidx_rlm(2)))
      allocate(asin_theta_1d_rtm(nidx_rtm(2)))
!
      allocate(sp_rlm(ncomp_sph_trans*nnod_rlm))
      allocate(vr_rtp(ncomp_sph_trans*nnod_rtp))
!
      allocate(sp_rj(ncomp_sph_trans*nnod_rj))
!
      allocate(vr_rtm(ncomp_sph_trans*nnod_rtm))
!
      allocate(cos_theta_1d_rtp(nidx_rtp(2)))
      allocate(sin_theta_1d_rtp(nidx_rtp(2)))
      allocate(cot_theta_1d_rtp(nidx_rtp(2)))
!
      lstack_rlm = 0
      mdx_p_rlm_rtm = 0
      mdx_n_rlm_rtm = 0
      asin_theta_1d_rtm = 0.0d0
!
      cos_theta_1d_rtp = 0.0d0
      sin_theta_1d_rtp = 0.0d0
      cot_theta_1d_rtp = 0.0d0
!
      sp_rj =  0.0d0
      vr_rtp = 0.0d0
!
      sp_rlm = 0.0d0
      vr_rtm = 0.0d0
!
      iflag_sph_trans = ncomp_sph_trans
!
      end subroutine allocate_work_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_4_sph_trans
!
!
      deallocate(lstack_rlm)
      deallocate(mdx_p_rlm_rtm, mdx_n_rlm_rtm)
      deallocate(asin_theta_1d_rtm, cot_theta_1d_rtp)
      deallocate(sin_theta_1d_rtp, cos_theta_1d_rtp)
!
      deallocate(sp_rj, vr_rtp)
      deallocate(sp_rlm, vr_rtm)
!
      iflag_sph_trans = 0
!
      end subroutine deallocate_work_4_sph_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_zonal_fft
!
      use m_spheric_parameter
!
      allocate(vr_rtp(ncomp_sph_trans*nnod_rtp))
      vr_rtp = 0.0d0
!
      iflag_sph_trans = ncomp_sph_trans
!
      end subroutine allocate_work_4_zonal_fft
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_4_zonal_fft
!
      deallocate(vr_rtp)
      iflag_sph_trans = 0
!
      end subroutine deallocate_work_4_zonal_fft
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_wk_nod_data_to_sph
!
      use m_spheric_parameter
!
      allocate( d_nod_rtp(nnod_rtp_pole,6) )
      d_nod_rtp = 0.0d0
!
      end subroutine allocate_wk_nod_data_to_sph
!
! -------------------------------------------------------------------
!
      subroutine deallocate_wk_nod_data_to_sph
!
      deallocate( d_nod_rtp )
!
      end subroutine deallocate_wk_nod_data_to_sph
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine check_vr_rtp(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'vr_rtp', nb
      do inod = 1, nnod_rtp
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(4i10,1p200e20.12)') inod,                    &
     &        idx_global_rtp(inod,1:3), vr_rtp(ist:ied)
      end do
!
      end subroutine check_vr_rtp
!
! ----------------------------------------------------------------------
!
      subroutine check_vr_rtm(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'vr_rtm', nb
      do inod = 1, nnod_rtm
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(4i10,1p200e20.12)') inod,                    &
     &        idx_global_rtm(inod,1:3), vr_rtm(ist:ied)
      end do
!
      end subroutine check_vr_rtm
!
! ----------------------------------------------------------------------
!
      subroutine check_sp_rlm(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'sp_rlm', nb
      do inod = 1, nnod_rlm
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(3i10,1p200e20.12)') inod,                    &
     &        idx_global_rlm(inod,1:2), sp_rlm(ist:ied)
      end do
!
      end subroutine check_sp_rlm
!
! ----------------------------------------------------------------------
!
      subroutine check_sp_rj(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'sp_rj', nb
      do inod = 1, nnod_rj
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(3i10,1p200e20.12)') inod,                    &
     &        idx_global_rj(inod,1:2), sp_rj(ist:ied)
      end do
!
      end subroutine check_sp_rj
!
! ----------------------------------------------------------------------
!
      end module m_work_4_sph_trans
