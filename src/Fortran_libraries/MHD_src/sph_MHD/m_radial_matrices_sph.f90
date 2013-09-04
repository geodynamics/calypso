!>@file   m_radial_matrices_sph.f90
!!@brief  module m_radial_matrices_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief Radial band matrix for time evolutions
!!
!!@verbatim
!!      subroutine allocate_temp_mat_sph
!!      subroutine allocate_velo_mat_sph
!!      subroutine allocate_magne_mat_sph
!!      subroutine allocate_composit_mat_sph
!!
!!      subroutine deallocate_temp_mat_sph
!!      subroutine deallocate_velo_mat_sph
!!      subroutine deallocate_magne_mat_sph
!!      subroutine deallocate_composit_mat_sph
!!
!!      subroutine check_vorticity_matrices_sph(my_rank)
!!      subroutine check_magne_matrices_sph(my_rank)
!!      subroutine check_temp_matrices_sph(my_rank)
!!      subroutine check_composit_matrix_sph(my_rank)
!!@endverbatim
!
      module m_radial_matrices_sph
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: vp_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: vp_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: vp_evo_det(:,:)
      integer(kind = kint), allocatable :: i_vp_pivot(:,:)
!
      real(kind = kreal), allocatable :: vt_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: vt_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: vt_evo_det(:,:)
      integer(kind = kint), allocatable :: i_vt_pivot(:,:)
!
      real(kind = kreal), allocatable :: wt_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: wt_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: wt_evo_det(:,:)
      integer(kind = kint), allocatable :: i_wt_pivot(:,:)
!
!
      real(kind = kreal), allocatable :: p_poisson_mat(:,:,:)
      real(kind = kreal), allocatable :: p_poisson_lu(:,:,:)
      real(kind = kreal), allocatable :: p_poisson_det(:,:)
      integer(kind = kint), allocatable :: i_p_pivot(:,:)
!
      real(kind = kreal), allocatable :: vs_poisson_mat(:,:,:)
      real(kind = kreal), allocatable :: vs_poisson_lu(:,:,:)
      real(kind = kreal), allocatable :: vs_poisson_det(:,:)
      integer(kind = kint), allocatable :: i_vs_pivot(:,:)
!
!
      real(kind = kreal), allocatable :: bs_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: bs_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: bs_evo_det(:,:)
      integer(kind = kint), allocatable :: i_bs_pivot(:,:)
!
      real(kind = kreal), allocatable :: bt_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: bt_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: bt_evo_det(:,:)
      integer(kind = kint), allocatable :: i_bt_pivot(:,:)
!
!
      real(kind = kreal), allocatable :: temp_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: temp_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: temp_evo_det(:,:)
      integer(kind = kint), allocatable :: i_temp_pivot(:,:)
!
!
      real(kind = kreal), allocatable :: composit_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: composit_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: composit_evo_det(:,:)
      integer(kind = kint), allocatable :: i_composit_pivot(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_temp_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
      nri =  nidx_rj(1)
      jmax = nidx_rj(2)
!
      allocate( temp_evo_mat(3,nri,jmax) )
      allocate( temp_evo_lu(5,nri,jmax) )
      allocate( temp_evo_det(nri,jmax) )
      allocate( i_temp_pivot(nri,jmax) )
!
      temp_evo_mat =  0.0d0
      temp_evo_lu =   0.0d0
      temp_evo_det =   0.0d0
      i_temp_pivot =   0
      temp_evo_mat(2,1:nri,1:jmax) = 1.0d0
!
      end subroutine allocate_temp_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine allocate_velo_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
      nri = nidx_rj(1)
      jmax = nidx_rj(2)
!
!
      allocate( vs_poisson_mat(3,nri,jmax) )
      allocate( vs_poisson_lu(5,nri,jmax) )
      allocate( vs_poisson_det(nri,jmax) )
      allocate( i_vs_pivot(nri,jmax) )
!
      allocate( vp_evo_mat(5,nri,jmax) )
      allocate( vp_evo_lu(9,nri,jmax) )
      allocate( vp_evo_det(nri,jmax) )
      allocate( i_vp_pivot(nri,jmax) )
!
      allocate( vt_evo_mat(3,nri,jmax) )
      allocate( vt_evo_lu(5,nri,jmax) )
      allocate( vt_evo_det(nri,jmax) )
      allocate( i_vt_pivot(nri,jmax) )
!
      allocate( wt_evo_mat(3,nri,jmax) )
      allocate( wt_evo_lu(5,nri,jmax) )
      allocate( wt_evo_det(nri,jmax) )
      allocate( i_wt_pivot(nri,jmax) )
!
      allocate( p_poisson_mat(3,nri,jmax) )
      allocate( p_poisson_lu(5,nri,jmax) )
      allocate( p_poisson_det(nri,jmax) )
      allocate( i_p_pivot(nri,jmax) )
!
      vp_evo_mat =   0.0d0
      vp_evo_lu =    0.0d0
      vp_evo_det =   0.0d0
      i_vp_pivot =   0
!
      vt_evo_mat =   0.0d0
      vt_evo_lu =    0.0d0
      vt_evo_det =   0.0d0
      i_vt_pivot =   0
!
      wt_evo_mat =   0.0d0
      wt_evo_lu =    0.0d0
      wt_evo_det =   0.0d0
      i_wt_pivot =   0
!
      vs_poisson_mat =   0.0d0
      vs_poisson_lu =    0.0d0
      vs_poisson_det =   0.0d0
      i_vs_pivot =   0
!
      p_poisson_mat = 0.0d0
      p_poisson_lu =  0.0d0
      p_poisson_det = 0.0d0
      i_p_pivot =     0
!
      vp_evo_mat(3,1:nri,1:jmax) = 1.0d0
      vt_evo_mat(2,1:nri,1:jmax) = 1.0d0
      wt_evo_mat(2,1:nri,1:jmax) = 1.0d0
!
      if(nlayer_ICB .gt. 1) then
        p_poisson_mat(2,1:nlayer_ICB-1,1:jmax) =  1.0d0
        vs_poisson_mat(2,1:nlayer_ICB-1,1:jmax) = 1.0d0
      end if
!
      if(nlayer_CMB .lt. nri) then
        p_poisson_mat(2,nlayer_CMB+1:nri,1:jmax) =  1.0d0
        vs_poisson_mat(2,nlayer_CMB+1:nri,1:jmax) = 1.0d0
      end if
!
      end subroutine allocate_velo_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine allocate_magne_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
!
      nri = nidx_rj(1)
      jmax = nidx_rj(2)
!
      allocate( bs_evo_mat(3,nri,jmax) )
      allocate( bs_evo_lu(5,nri,jmax) )
      allocate( bs_evo_det(nri,jmax) )
      allocate( i_bs_pivot(nri,jmax) )
!
      allocate( bt_evo_mat(3,nri,jmax) )
      allocate( bt_evo_lu(5,nri,jmax) )
      allocate( bt_evo_det(nri,jmax) )
      allocate( i_bt_pivot(nri,jmax) )
!
      bs_evo_mat =  0.0d0
      bs_evo_lu =   0.0d0
      bs_evo_det =  0.0d0
      i_bs_pivot =  0
!
      bt_evo_mat =  0.0d0
      bt_evo_lu =   0.0d0
      bt_evo_det =  0.0d0
      i_bt_pivot =  0
!
      bs_evo_mat(2,1:nri,1:jmax) = 1.0d0
      bt_evo_mat(2,1:nri,1:jmax) = 1.0d0
!
      end subroutine allocate_magne_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine allocate_composit_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
      nri =  nidx_rj(1)
      jmax = nidx_rj(2)
!
      allocate( composit_evo_mat(3,nri,jmax) )
      allocate( composit_evo_lu(5,nri,jmax) )
      allocate( composit_evo_det(nri,jmax) )
      allocate( i_composit_pivot(nri,jmax) )
!
      composit_evo_mat =  0.0d0
      composit_evo_lu =   0.0d0
      composit_evo_det =  0.0d0
      i_composit_pivot =      0
      composit_evo_mat(2,1:nri,1:jmax) = 1.0d0
!
      end subroutine allocate_composit_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_temp_mat_sph
!
!
      deallocate( temp_evo_mat, temp_evo_lu )
      deallocate( temp_evo_det, i_temp_pivot)
!
      end subroutine deallocate_temp_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_velo_mat_sph
!
!
      deallocate( vs_poisson_mat, vs_poisson_lu )
      deallocate( vs_poisson_det, i_vs_pivot )
!
      deallocate( vp_evo_mat, vp_evo_lu )
      deallocate( vp_evo_det, i_vp_pivot )
!
      deallocate( vt_evo_mat, vt_evo_lu )
      deallocate( vt_evo_det, i_vt_pivot )
!
      deallocate( wt_evo_mat, wt_evo_lu )
      deallocate( wt_evo_det, i_wt_pivot )
!
      deallocate( p_poisson_mat, p_poisson_lu )
      deallocate( p_poisson_det, i_p_pivot )
!
      end subroutine deallocate_velo_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_magne_mat_sph
!
!
      deallocate( bs_evo_mat, bs_evo_lu )
      deallocate( bs_evo_det, i_bs_pivot )
!
      deallocate( bt_evo_mat, bt_evo_lu )
      deallocate( bt_evo_det, i_bt_pivot )
!
      end subroutine deallocate_magne_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_composit_mat_sph
!
!
      deallocate( composit_evo_mat, composit_evo_lu )
      deallocate( composit_evo_det, i_composit_pivot )
!
      end subroutine deallocate_composit_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_vorticity_matrices_sph(my_rank)
!
      use m_spheric_parameter
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'evalution matrix for toroidal velocity'
      call check_radial_3band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vt_evo_mat)
!
      write(50+my_rank,'(a)') 'evalution matrix for toroidal vorticity'
      call check_radial_3band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, wt_evo_mat)
!
      write(50+my_rank,'(a)') 'poisson matrix for pressure'
      call check_radial_3band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, p_poisson_mat)
!
      write(50+my_rank,'(a)') 'evalution matrix for poloidal velocity'
      call check_radial_5band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vp_evo_mat)
!
      end subroutine check_vorticity_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_magne_matrices_sph(my_rank)
!
      use m_spheric_parameter
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'evalution matrix for poloidal magne'
      call check_radial_3band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, bs_evo_mat)
!
      write(50+my_rank,'(a)') 'evalution matrix for toroidal magne'
      call check_radial_3band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, bt_evo_mat)
!
      end subroutine check_magne_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_temp_matrices_sph(my_rank)
!
      use m_spheric_parameter
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'evalution matrix for temperature'
      call check_radial_3band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, temp_evo_mat)
!
      end subroutine check_temp_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_composit_matrix_sph(my_rank)
!
      use m_spheric_parameter
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'evalution matrix for dummy scalar'
      call check_radial_3band_mat(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, composit_evo_mat)
!
      end subroutine check_composit_matrix_sph
!
! -----------------------------------------------------------------------
!
      end module m_radial_matrices_sph
