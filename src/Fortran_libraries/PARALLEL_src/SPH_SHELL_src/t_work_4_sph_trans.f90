!>@file   t_work_4_sph_trans.f90
!!@brief  module t_work_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  global addresses for spherical transform
!!        communication test
!!
!!@verbatim
!!      subroutine alloc_work_4_sph_trans(nidx_rtm, nidx_rlm)
!!      subroutine alloc_l_rtm_block
!!
!!      subroutine dealloc_work_4_sph_trans
!!      subroutine dealloc_l_rtm_block
!!@endverbatim
!!
      module t_work_4_sph_trans
!
      use m_precision
      use t_schmidt_poly_on_rtm
!
      implicit none
!
!>      Structure of indices for spherical transforms
      type index_4_sph_trans
!>       total number of components for spherical harmonics transform
        integer(kind = kint) :: ncomp_sph_trans
!>       total number of vectors for spherical harmonics transform
        integer(kind = kint) :: nvector_sph_trans
!>       total number of svalars for spherical harmonics transform
        integer(kind = kint) :: nscalar_sph_trans
!
!>       Spectr harmonics order for Legendre transform
        integer(kind = kint), allocatable :: mdx_p_rlm_rtm(:)
!>       Spectr harmonics order for Legendre transform
        integer(kind = kint), allocatable :: mdx_n_rlm_rtm(:)
!
!>       Number of block for grid in @f$ \theta @f$-direction
        integer(kind = kint) :: nblock_l_rtm = 1
!>       End point of each block for grid in @f$ \theta @f$-direction
        integer(kind = kint), allocatable :: lstack_block_rtm(:)
!
!>       Number of block for grid in hermonics degree
        integer(kind = kint) :: nblock_j_rlm = 1
!
!>       End address of spherical harmonics order for SMP parallelization
        integer(kind = kint), allocatable :: lstack_rlm(:)
!>       Maximum point of each block for grid in  hermonics degree
        integer(kind = kint) :: maxdegree_rlm
!>       End address of spherical harmonics order for SMP parallelization
        integer(kind = kint), allocatable :: lstack_even_rlm(:)
      end type index_4_sph_trans
!
!>        Structures of parameters for spherical transform
      type parameters_4_sph_trans
!>        Structures of Legendre polynomials for spherical transform
        type(legendre_4_sph_trans) :: leg
!>        Structure of indices for spherical transforms
        type(index_4_sph_trans) :: idx_trns
      end type parameters_4_sph_trans
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_4_sph_trans(nidx_rtm, nidx_rlm, idx_trns)
!
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      type(index_4_sph_trans), intent(inout) :: idx_trns
!
!
      allocate(idx_trns%lstack_rlm(0:nidx_rtm(3)))
      allocate(idx_trns%lstack_even_rlm(0:nidx_rtm(3)))
!
      allocate(idx_trns%mdx_p_rlm_rtm(nidx_rlm(2)))
      allocate(idx_trns%mdx_n_rlm_rtm(nidx_rlm(2)))
!
      if(nidx_rtm(3) .gt. 0) then
        idx_trns%lstack_rlm = 0
        idx_trns%lstack_even_rlm = 0
      end if
      if(nidx_rlm(2) .gt. 0) then
        idx_trns%mdx_p_rlm_rtm = 0
        idx_trns%mdx_n_rlm_rtm = 0
      end if
      idx_trns%maxdegree_rlm = 0
!
      end subroutine alloc_work_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine alloc_l_rtm_block(idx_trns)
!
      type(index_4_sph_trans), intent(inout) :: idx_trns
!
!
      allocate(idx_trns%lstack_block_rtm(0:idx_trns%nblock_l_rtm))
      idx_trns%lstack_block_rtm = 0
!
      end subroutine alloc_l_rtm_block
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_4_sph_trans(idx_trns)
!
      type(index_4_sph_trans), intent(inout) :: idx_trns
!
!
      deallocate(idx_trns%lstack_rlm, idx_trns%lstack_even_rlm)
      deallocate(idx_trns%mdx_p_rlm_rtm, idx_trns%mdx_n_rlm_rtm)
!
      idx_trns%maxdegree_rlm =   0
!
      end subroutine dealloc_work_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_l_rtm_block(idx_trns)
!
      type(index_4_sph_trans), intent(inout) :: idx_trns
!
!
      deallocate(idx_trns%lstack_block_rtm)
!
      end subroutine dealloc_l_rtm_block
!
! ----------------------------------------------------------------------
!
      end module t_work_4_sph_trans
