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
!!
!!      subroutine set_import_table_ctl(import_ctl, trans_p)
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!      subroutine write_import_table_mode(trans_p)
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!@endverbatim
!!
      module t_work_4_sph_trans
!
      use m_precision
      use t_schmidt_poly_on_rtm
      use m_FFT_selector
      use select_copy_from_recv
!
      implicit none
!
!>      Character flag to use import table
      character(len = kchara), parameter                                &
     &                       :: hd_import_item = 'regular_table'
!>      Character flag to use reverse import table
      character(len = kchara), parameter                                &
     &                       :: hd_import_rev =  'reversed_table'
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
!>      vector length for legendre transform
        integer(kind = kint) :: nvector_legendre = 0
!>        Integer flag to select FFT
        integer(kind = kint) :: iflag_FFT = iflag_UNDEFINED_FFT
!>        Integer flag to select routines to get data from recieve buffer
        integer(kind = kint) :: iflag_SPH_recv = iflag_import_UNDEFINED
!
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
!-----------------------------------------------------------------------
!
      subroutine set_import_table_ctl(import_ctl, trans_p)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: import_ctl
      type(parameters_4_sph_trans), intent(inout) :: trans_p
!
!
      if(cmp_no_case(import_ctl, hd_import_item)) then
        trans_p%iflag_SPH_recv = iflag_import_item
      else if(cmp_no_case(import_ctl, hd_import_rev)) then
        trans_p%iflag_SPH_recv = iflag_import_rev
      else
        trans_p%iflag_SPH_recv = iflag_import_UNDEFINED
      end if
!
      end subroutine set_import_table_ctl
!
! ------------------------------------------------------------------
!
      subroutine write_import_table_mode(trans_p)
!
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
!
      write(*,'(a,i4)', advance='no')                                 &
     &   'Communication mode for sph. transform: ',                   &
     &    trans_p%iflag_SPH_recv
      if(trans_p%iflag_SPH_recv .eq. iflag_import_item) then
        write(*,'(3a)') ' (', trim(hd_import_item), ') '
      else if(trans_p%iflag_SPH_recv .eq. iflag_import_rev) then
        write(*,'(3a)') ' (', trim(hd_import_rev), ') '
      end if
!
      end subroutine write_import_table_mode
!
! ------------------------------------------------------------------
!
      end module t_work_4_sph_trans
