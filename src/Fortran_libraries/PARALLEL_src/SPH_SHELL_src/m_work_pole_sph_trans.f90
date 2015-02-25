!
!      module m_work_pole_sph_trans
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine init_num_pole_sph_trans
!!      subroutine deallocate_num_pole_sph_trans
!!
!
      module m_work_pole_sph_trans
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!
      integer(kind = kint) :: nnod_pole
!
      integer(kind = kint), allocatable :: istack_npole_smp(:)
!
      integer(kind = kint) :: max_npole_smp
!
      private :: allocate_num_pole_sph_trans
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_num_pole_sph_trans
!
!
      allocate(istack_npole_smp(0:np_smp) )
      istack_npole_smp = 0
!
      end subroutine allocate_num_pole_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_num_pole_sph_trans
!
!
      deallocate(istack_npole_smp)
!
      end subroutine deallocate_num_pole_sph_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_num_pole_sph_trans
!
      use m_spheric_parameter
      use cal_minmax_and_stacks
!
!
      nnod_pole = 2*nidx_global_rtp(1) + 1
      call allocate_num_pole_sph_trans
!
      call count_number_4_smp(np_smp, ione, nnod_pole,                  &
     &    istack_npole_smp, max_npole_smp)
!
      end subroutine init_num_pole_sph_trans
!
! ----------------------------------------------------------------------
!
      end module m_work_pole_sph_trans
