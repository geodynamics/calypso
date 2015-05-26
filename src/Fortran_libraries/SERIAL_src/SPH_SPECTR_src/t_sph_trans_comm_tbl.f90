!>@file   t_sph_trans_comm_tbl.f90
!!@brief  module t_sph_trans_comm_tbl
!!
!!@author H. Matsui
!!@date Programmed in July, 2007 
!
!> @brief Structure for communication table for spherical transform
!!
!!@verbatim
!!      subroutine alloc_type_sph_comms_stack(comms_sph)
!!      subroutine alloc_type_sph_comms_item(sph_mesh, comms_sph)
!!      subroutine dealloc_type_sph_comms_item(comms_sph)
!!        type(sph_grids), intent(in) :: sph_mesh
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!
!!      subroutine alloc_type_sph_comm_stack(comm)
!!      subroutine alloc_type_sph_comm_item(numnod, comm)
!!      subroutine dealloc_type_sph_comm_item(comm)
!!      subroutine dealloc_type_sph_comm_item(comm)
!!        type(sph_comm_tbl), intent(inout) :: comm
!!
!!      subroutine set_reverse_sph_comm_tbl_t(numnod, comm)
!!        integer(kind = kint), intent(in) :: numnod
!!        type(sph_comm_tbl), intent(inout) :: comm
!!@endverbatim
!
      module t_sph_trans_comm_tbl
!
      use m_precision
!
      implicit none
!
!
!> Structure for communication table for each spherical grid
      type sph_comm_tbl
!>        number of domain to communicate from @f$ f(r,j) @f$ 
        integer(kind = kint) :: nneib_domain
!>        total number of data points to communicate
        integer(kind = kint) :: ntot_item_sr
!>        integer flag for transfering data within same process
        integer(kind = kint) :: iflag_self
!>        process IDs to communicate
        integer(kind = kint), pointer :: id_domain(:)
!>        end point for communication to each process
        integer(kind = kint), pointer :: istack_sr(:)
!>        local data id to communicate
        integer(kind = kint), pointer :: item_sr(:)
!>        communication table id for local point
        integer(kind = kint), pointer :: irev_sr(:)
      end type sph_comm_tbl
!
!>  Structure for communication table for spherical transform
      type sph_comm_tables
!>        Communication table for @f$ f(r,t,p) @f$ 
        type(sph_comm_tbl) :: comm_rtp
!>        Communication table for @f$ f(r,t,m) @f$ 
        type(sph_comm_tbl) :: comm_rtm
!>        Communication table for @f$ f(r,l,m) @f$ 
        type(sph_comm_tbl) :: comm_rlm
!>        Communication table for @f$ f(r,j) @f$ 
        type(sph_comm_tbl) :: comm_rj
      end type sph_comm_tables
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_type_sph_comms_stack(comms_sph)
!
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      call alloc_type_sph_comm_stack(comms_sph%comm_rtp)
      call alloc_type_sph_comm_stack(comms_sph%comm_rtm)
      call alloc_type_sph_comm_stack(comms_sph%comm_rlm)
      call alloc_type_sph_comm_stack(comms_sph%comm_rj)
!
      end subroutine alloc_type_sph_comms_stack
!
! -----------------------------------------------------------------------
!
      subroutine alloc_type_sph_comms_item(sph_mesh, comms_sph)
!
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: sph_mesh
      type(sph_comm_tables), intent(inout) :: comms_sph
!
!
      call alloc_type_sph_comm_item(sph_mesh%sph_rtp%nnod_rtp,          &
     &    comms_sph%comm_rtp)
      call alloc_type_sph_comm_item(sph_mesh%sph_rtm%nnod_rtm,          &
     &    comms_sph%comm_rtm)
      call alloc_type_sph_comm_item(sph_mesh%sph_rlm%nnod_rlm,          &
     &    comms_sph%comm_rlm)
      call alloc_type_sph_comm_item(sph_mesh%sph_rj%nnod_rj,            &
     &    comms_sph%comm_rj)
!
      end subroutine alloc_type_sph_comms_item
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_comms_item(comms_sph)
!
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      call dealloc_type_sph_comm_item(comms_sph%comm_rtp)
      call dealloc_type_sph_comm_item(comms_sph%comm_rtm)
      call dealloc_type_sph_comm_item(comms_sph%comm_rlm)
      call dealloc_type_sph_comm_item(comms_sph%comm_rj)
!
      end subroutine dealloc_type_sph_comms_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_type_sph_comm_stack(comm)
!
      type(sph_comm_tbl), intent(inout) :: comm
!
      allocate( comm%id_domain(comm%nneib_domain) )
      allocate( comm%istack_sr(0:comm%nneib_domain) )
      if(comm%nneib_domain .gt. 0) comm%id_domain =  0
      comm%istack_sr =  0
      comm%iflag_self = 0
!
      end subroutine alloc_type_sph_comm_stack
!
! -----------------------------------------------------------------------
!
      subroutine alloc_type_sph_comm_item(numnod, comm)
!
      integer(kind = kint), intent(in) :: numnod
      type(sph_comm_tbl), intent(inout) :: comm
!
!
      allocate( comm%item_sr(comm%ntot_item_sr) )
      allocate( comm%irev_sr(numnod) )
      if(comm%ntot_item_sr .gt. 0) comm%item_sr = 0
      if(numnod .gt. 0) comm%irev_sr = 0
!
      end subroutine alloc_type_sph_comm_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_comm_stack(comm)
!
      type(sph_comm_tbl), intent(inout) :: comm
!
      deallocate(comm%id_domain, comm%istack_sr)
!
      end subroutine dealloc_type_sph_comm_stack
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_comm_item(comm)
!
      type(sph_comm_tbl), intent(inout) :: comm
!
      deallocate(comm%item_sr, comm%irev_sr)
      call dealloc_type_sph_comm_stack(comm)
!
      end subroutine dealloc_type_sph_comm_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_reverse_sph_comm_tbl_t(numnod, comm)
!
      integer(kind = kint), intent(in) :: numnod
      type(sph_comm_tbl), intent(inout) :: comm
!
      integer(kind = kint) :: i, k
!
!
!$omp parallel do
      do i = 1, numnod
        comm%irev_sr(i) = comm%ntot_item_sr + 1
      end do
!$omp end parallel do
!
!$omp parallel do private(i)
      do k = 1, comm%ntot_item_sr
        i = comm%item_sr(k)
        comm%irev_sr(i) = k
      end do
!$omp end parallel do
!
      end subroutine set_reverse_sph_comm_tbl_t
!
! -----------------------------------------------------------------------
!
      end module t_sph_trans_comm_tbl
