!copy_sph_comm_tbl_type_4_IO.f90
!      module copy_sph_comm_tbl_type_4_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine copy_comm_sph_type_from_IO(my_rank, numnod, comm)
!      subroutine copy_comm_sph_type_to_IO(my_rank, comm)
!        integer(kind = kint), intent(in) :: my_rank
!        type(sph_comm_tbl), intent(inout) :: comm
!
      module copy_sph_comm_tbl_type_4_IO
!
      use m_precision
!
      use m_constants
      use t_sph_trans_comm_tbl
      use m_comm_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_sph_type_from_IO(my_rank, numnod, comm)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: numnod
      type(sph_comm_tbl), intent(inout) :: comm
!
!
      comm%nneib_domain = num_neib_domain_IO
      comm%ntot_item_sr = ntot_import_IO
!
      call alloc_type_sph_comm_stack(comm)
      call alloc_type_sph_comm_item(numnod, comm)
!
      comm%id_domain(1:comm%nneib_domain)                               &
     &      = id_neib_domain_IO(1:comm%nneib_domain)
      comm%istack_sr(0:comm%nneib_domain)                               &
     &      = istack_import_IO(0:comm%nneib_domain)
!
      comm%item_sr(1:comm%ntot_item_sr)                                 &
     &      = item_import_IO(1:comm%ntot_item_sr)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      call set_reverse_sph_comm_tbl_t(numnod, comm)
!
      if(comm%id_domain(comm%nneib_domain) .eq. my_rank) then
        comm%iflag_self = 1
      else
        comm%iflag_self = 0
      end if
!
      end subroutine copy_comm_sph_type_from_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_comm_sph_type_to_IO(my_rank, comm)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_comm_tbl), intent(inout) :: comm
!
      my_rank_IO = my_rank
      num_neib_domain_IO = comm%nneib_domain
      ntot_import_IO =     comm%ntot_item_sr
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      id_neib_domain_IO(1:comm%nneib_domain)                            &
     &      = comm%id_domain(1:comm%nneib_domain)
      istack_import_IO(0:comm%nneib_domain)                             &
     &      = comm%istack_sr(0:comm%nneib_domain)
!
      item_import_IO(1:comm%ntot_item_sr)                               &
     &      = comm%item_sr(1:comm%ntot_item_sr)
!
      call dealloc_type_sph_comm_item(comm)
!
      end subroutine copy_comm_sph_type_to_IO
!
! -----------------------------------------------------------------------
!
      end module copy_sph_comm_tbl_type_4_IO
