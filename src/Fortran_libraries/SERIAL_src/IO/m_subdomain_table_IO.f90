!> @file  m_subdomain_table_IO.f90
!!      module m_subdomain_table_IO
!!
!! @author  H. Matsui
!! @date Written in June, 2009
!
!> @brief Arrays for subdomain list for partitioner
!!
!!@verbatim
!!      subroutine allocate_domain_group_IO
!!      subroutine deallocate_domain_group_IO
!!
!!      subroutine output_group_4_partition
!!      subroutine read_group_4_partition
!!
!!      subroutine read_group_by_metis(ierr, numnod, internal_node)
!!@endverbatim
!
      module m_subdomain_table_IO
!
      use m_precision
!
      implicit none
!
      integer(kind=kint ), parameter :: id_subdomain = 21
      character(len=kchara) :: fname_subdomain = 'subdomain_table.dat'
      character(len=kchara) :: metis_sdom_name ='metis_part.dat'
!
      integer(kind = kint) :: nproc_group_IO
      integer(kind = kint) :: nnod_group_IO, internod_group_IO
      integer(kind = kint),  allocatable :: IGROUP_IO(:)
!
      character(len=255) :: character_4_read
      private :: character_4_read
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_domain_group_IO
!
      allocate(IGROUP_IO(nnod_group_IO))
      IGROUP_IO = 0
!
      end subroutine allocate_domain_group_IO
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_domain_group_IO
!
      deallocate(IGROUP_IO)
!
      end subroutine deallocate_domain_group_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine output_group_4_partition
!
      open(id_subdomain,file = fname_subdomain)
!
      write(id_subdomain,*) '! number of subdomains'
      write(id_subdomain,'(i16)') nproc_group_IO
!
      write(id_subdomain,*) '! number of total node'
      write(id_subdomain,'(2i16)') nnod_group_IO, internod_group_IO
!
      write(id_subdomain,'(10i16)') IGROUP_IO(1:nnod_group_IO)
!
      close(id_subdomain)
!
      call deallocate_domain_group_IO
!
      end subroutine output_group_4_partition
!
!   --------------------------------------------------------------------
!
      subroutine read_group_4_partition
!
      use skip_comment_f
!
      open(id_subdomain,file = fname_subdomain)
!
      call skip_comment(character_4_read, id_subdomain)
      read(character_4_read,*) nproc_group_IO
!
      call skip_comment(character_4_read, id_subdomain)
      read(character_4_read,*) nnod_group_IO, internod_group_IO
!
      call allocate_domain_group_IO
      read(id_subdomain,*) IGROUP_IO(1:nnod_group_IO)
!
      close(id_subdomain)
!
      end subroutine read_group_4_partition
!
!   --------------------------------------------------------------------
!
      subroutine read_group_by_metis(ierr, numnod, internal_node)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint) :: inod, NPARTMAX
!
!
      ierr = 0
      open(id_subdomain,file = metis_sdom_name)
!
      NPARTMAX=-100
!
      nnod_group_IO =     numnod
      internod_group_IO = internal_node
      call allocate_domain_group_IO
!
      do inod = 1, internal_node
        read(id_subdomain,*) IGROUP_IO(inod)
      end do
      close(id_subdomain)
!
      do inod = 1, internal_node
        IGROUP_IO(inod) = IGROUP_IO(inod) + 1
        NPARTMAX = max(NPARTMAX,IGROUP_IO(inod))
      end do
!
      nproc_group_IO = NPARTMAX
      if (nproc_group_IO.lt.1) ierr = 32
!
      end subroutine read_group_by_metis
!
!   --------------------------------------------------------------------
!
      end module m_subdomain_table_IO
