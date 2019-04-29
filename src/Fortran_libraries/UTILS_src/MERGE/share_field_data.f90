!>@file   share_field_data.f90
!!@brief  module share_field_data
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2011
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine share_time_step_data(time_d)
!!        type(time_data), intent(inout) :: time_d
!!
!!      subroutine share_phys_field_names(fld)
!!      subroutine share_each_field_data(ip_org, fld)
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine share_field_IO_names(fld_IO)
!!      subroutine share_each_field_IO_data(ip_org, fld_IO)
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module share_field_data
!
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine share_time_step_data(time_d)
!
      use t_time_data
!
      type(time_data), intent(inout) :: time_d
!
!
      call MPI_Bcast(time_d%i_time_step, 1, CALYPSO_INTEGER,            &
     &    0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(time_d%time, 1, CALYPSO_REAL,                      &
     &    0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(time_d%dt, 1, CALYPSO_REAL,                        &
     &    0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_time_step_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_phys_field_names(fld)
!
      use t_phys_data
      use cal_minmax_and_stacks
!
      type(phys_data), intent(inout) :: fld
!
!        write(*,*) 'MPI_Bcast num_phys', ip
      call share_field_num(fld%num_phys, fld%ntot_phys)
!
      if(my_rank .ne. 0) call alloc_phys_name_type(fld)
!
      call share_field_stack                                            &
     &   (fld%num_phys, fld%istack_component, fld%phys_name)
      call s_cal_numbers_from_stack                                     &
     &   (fld%num_phys, fld%num_component, fld%istack_component)
!
!        write(*,*) 'MPI_Bcast iflag_monitor', ip
      call MPI_Bcast(fld%iflag_monitor, int(fld%num_phys),              &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_phys_field_names
!
! -----------------------------------------------------------------------
!
      subroutine share_each_field_data(ip_org, fld)
!
      use t_phys_data
!
      integer(kind = kint), intent(in) :: ip_org
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint_gl) ::  num64
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org-1,nprocs))
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(fld%n_point, 1, CALYPSO_INTEGER,                   &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(irank_org,nprocs) .ne. my_rank) then
        call alloc_phys_data_type(fld%n_point, fld)
      end if
!
      num64 = fld%ntot_phys * fld%n_point
      call calypso_mpi_bcast_real(fld%d_fld, num64, irank_org)
!
      end subroutine share_each_field_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_field_IO_names(ip_org, fld_IO)
!
      use t_field_data_IO
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: ip_org
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org-1,nprocs))
!        write(*,*) 'MPI_Bcast fld_IO%num_field_IO', ip
      call MPI_Bcast(fld_IO%num_field_IO, 1,                            &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast fld_IO%ntot_comp_IO', ip
      call MPI_Bcast(fld_IO%ntot_comp_IO, 1,                            &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. irank_org) call alloc_phys_name_IO(fld_IO)
!
!
!        write(*,*) 'MPI_Bcast fld_IO%istack_comp_IO', ip
      call MPI_Bcast(fld_IO%istack_comp_IO, int(fld_IO%num_field_IO+1), &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast phys_name', ip
      call MPI_Bcast(fld_IO%fld_name, int(fld_IO%num_field_IO*kchara),  &
     &    CALYPSO_CHARACTER, irank_org, CALYPSO_COMM, ierr_MPI)
      call s_cal_numbers_from_stack(fld_IO%num_field_IO,                &
     &    fld_IO%num_comp_IO, fld_IO%istack_comp_IO)
!
      end subroutine share_field_IO_names
!
! -----------------------------------------------------------------------
!
      subroutine share_each_field_IO_data(ip_org, fld_IO)
!
      use t_field_data_IO
!
      integer(kind = kint), intent(in) :: ip_org
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: num
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org-1,nprocs))
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(fld_IO%nnod_IO, 1, CALYPSO_INTEGER,                &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(irank_org,nprocs) .ne. my_rank) then
        call alloc_phys_data_IO(fld_IO)
      end if
!
      num = fld_IO%ntot_comp_IO * fld_IO%nnod_IO
      call MPI_Bcast(fld_IO%d_IO, 1, CALYPSO_REAL,                      &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_each_field_IO_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_field_num(num_field, ntot_component)
!
      integer(kind = kint), intent(inout) :: num_field
      integer(kind = kint), intent(inout) :: ntot_component
!
!        write(*,*) 'MPI_Bcast num_field', ip
      call MPI_Bcast(num_field, 1,                                      &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast ntot_component', ip
      call MPI_Bcast(ntot_component, 1,                                 &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
     end  subroutine share_field_num
!
! -----------------------------------------------------------------------
!
      subroutine share_field_stack(num_field, istack_comp, fld_name)
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(inout) :: istack_comp(0:num_field)
      character(len=kchara), intent(inout) :: fld_name(num_field)
!
!        write(*,*) 'MPI_Bcast istack_comp', ip
      call MPI_Bcast(istack_comp, int(num_field+1),                     &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast phys_name', ip
      call MPI_Bcast(fld_name, int(num_field*kchara),                   &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_field_stack
!
! -----------------------------------------------------------------------
!
      end module share_field_data
