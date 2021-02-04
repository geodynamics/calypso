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
      use calypso_mpi_real
      use calypso_mpi_int
      use t_time_data
!
      type(time_data), intent(inout) :: time_d
!
!
      call calypso_mpi_bcast_one_int(time_d%i_time_step, 0)
      call calypso_mpi_bcast_one_real(time_d%time, 0)
      call calypso_mpi_bcast_one_real(time_d%dt, 0)
!
      end subroutine share_time_step_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_phys_field_names(fld)
!
      use t_phys_data
      use calypso_mpi_logical
      use transfer_to_long_integers
      use cal_minmax_and_stacks
!
      type(phys_data), intent(inout) :: fld
!
      call share_field_num(fld%num_phys, fld%ntot_phys)
!
      if(my_rank .ne. 0) call alloc_phys_name(fld)
!
      call share_field_stack                                            &
     &   (fld%num_phys, fld%istack_component, fld%phys_name)
      call s_cal_numbers_from_stack                                     &
     &   (fld%num_phys, fld%num_component, fld%istack_component)
!
      call calypso_mpi_bcast_logical                                    &
     &   (fld%flag_monitor, cast_long(fld%num_phys), 0)
!
      end subroutine share_phys_field_names
!
! -----------------------------------------------------------------------
!
      subroutine share_each_field_data(ip_org, fld)
!
      use t_phys_data
      use calypso_mpi_real
      use calypso_mpi_int
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
      call calypso_mpi_bcast_one_int(fld%n_point, irank_org)
!
      if(mod(irank_org,nprocs) .ne. my_rank) then
        call alloc_phys_data(fld%n_point, fld)
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
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
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
      call calypso_mpi_bcast_one_int(fld_IO%num_field_IO, irank_org)
!        write(*,*) 'MPI_Bcast fld_IO%ntot_comp_IO', ip
      call calypso_mpi_bcast_one_int(fld_IO%ntot_comp_IO, irank_org)
!
      if(my_rank .ne. irank_org) call alloc_phys_name_IO(fld_IO)
!
!
!        write(*,*) 'MPI_Bcast fld_IO%istack_comp_IO', ip
      call calypso_mpi_bcast_int(fld_IO%istack_comp_IO,                 &
     &    cast_long(fld_IO%num_field_IO+1), irank_org)
!        write(*,*) 'MPI_Bcast phys_name', ip
      call calypso_mpi_bcast_character(fld_IO%fld_name,                 &
     &    cast_long(fld_IO%num_field_IO*kchara), irank_org)
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
      use calypso_mpi_real
      use calypso_mpi_int
!
      integer(kind = kint), intent(in) :: ip_org
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org-1,nprocs))
      call calypso_mpi_bcast_one_int(fld_IO%nnod_IO, irank_org)
!
      if(mod(irank_org,nprocs) .ne. my_rank) then
        call alloc_phys_data_IO(fld_IO)
      end if
!
      num = fld_IO%ntot_comp_IO * fld_IO%nnod_IO
      call calypso_mpi_bcast_real(fld_IO%d_IO, num, irank_org)
!
      end subroutine share_each_field_IO_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_field_num(num_field, ntot_component)
!
      use calypso_mpi_int
!
      integer(kind = kint), intent(inout) :: num_field
      integer(kind = kint), intent(inout) :: ntot_component
!
      call calypso_mpi_bcast_one_int(num_field, 0)
      call calypso_mpi_bcast_one_int(ntot_component, 0)
!
     end  subroutine share_field_num
!
! -----------------------------------------------------------------------
!
      subroutine share_field_stack(num_field, istack_comp, fld_name)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(inout) :: istack_comp(0:num_field)
      character(len=kchara), intent(inout) :: fld_name(num_field)
!
      call calypso_mpi_bcast_int                                        &
     &   (istack_comp, cast_long(num_field+1), 0)
      call calypso_mpi_bcast_character                                  &
     &   (fld_name, cast_long(num_field*kchara), 0)
      end subroutine share_field_stack
!
! -----------------------------------------------------------------------
!
      end module share_field_data
