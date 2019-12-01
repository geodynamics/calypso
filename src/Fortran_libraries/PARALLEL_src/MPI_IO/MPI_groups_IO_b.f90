!>@file  MPI_groups_IO_b.f90
!!       module MPI_groups_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine mpi_read_group_data_b(IO_param, group_IO)
!!      subroutine mpi_read_surf_grp_data_b(IO_param, surf_grp_IO)
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!
!!      subroutine mpi_write_grp_data_b(IO_param, group_IO)
!!      subroutine mpi_write_surf_grp_data_b(IO_param, surf_grp_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(group_data), intent(in) :: group_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!@endverbatim
!
      module MPI_groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
      use t_calypso_mpi_IO_param
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_group_data_b(IO_param, group_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(group_data), intent(inout) :: group_IO
!
      integer(kind = kint_gl) :: num64
!
      integer i
!
      call mpi_read_one_inthead_b(IO_param, group_IO%num_grp)
      call alloc_group_num(group_IO)
!
      call mpi_read_mul_charahead_b                                     &
     &     (IO_param, group_IO%num_grp, group_IO%grp_name)
!
      num64 = group_IO%num_grp
      call mpi_read_integer_stack_b(IO_param, num64,                    &
     &    group_IO%istack_grp, group_IO%num_item)
!
      call alloc_group_item(group_IO)
!
      num64 = group_IO%num_item
      call mpi_read_int_vector_b(IO_param, num64, group_IO%item_grp)
!
      end subroutine mpi_read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_surf_grp_data_b(IO_param, surf_grp_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_read_one_inthead_b(IO_param, surf_grp_IO%num_grp)
      call alloc_sf_group_num(surf_grp_IO)
!
      call mpi_read_mul_charahead_b                                     &
     &     (IO_param, surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
      num64 = surf_grp_IO%num_grp
      call mpi_read_integer_stack_b(IO_param, num64,                    &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
!
      call alloc_sf_group_item(surf_grp_IO)
!
        num64 = 2 * surf_grp_IO%num_item
        call mpi_read_int_vector_b                                      &
     &     (IO_param, num64, surf_grp_IO%item_sf_grp)
!
      end subroutine mpi_read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_grp_data_b(IO_param, group_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(group_data), intent(in) :: group_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_write_one_inthead_b(IO_param, group_IO%num_grp)
!
      call mpi_write_mul_charahead_b                                    &
     &     (IO_param, group_IO%num_grp, group_IO%grp_name)
!
      num64 = group_IO%num_grp
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_integer_stack_b                                    &
     &   (IO_param, num64, group_IO%istack_grp)
!
      num64 = group_IO%num_item
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param, num64, group_IO%item_grp)
!
      end subroutine mpi_write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_surf_grp_data_b(IO_param, surf_grp_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surface_group_data), intent(in) :: surf_grp_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_write_one_inthead_b(IO_param, surf_grp_IO%num_grp)
!
      call mpi_write_mul_charahead_b                                    &
     &   (IO_param, surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
      num64 = surf_grp_IO%num_grp
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_integer_stack_b                                    &
     &     (IO_param, num64, surf_grp_IO%istack_grp)
!
      num64 = 2 * surf_grp_IO%num_item
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b                                       &
     &   (IO_param, num64, surf_grp_IO%item_sf_grp)
!
      end subroutine mpi_write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module MPI_groups_IO_b
