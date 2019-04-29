!>@file  gz_MPI_groups_IO_b.f90
!!       module gz_MPI_groups_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine gz_mpi_read_group_data_b(IO_param, group_IO)
!!      subroutine gz_mpi_read_surf_grp_data_b(IO_param, surf_grp_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!
!!      subroutine gz_mpi_write_grp_data_b(IO_param, group_IO)
!!      subroutine gz_mpi_write_surf_grp_data_b(IO_param, surf_grp_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(group_data), intent(in) :: group_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!@endverbatim
!
      module gz_MPI_groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
      use t_calypso_mpi_IO_param
!
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_group_data_b(IO_param, group_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(group_data), intent(inout) :: group_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call gz_mpi_read_one_inthead_b(IO_param, group_IO%num_grp)
      call alloc_group_num(group_IO)
!
      num64 = group_IO%num_grp
      call gz_mpi_read_mul_charahead_b                                  &
     &     (IO_param, group_IO%num_grp, group_IO%grp_name)
      call gz_mpi_read_integer_stack_b(IO_param, num64,                 &
     &      group_IO%istack_grp, group_IO%num_item)
!
      call alloc_group_item(group_IO)
!
      num64 = group_IO%num_item
      call gz_mpi_read_int_vector_b                                     &
     &     (IO_param, num64, group_IO%item_grp)
!
      end subroutine gz_mpi_read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_surf_grp_data_b(IO_param, surf_grp_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call gz_mpi_read_one_inthead_b(IO_param, surf_grp_IO%num_grp)
      call alloc_sf_group_num(surf_grp_IO)
!
      num64 = surf_grp_IO%num_grp
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, surf_grp_IO%num_grp, surf_grp_IO%grp_name)
      call gz_mpi_read_integer_stack_b(IO_param, num64,                 &
     &    surf_grp_IO%istack_grp, surf_grp_IO%num_item)
!
      call alloc_sf_group_item(surf_grp_IO)
!
      num64 = 2 * surf_grp_IO%num_item
      call gz_mpi_read_int_vector_b                                     &
     &     (IO_param, num64, surf_grp_IO%item_sf_grp)
!
      end subroutine gz_mpi_read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_grp_data_b(IO_param, group_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(group_data), intent(in) :: group_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call gz_mpi_write_one_inthead_b(IO_param, group_IO%num_grp)
!
      call gz_mpi_write_mul_charahead_b                                 &
     &     (IO_param, group_IO%num_grp, group_IO%grp_name)
      num64 = group_IO%num_grp
      call gz_mpi_write_integer_stack_b                                 &
     &   (IO_param, num64, group_IO%istack_grp)
!
      num64 = group_IO%num_item
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, num64, group_IO%item_grp)
!
      end subroutine gz_mpi_write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_surf_grp_data_b(IO_param, surf_grp_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surface_group_data), intent(in) :: surf_grp_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call gz_mpi_write_one_inthead_b(IO_param, surf_grp_IO%num_grp)
!
      call gz_mpi_write_mul_charahead_b                                 &
     &     (IO_param, surf_grp_IO%num_grp, surf_grp_IO%grp_name)
      num64 = surf_grp_IO%num_grp
      call gz_mpi_write_integer_stack_b                                 &
     &   (IO_param, num64, surf_grp_IO%istack_grp)
!
      num64 = 2 * surf_grp_IO%num_item
      call gz_mpi_write_int_vector_b                                    &
     &   (IO_param, num64, surf_grp_IO%item_sf_grp)
!
      end subroutine gz_mpi_write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_groups_IO_b
