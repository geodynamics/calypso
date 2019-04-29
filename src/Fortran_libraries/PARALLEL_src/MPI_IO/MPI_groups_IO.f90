!>@file  MPI_groups_IO.f90
!!       module MPI_groups_IO
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine mpi_read_group_data(IO_param, group_IO)
!!      subroutine mpi_read_surf_grp_data(IO_param, surf_grp_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!
!!      subroutine mpi_write_grp_data(IO_param, group_IO)
!!      subroutine mpi_write_surf_grp_data(IO_param, surf_grp_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(group_data), intent(in) :: group_IO
!!        type(surface_group_data), intent(in) :: surf_grp_IO
!!@endverbatim
!
      module MPI_groups_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
      use t_calypso_mpi_IO_param
!
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
      use field_data_MPI_IO
!
      implicit none
!
      private :: mpi_read_surf_grp_item, mpi_write_surf_grp_item
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_group_data(IO_param, group_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(group_data), intent(inout) :: group_IO
!
      integer(kind = kint) :: i, ist, ied, num, num_tmp
!
!
      call read_integer_textline                                        &
     &   (mpi_read_charahead(IO_param, len_int_txt), group_IO%num_grp)
      call alloc_group_num(group_IO)
!
      if(group_IO%num_grp .le. 0) then
        group_IO%num_item = 0
        call alloc_group_item(group_IO)
      else
        call mpi_read_num_of_data(IO_param, num_tmp)
        call mpi_read_int_stack(IO_param, group_IO%num_grp,             &
     &      group_IO%istack_grp, group_IO%num_item)
!
        call alloc_group_item(group_IO)
!
        do i = 1, group_IO%num_grp
          call read_field_name_mpi                                      &
     &       (IO_param%id_file, IO_param%ioff_gl, group_IO%grp_name(i))
          ist = group_IO%istack_grp(i-1) + 1
          ied = group_IO%istack_grp(i)
          num = group_IO%istack_grp(i) - group_IO%istack_grp(i-1)
          call mpi_read_num_of_data(IO_param, num_tmp)
          call mpi_read_comm_table                                      &
     &       (IO_param, ieight, num, group_IO%item_grp(ist:ied))
        end do
      end if
!
      end subroutine mpi_read_group_data
!
!------------------------------------------------------------------
!
      subroutine mpi_read_surf_grp_data(IO_param, surf_grp_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint) :: i, num, num_tmp
!
!
      call read_integer_textline                                        &
     &   (mpi_read_charahead(IO_param, len_int_txt),                    &
     &    surf_grp_IO%num_grp)
      call alloc_sf_group_num(surf_grp_IO)
!
      if(surf_grp_IO%num_grp .le. 0) then
        call alloc_sf_group_num(surf_grp_IO)
        surf_grp_IO%num_item = 0
        call alloc_sf_group_item(surf_grp_IO)
      else
        call mpi_read_num_of_data(IO_param, num_tmp)
        call mpi_read_int_stack(IO_param, surf_grp_IO%num_grp,          &
     &      surf_grp_IO%istack_grp, surf_grp_IO%num_item)
!
        call alloc_sf_group_item(surf_grp_IO)
!
        do i = 1, surf_grp_IO%num_grp
          call read_field_name_mpi(IO_param%id_file, IO_param%ioff_gl,  &
     &        surf_grp_IO%grp_name(i))
!
          num = surf_grp_IO%istack_grp(i) - surf_grp_IO%istack_grp(i-1)
          call mpi_read_surf_grp_item(IO_param, ieight,                 &
     &        surf_grp_IO%num_item, surf_grp_IO%istack_grp(i-1),        &
     &        num, surf_grp_IO%item_sf_grp)
        end do
      end if
!
      end subroutine mpi_read_surf_grp_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_grp_data(IO_param, group_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(group_data), intent(in) :: group_IO
!
      integer(kind = kint) :: i, ist, ied, num
!
!
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(group_IO%num_grp))
      call mpi_write_int_stack                                          &
     &   (IO_param, group_IO%num_grp, group_IO%istack_grp)
!
      do i = 1, group_IO%num_grp
        call mpi_write_charahead(IO_param,                              &
     &      len_one_word_textline(group_IO%grp_name(i)),                &
     &      one_word_textline(group_IO%grp_name(i)))
!
        ist = group_IO%istack_grp(i-1) + 1
        ied = group_IO%istack_grp(i)
        num = group_IO%istack_grp(i) - group_IO%istack_grp(i-1)
        call mpi_write_comm_table                                       &
     &     (IO_param, ieight, num, group_IO%item_grp(ist:ied))
      end do
!
      end subroutine mpi_write_grp_data
!
!------------------------------------------------------------------
!
      subroutine mpi_write_surf_grp_data(IO_param, surf_grp_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(surface_group_data), intent(in) :: surf_grp_IO
!
      integer(kind = kint) :: i, num
!
!
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(surf_grp_IO%num_grp))
      call mpi_write_int_stack                                          &
     &   (IO_param, surf_grp_IO%num_grp, surf_grp_IO%istack_grp)
!
      do i = 1, surf_grp_IO%num_grp
        call mpi_write_charahead(IO_param,                              &
     &      len_one_word_textline(surf_grp_IO%grp_name(i)),             &
     &      one_word_textline(surf_grp_IO%grp_name(i)))
!
        num = surf_grp_IO%istack_grp(i) - surf_grp_IO%istack_grp(i-1)
        call mpi_write_surf_grp_item(IO_param, ieight,                  &
     &      surf_grp_IO%num_item, surf_grp_IO%istack_grp(i-1),          &
     &      num, surf_grp_IO%item_sf_grp)
      end do
!
      end subroutine mpi_write_surf_grp_data
!
!------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_surf_grp_item                                 &
     &         (IO_param, ncolumn, ntot, ist, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: ntot, ist, num, ncolumn
      integer(kind=kint), intent(inout) :: int_dat(2,ntot)
!
      integer(kind = kint) :: int_tmp(num), num_tmp
!
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_comm_table(IO_param, ncolumn, num, int_tmp)
!$omp parallel workshare
      int_dat(1,ist+1:ist+num) = int_tmp(1:num)
!$omp end parallel workshare
!
!
      call mpi_read_num_of_data(IO_param, num_tmp)
      call mpi_read_comm_table(IO_param, ncolumn, num, int_tmp)
!$omp parallel workshare
      int_dat(2,ist+1:ist+num) = int_tmp(1:num)
!$omp end parallel workshare
!
      end subroutine mpi_read_surf_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_surf_grp_item                                &
     &         (IO_param, ncolumn, ntot, ist, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: ntot, ist, num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(2,ntot)
!
      integer(kind = kint) :: int_tmp(num)
!
!
!$omp parallel workshare
       int_tmp(1:num) = int_dat(1,ist+1:ist+num)
!$omp end parallel workshare
      call mpi_write_comm_table(IO_param, ncolumn, num, int_tmp)
!
!
!$omp parallel workshare
       int_tmp(1:num) = int_dat(2,ist+1:ist+num)
!$omp end parallel workshare
      call mpi_write_comm_table(IO_param, ncolumn, num, int_tmp)
!
      end subroutine mpi_write_surf_grp_item
!
! -----------------------------------------------------------------------
!
      end module MPI_groups_IO
