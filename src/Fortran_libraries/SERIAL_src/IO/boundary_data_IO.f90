!boundary_data_IO.f90
!      module boundary_data_IO
!
!     Written by H. Matsui on Sep., 2006
!
!       subroutine read_boundary_data_a(input_file_code)
!       subroutine write_boundary_data_a(input_file_code)
!
      module boundary_data_IO
!
      use m_precision
!
      use m_read_boundary_data
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
       subroutine read_boundary_data_a(input_file_code)
!
       use skip_comment_f
       use group_data_IO
!
       integer (kind = kint), intent(in) :: input_file_code
!
       character(len=255) :: character_4_read
!
!
!   read node group
!
      call skip_comment(character_4_read,input_file_code)
      read(character_4_read,*) num_bc_dummy
!      write(*,*) 'num_bc_dummy', num_bc_dummy
!
      call allocate_bc_stack_dummy
!
      call read_group_stack(input_file_code,                            &
     &    num_bc_dummy, num_nod_bc_dummy, bc_istack_dummy)
!
      call allocate_bc_item_dummy
!
      call read_group_item(input_file_code, num_bc_dummy,               &
     &    num_nod_bc_dummy, bc_istack_dummy, bc_name_dummy,             &
     &    bc_item_dummy)
!
!  read element group
!
      call skip_comment(character_4_read,input_file_code)
      read(character_4_read,*) num_mat_dummy
!      write(*,*) 'num_mat_dummy', num_mat_dummy
!
      call allocate_bc_ele_stack_dummy
!
      call read_group_stack(input_file_code,                            &
     &    num_mat_dummy, num_mat_bc_dummy, mat_istack_dummy)
!
      call allocate_bc_ele_item_dummy
!
      call read_group_item(input_file_code, num_mat_dummy,              &
     &    num_mat_bc_dummy, mat_istack_dummy, mat_name_dummy,           &
     &    mat_item_dummy)
!
!  read surface group
!
      call skip_comment(character_4_read,input_file_code)
      read(character_4_read,*) num_surf_dummy
!      write(*,*) 'num_surf_dummy', num_surf_dummy
!
      call allocate_bc_sf_stack_dummy
!
      call read_group_stack(input_file_code,                            &
     &    num_surf_dummy, num_surf_bc_dummy, surf_istack_dummy)
!
      call allocate_bc_sf_item_dummy
!
      call read_surface_group_item(input_file_code, num_surf_dummy,     &
     &    num_surf_bc_dummy, surf_istack_dummy, surf_name_dummy,        &
     &    surf_item_dummy)
!
      end subroutine read_boundary_data_a
!
! ----------------------------------------------------------------------
!
      subroutine write_boundary_data_a(input_file_code)
!
      use group_data_IO
!
      integer (kind = kint), intent(in) :: input_file_code
!
!   write node group
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4. group information  '
      write(input_file_code,'(a)') '! 4.1 node group '
      write(input_file_code,'(a)') '!'
!
      call write_group_data(input_file_code, num_bc_dummy,              &
     &    num_nod_bc_dummy, bc_istack_dummy, bc_name_dummy,             &
     &    bc_item_dummy)
!
      call deallocate_bc_item_dummy
!
!  write element group
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.2 element group '
      write(input_file_code,'(a)') '!'
!
      call write_group_data(input_file_code, num_mat_dummy,             &
     &    num_mat_bc_dummy, mat_istack_dummy, mat_name_dummy,           &
     &    mat_item_dummy)
!
      call deallocate_bc_ele_item_dummy
!
!  write surface group
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.3 surface group '
      write(input_file_code,'(a)') '!'
!
      call write_surf_group_data(input_file_code, num_surf_dummy,       &
     &    num_surf_bc_dummy, surf_istack_dummy, surf_name_dummy,        &
     &    surf_item_dummy)
!
      call deallocate_bc_sf_item_dummy
!
      end subroutine write_boundary_data_a
!
! ----------------------------------------------------------------------
!
      end module boundary_data_IO
