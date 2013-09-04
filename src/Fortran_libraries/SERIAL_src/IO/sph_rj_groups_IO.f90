!sph_rj_groups_IO.f90
!      module sph_rj_groups_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_modes_rj_groups(mesh_file_id)
!      subroutine write_modes_rj_groups(mesh_file_id)
!
      module sph_rj_groups_IO
!
      use m_precision
!
      use m_constants
      use m_group_data_sph_specr_IO
      use stack_array_IO
      use group_data_IO
      use skip_comment_f
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
      private :: read_rj_radial_grp_data, write_rj_radial_grp_data
      private :: read_rj_sphere_grp_data, write_rj_sphere_grp_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_modes_rj_groups(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      call read_rj_radial_grp_data(mesh_file_id)
      call read_rj_sphere_grp_data(mesh_file_id)
!
      end subroutine read_modes_rj_groups
!
!------------------------------------------------------------------
!
      subroutine write_modes_rj_groups(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)') '!'
      write(mesh_file_id,'(a)') '! Group data'
      write(mesh_file_id,'(a)') '!'
      call write_rj_radial_grp_data(mesh_file_id)
      call write_rj_sphere_grp_data(mesh_file_id)
!
      end subroutine write_modes_rj_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rj_radial_grp_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      call skip_comment(character_4_read, mesh_file_id)
      read(character_4_read,*) num_radial_grp_rj_IO
!
      call allocate_rj_r_grp_IO_stack
!
      if (num_radial_grp_rj_IO .gt. 0) then
        call read_group_stack(mesh_file_id, num_radial_grp_rj_IO,       &
     &      ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO)
!
        call allocate_rj_r_grp_IO_item
        call read_group_item(mesh_file_id, num_radial_grp_rj_IO,        &
     &      ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO,             &
     &      name_radial_grp_rj_IO,item_radial_grp_rj_IO)
!
      else
        ntot_radial_grp_rj_IO = 0
        call allocate_rj_r_grp_IO_item
      end if
!
      end subroutine read_rj_radial_grp_data
!
!------------------------------------------------------------------
!
      subroutine read_rj_sphere_grp_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      call skip_comment(character_4_read, mesh_file_id)
      read(character_4_read,*) num_sphere_grp_rj_IO
!
      call allocate_rj_j_grp_IO_stack
!
      if (num_sphere_grp_rj_IO .gt. 0) then
        call read_group_stack(mesh_file_id, num_sphere_grp_rj_IO,       &
     &      ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO)
!
        call allocate_rj_j_grp_IO_item
        call read_group_item(mesh_file_id, num_sphere_grp_rj_IO,        &
     &      ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO,             &
     &      name_sphere_grp_rj_IO, item_sphere_grp_rj_IO)
!
      else
        ntot_sphere_grp_rj_IO = 0
        call allocate_rj_j_grp_IO_item
      end if
!
      end subroutine read_rj_sphere_grp_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_rj_radial_grp_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)') '!'
      write(mesh_file_id,'(a)') '! radial groups in spectr'
      write(mesh_file_id,'(a)') '!'
!
      call write_group_data(mesh_file_id, num_radial_grp_rj_IO,         &
     &    ntot_radial_grp_rj_IO, istack_radial_grp_rj_IO,               &
     &    name_radial_grp_rj_IO, item_radial_grp_rj_IO)
!
      call deallocate_rj_r_grp_IO_item
!
      end subroutine write_rj_radial_grp_data
!
!------------------------------------------------------------------
!
      subroutine write_rj_sphere_grp_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)') '!'
      write(mesh_file_id,'(a)') '! spehrical harmonics groups'
      write(mesh_file_id,'(a)') '!'
!
      call write_group_data(mesh_file_id, num_sphere_grp_rj_IO,         &
     &    ntot_sphere_grp_rj_IO, istack_sphere_grp_rj_IO,               &
     &    name_sphere_grp_rj_IO, item_sphere_grp_rj_IO)
!
      call deallocate_rj_j_grp_IO_item
!
      end subroutine write_rj_sphere_grp_data
!
!------------------------------------------------------------------
!
      end module sph_rj_groups_IO
