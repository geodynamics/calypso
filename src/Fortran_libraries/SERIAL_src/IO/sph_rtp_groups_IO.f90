!>@file  sph_rtp_groups_IO.f90
!!       module sph_rtp_groups_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief routines for spectrum group data IO
!!
!!@verbatim
!!      subroutine read_geom_rtp_groups(mesh_file_id)
!!      subroutine write_geom_rtp_groups(mesh_file_id)
!!@endverbatim
!
      module sph_rtp_groups_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_group_data_sph_specr_IO
      use stack_array_IO
      use group_data_IO
      use skip_comment_f
!
      implicit none
!
      private :: read_rtp_node_grp_data,   write_rtp_node_grp_data
      private :: read_rtp_radial_grp_data, write_rtp_radial_grp_data
      private :: read_rtp_theta_grp_data,  write_rtp_theta_grp_data
      private :: read_rtp_zonal_grp_data,  write_rtp_zonal_grp_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_groups(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      if(iflag_debug .gt. 0) write(*,*) 'read_rtp_node_grp_data'
      call read_rtp_node_grp_data(mesh_file_id)
!
      if(iflag_debug .gt. 0) write(*,*) 'read_rtp_radial_grp_data'
      call read_rtp_radial_grp_data(mesh_file_id)
!
      if(iflag_debug .gt. 0) write(*,*) 'read_rtp_theta_grp_data'
      call read_rtp_theta_grp_data(mesh_file_id)
!
      if(iflag_debug .gt. 0) write(*,*) 'read_rtp_zonal_grp_data'
      call read_rtp_zonal_grp_data(mesh_file_id)
!
      end subroutine read_geom_rtp_groups
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_groups(mesh_file_id)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)', advance='NO') hd_grphd()
!
      call write_rtp_node_grp_data(mesh_file_id)
!
      if(iflag_debug .gt. 0) write(*,'(a)') 'write_rtp_radial_grp_data'
      call write_rtp_radial_grp_data(mesh_file_id)
!
      if(iflag_debug .gt. 0) write(*,'(a)') 'write_rtp_theta_grp_data'
      call write_rtp_theta_grp_data(mesh_file_id)
!
      if(iflag_debug .gt. 0) write(*,'(a)') 'write_rtp_zonal_grp_data'
      call write_rtp_zonal_grp_data(mesh_file_id)
!
      end subroutine write_geom_rtp_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rtp_node_grp_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      character(len=kchara) :: character_4_read
!
!
      call skip_comment(character_4_read, mesh_file_id)
      read(character_4_read,*) num_bc_grp_rtp_IO
!
      call allocate_rtp_nod_grp_IO_stack
!
      if (num_bc_grp_rtp_IO .gt. 0) then
        call read_group_stack(mesh_file_id, num_bc_grp_rtp_IO,          &
     &      ntot_bc_grp_rtp_IO, istack_bc_grp_rtp_IO)
!
        call allocate_rtp_nod_grp_IO_item
        call read_group_item(mesh_file_id, num_bc_grp_rtp_IO,           &
     &      ntot_bc_grp_rtp_IO, istack_bc_grp_rtp_IO,                   &
     &      name_bc_grp_rtp_IO,item_bc_grp_rtp_IO)
!
      else
        ntot_bc_grp_rtp_IO = 0
        call allocate_rtp_nod_grp_IO_item
      end if
!
      end subroutine read_rtp_node_grp_data
!
!------------------------------------------------------------------
!
      subroutine read_rtp_radial_grp_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      character(len=kchara) :: character_4_read
!
!
      call skip_comment(character_4_read, mesh_file_id)
      read(character_4_read,*) num_radial_grp_rtp_IO
!
      call allocate_rtp_r_grp_IO_stack
!
      if (num_radial_grp_rtp_IO .gt. 0) then
        call read_group_stack(mesh_file_id, num_radial_grp_rtp_IO,      &
     &      ntot_radial_grp_rtp_IO, istack_radial_grp_rtp_IO)
!
        call allocate_rtp_r_grp_IO_item
        call read_group_item(mesh_file_id, num_radial_grp_rtp_IO,       &
     &      ntot_radial_grp_rtp_IO, istack_radial_grp_rtp_IO,           &
     &      name_radial_grp_rtp_IO,item_radial_grp_rtp_IO)
!
      else
        ntot_radial_grp_rtp_IO = 0
        call allocate_rtp_r_grp_IO_item
      end if
!
      end subroutine read_rtp_radial_grp_data
!
!------------------------------------------------------------------
!
      subroutine read_rtp_theta_grp_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      character(len=kchara) :: character_4_read
!
!
      call skip_comment(character_4_read, mesh_file_id)
      read(character_4_read,*) num_theta_grp_rtp_IO
!
      call allocate_rtp_t_grp_IO_stack
!
      if (num_theta_grp_rtp_IO .gt. 0) then
        call read_group_stack(mesh_file_id, num_theta_grp_rtp_IO,       &
     &      ntot_theta_grp_rtp_IO, istack_theta_grp_rtp_IO)
!
        call allocate_rtp_t_grp_IO_item
        call read_group_item(mesh_file_id, num_theta_grp_rtp_IO,        &
     &      ntot_theta_grp_rtp_IO, istack_theta_grp_rtp_IO,             &
     &      name_theta_grp_rtp_IO,item_theta_grp_rtp_IO)
!
      else
        ntot_theta_grp_rtp_IO = 0
        call allocate_rtp_t_grp_IO_item
      end if
!
      end subroutine read_rtp_theta_grp_data
!
!------------------------------------------------------------------
!
      subroutine read_rtp_zonal_grp_data(mesh_file_id)
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
      character(len=kchara) :: character_4_read
!
!
      call skip_comment(character_4_read, mesh_file_id)
      read(character_4_read,*) num_zonal_grp_rtp_IO
!
      call allocate_rtp_p_grp_IO_stack
!
      if (num_zonal_grp_rtp_IO .gt. 0) then
        call read_group_stack(mesh_file_id, num_zonal_grp_rtp_IO,       &
     &      ntot_zonal_grp_rtp_IO, istack_zonal_grp_rtp_IO)
!
        call allocate_rtp_p_grp_IO_item
        call read_group_item(mesh_file_id, num_zonal_grp_rtp_IO,        &
     &      ntot_zonal_grp_rtp_IO, istack_zonal_grp_rtp_IO,             &
     &      name_zonal_grp_rtp_IO,item_zonal_grp_rtp_IO)
!
      else
        ntot_zonal_grp_rtp_IO = 0
        call allocate_rtp_p_grp_IO_item
      end if
!
      end subroutine read_rtp_zonal_grp_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_rtp_node_grp_data(mesh_file_id)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)', advance='NO') hd_ngrphd()
!
      call write_group_data(mesh_file_id, num_bc_grp_rtp_IO,            &
     &    ntot_bc_grp_rtp_IO, istack_bc_grp_rtp_IO, name_bc_grp_rtp_IO, &
     &    item_bc_grp_rtp_IO)
!
      call deallocate_rtp_nod_grp_IO_item
!
      end subroutine write_rtp_node_grp_data
!
!------------------------------------------------------------------
!
      subroutine write_rtp_radial_grp_data(mesh_file_id)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)', advance='NO') hd_rgrphd()
!
      call write_group_data(mesh_file_id, num_radial_grp_rtp_IO,        &
     &    ntot_radial_grp_rtp_IO, istack_radial_grp_rtp_IO,             &
     &    name_radial_grp_rtp_IO, item_radial_grp_rtp_IO)
!
      call deallocate_rtp_r_grp_IO_item
!
      end subroutine write_rtp_radial_grp_data
!
!------------------------------------------------------------------
!
      subroutine write_rtp_theta_grp_data(mesh_file_id)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)', advance='NO') hd_tgrphd()
!
      call write_group_data(mesh_file_id, num_theta_grp_rtp_IO,         &
     &    ntot_theta_grp_rtp_IO, istack_theta_grp_rtp_IO,               &
     &    name_theta_grp_rtp_IO, item_theta_grp_rtp_IO)
!
      call deallocate_rtp_t_grp_IO_item
!
      end subroutine write_rtp_theta_grp_data
!
!------------------------------------------------------------------
!
      subroutine write_rtp_zonal_grp_data(mesh_file_id)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: mesh_file_id
!
!
      write(mesh_file_id,'(a)', advance='NO') hd_pgrphd()
!
      call write_group_data(mesh_file_id, num_zonal_grp_rtp_IO,         &
     &    ntot_zonal_grp_rtp_IO, istack_zonal_grp_rtp_IO,               &
     &    name_zonal_grp_rtp_IO, item_zonal_grp_rtp_IO)
!
      call deallocate_rtp_p_grp_IO_item
!
      end subroutine write_rtp_zonal_grp_data
!
!------------------------------------------------------------------
!
      end module sph_rtp_groups_IO
