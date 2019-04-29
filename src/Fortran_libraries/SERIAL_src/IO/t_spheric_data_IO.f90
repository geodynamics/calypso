!>@file   t_spheric_data_IO.f90
!!@brief  module t_spheric_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure for grid and comm table for spherical transform
!!
!!@verbatim
!!      subroutine alloc_multi_mesh_data_IO(nloop, sph_file)
!!      subroutine dealloc_multi_mesh_data_IO(sph_file)
!!@endverbatim
!
      module t_spheric_data_IO
!
      use m_precision
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_node_id_spherical_IO
      use t_comm_table
!
      implicit none
!
!
!> Structure of spherical transform mesh IO
      type sph_file_data_type
!>        Structure for spherical harmonics table IO
        type(sph_IO_data) :: sph_IO
!>        data structure for communication table IO
        type(communication_table) :: comm_IO
!
!>        Structure of group data for spherical transform
        type(sph_group_data) :: sph_grp_IO
      end type sph_file_data_type
!
!
!> Structure of group data for spherical transform
      type mul_sph_group_data
!>         node group for grid space
        type(group_data), pointer :: bc_rtp_grp(:)
!>         radial group for grid space
        type(group_data), pointer :: radial_rtp_grp(:)
!>         meridional group for grid space
        type(group_data), pointer :: theta_rtp_grp(:)
!>         zonal group for grid space
        type(group_data), pointer :: zonal_rtp_grp(:)
!
!>         radial group for sprctrum space
        type(group_data), pointer :: radial_rj_grp(:)
!>         spherical harmonics group for sprctrum space
        type(group_data), pointer :: sphere_rj_grp(:)
      end type mul_sph_group_data
!
!> Structure of spherical transform mesh IO for multiple domains
      type multi_sph_file_data
!>      Number of subdomains in each process
        integer(kind = kint) :: nloop_IO
!
!>        Structure for spherical harmonics table IO
        type(sph_IO_data), pointer :: sph_IO(:)
!>        data structure for communication table IO
        type(communication_table), pointer :: comm_IO(:)
!
!>        Structure of group data for spherical transform
        type(mul_sph_group_data), pointer :: sph_grp_IO
      end type multi_sph_file_data
!
      private :: alloc_multi_sph_group_IO
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine dealloc_rtp_grid_IO(sph_IO)
!
      type(sph_file_data_type), intent(inout) :: sph_IO
!
      call dealloc_import_table(sph_IO%comm_IO)
      call dealloc_sph_grid_idx_IO(sph_IO%sph_IO)
      call dealloc_sph_grid_group(sph_IO%sph_grp_IO)
!
      end subroutine dealloc_rtp_grid_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_rtm_grid_IO(sph_IO)
!
      type(sph_file_data_type), intent(inout) :: sph_IO
!
      call dealloc_import_table(sph_IO%comm_IO)
      call dealloc_sph_grid_idx_IO(sph_IO%sph_IO)
!
      end subroutine dealloc_rtm_grid_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_rlm_mode_IO(sph_IO)
!
      type(sph_file_data_type), intent(inout) :: sph_IO
!
      call dealloc_import_table(sph_IO%comm_IO)
      call dealloc_sph_mode_idx_IO(sph_IO%sph_IO)
!
      end subroutine dealloc_rlm_mode_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_rj_mode_IO(sph_IO)
!
      type(sph_file_data_type), intent(inout) :: sph_IO
!
      call dealloc_import_table(sph_IO%comm_IO)
      call dealloc_sph_mode_idx_IO(sph_IO%sph_IO)
      call dealloc_sph_mode_group(sph_IO%sph_grp_IO)
!
      end subroutine dealloc_rj_mode_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_multi_mesh_data_IO(nloop, sph_file)
!
      integer(kind = kint), intent(in) :: nloop
      type(multi_sph_file_data), intent(inout) :: sph_file
!
      sph_file%nloop_IO = nloop
      allocate(sph_file%sph_IO(sph_file%nloop_IO))
      allocate(sph_file%comm_IO(sph_file%nloop_IO))
!
      call alloc_multi_sph_group_IO                                     &
     &   (sph_file%nloop_IO, sph_file%sph_grp_IO)
!
      end subroutine alloc_multi_mesh_data_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_multi_mesh_data_IO(sph_file)
!
      type(multi_sph_file_data), intent(inout) :: sph_file
!
      deallocate(sph_file%sph_IO)
      deallocate(sph_file%comm_IO)
!
      call dealloc_multi_sph_group_IO(sph_file%sph_grp_IO)
!
      end subroutine dealloc_multi_mesh_data_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_multi_sph_group_IO(nloop, sph_grp_IO)
!
      integer(kind = kint), intent(in) :: nloop
      type(mul_sph_group_data), intent(inout) :: sph_grp_IO
!
!
      allocate(sph_grp_IO%bc_rtp_grp(nloop))
      allocate(sph_grp_IO%radial_rtp_grp(nloop))
      allocate(sph_grp_IO%theta_rtp_grp(nloop))
      allocate(sph_grp_IO%zonal_rtp_grp(nloop))
!
      allocate(sph_grp_IO%radial_rj_grp(nloop))
      allocate(sph_grp_IO%sphere_rj_grp(nloop))
!
      end subroutine alloc_multi_sph_group_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_multi_sph_group_IO(sph_grp_IO)
!
      type(mul_sph_group_data), intent(inout) :: sph_grp_IO
!
!
      deallocate(sph_grp_IO%bc_rtp_grp)
      deallocate(sph_grp_IO%radial_rtp_grp)
      deallocate(sph_grp_IO%theta_rtp_grp)
      deallocate(sph_grp_IO%zonal_rtp_grp)
!
      deallocate(sph_grp_IO%radial_rj_grp)
      deallocate(sph_grp_IO%sphere_rj_grp)
!
      end subroutine dealloc_multi_sph_group_IO
!
!------------------------------------------------------------------
!
      end module t_spheric_data_IO
