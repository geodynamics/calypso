!>@file   t_ctl_data_4_FEM_mesh.f90
!!@brief  module t_ctl_data_4_FEM_mesh
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine read_FEM_mesh_control(hd_block, iflag, Fmesh_ctl)
!!        type(FEM_mesh_control), intent(inout) :: Fmesh_ctl
!!      subroutine write_FEM_mesh_control                               &
!!     &         (id_file, hd_block, Fmesh_ctl, level)
!!
!! ------------------------------------------------------------------
!!      Example of control parameters
!!
!!    begin FEM_mesh_ctl
!!      memory_conservation_ctl        'YES'
!!      FEM_mesh_output_switch         'NO'
!!      FEM_surface_output_switch      'NO'
!!      FEM_viewer_mesh_output_switch  'NO'
!!
!!      sleeve_level_ctl            2
!!      element_overlap_ctl        ON
!!    end FEM_mesh_ctl
!! ------------------------------------------------------------------
!!@endverbatim
!!
!>@n@param      memory_conservation_ctl    memory conservation flag
!>@n@param      FEM_mesh_output_switch    FEM mesh output flag
!>@n@param      FEM_surface_output_switch  FEM surface mesh output flag
!>@n@param      FEM_viewer_mesh_output_switch   Viewer mesh output flag
!>@n@param      sleeve_level_ctl          sleeve size flag
!
      module t_ctl_data_4_FEM_mesh
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
!
!>      Structure of mesh IO controls and sleeve informations
      type FEM_mesh_control
        type(read_character_item) :: memory_conservation_ctl
        type(read_character_item) :: FEM_mesh_output_switch
        type(read_character_item) :: FEM_surface_output_switch
        type(read_character_item) :: FEM_viewer_output_switch
!
        type(read_integer_item) ::   FEM_sleeve_level_ctl
        type(read_character_item) :: FEM_element_overlap_ctl
      end type FEM_mesh_control
!
!   file and domain controls
!
      character(len=kchara), parameter, private                         &
     &       :: hd_mem_conserve =   'memory_conservation_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_FEM_mesh_output = 'FEM_mesh_output_switch'
      character(len=kchara), parameter, private                         &
     &       :: hd_FEM_surf_output = 'FEM_surface_output_switch'
      character(len=kchara), parameter, private                         &
     &       :: hd_FEM_viewer_output = 'FEM_viewer_mesh_output_switch'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_sleeve_level =  'sleeve_level_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_ele_overlap =   'element_overlap_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_FEM_mesh_control(hd_block, iflag, Fmesh_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(FEM_mesh_control), intent(inout) :: Fmesh_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if(iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        iflag =  find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_mem_conserve,                       &
     &      Fmesh_ctl%memory_conservation_ctl)
        call read_chara_ctl_type(hd_FEM_mesh_output,                    &
     &      Fmesh_ctl%FEM_mesh_output_switch)
        call read_chara_ctl_type(hd_FEM_surf_output,                    &
     &      Fmesh_ctl%FEM_surface_output_switch)
        call read_chara_ctl_type(hd_FEM_viewer_output,                  &
     &      Fmesh_ctl%FEM_viewer_output_switch)
!
        call read_integer_ctl_type                                      &
     &     (hd_sleeve_level, Fmesh_ctl%FEM_sleeve_level_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_ele_overlap, Fmesh_ctl%FEM_element_overlap_ctl)
       end do
!
      end subroutine read_FEM_mesh_control
!
!  ---------------------------------------------------------------------
!
      subroutine write_FEM_mesh_control                                 &
     &         (id_file, hd_block, Fmesh_ctl, level)
!
      use m_machine_parameter
      use m_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: hd_block
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
      maxlen = max(maxlen, len_trim(hd_mem_conserve))
      maxlen = max(maxlen, len_trim(hd_FEM_mesh_output))
      maxlen = max(maxlen, len_trim(hd_FEM_surf_output))
      maxlen = max(maxlen, len_trim(hd_FEM_viewer_output))
      maxlen = max(maxlen, len_trim(hd_sleeve_level))
      maxlen = max(maxlen, len_trim(hd_ele_overlap))
!
      write(id_file,'(a)') '!'
      call write_chara_ctl_type(id_file, level, maxlen,                 &
     &    hd_mem_conserve, Fmesh_ctl%memory_conservation_ctl)
      call write_chara_ctl_type(id_file, level, maxlen,                 &
     &    hd_FEM_mesh_output, Fmesh_ctl%FEM_mesh_output_switch)
      call write_chara_ctl_type(id_file, level, maxlen,                 &
     &    hd_FEM_surf_output, Fmesh_ctl%FEM_surface_output_switch)
      call write_chara_ctl_type(id_file, level, maxlen,                 &
     &    hd_FEM_viewer_output, Fmesh_ctl%FEM_viewer_output_switch)
!
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_sleeve_level, Fmesh_ctl%FEM_sleeve_level_ctl)
      call write_chara_ctl_type(id_file, level, maxlen,                 &
     &    hd_ele_overlap, Fmesh_ctl%FEM_element_overlap_ctl)
!
      level =  write_end_flag_for_ctl(id_file, level, hd_block)
!
      end subroutine write_FEM_mesh_control
!
!  ---------------------------------------------------------------------
!
      end module  t_ctl_data_4_FEM_mesh
