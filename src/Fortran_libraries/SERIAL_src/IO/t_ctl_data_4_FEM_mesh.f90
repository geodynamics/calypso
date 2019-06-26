!>@file   t_ctl_data_4_FEM_mesh.f90
!!@brief  module t_ctl_data_4_FEM_mesh
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine read_FEM_mesh_control                                &
!!     &         (id_control, hd_block, Fmesh_ctl, c_buf)
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
!
        integer(kind=kint) :: i_FEM_mesh =   0
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
      subroutine read_FEM_mesh_control                                  &
     &         (id_control, hd_block, Fmesh_ctl, c_buf)
!
      use m_machine_parameter
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(FEM_mesh_control), intent(inout) :: Fmesh_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(Fmesh_ctl%i_FEM_mesh .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_mem_conserve,                &
     &      Fmesh_ctl%memory_conservation_ctl)
        call read_chara_ctl_type(c_buf, hd_FEM_mesh_output,             &
     &      Fmesh_ctl%FEM_mesh_output_switch)
        call read_chara_ctl_type(c_buf, hd_FEM_surf_output,             &
     &      Fmesh_ctl%FEM_surface_output_switch)
        call read_chara_ctl_type(c_buf, hd_FEM_viewer_output,           &
     &      Fmesh_ctl%FEM_viewer_output_switch)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_sleeve_level, Fmesh_ctl%FEM_sleeve_level_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ele_overlap, Fmesh_ctl%FEM_element_overlap_ctl)
       end do
       Fmesh_ctl%i_FEM_mesh = 1
!
      end subroutine read_FEM_mesh_control
!
!  ---------------------------------------------------------------------
!
      subroutine write_FEM_mesh_control                                 &
     &         (id_file, hd_block, Fmesh_ctl, level)
!
      use m_machine_parameter
      use t_read_control_elements
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
