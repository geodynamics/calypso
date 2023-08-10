!>@file   t_ctl_data_4_FEM_mesh.f90
!!        module t_ctl_data_4_FEM_mesh
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine init_FEM_mesh_ctl_label(hd_block, Fmesh_ctl)
!!      subroutine read_FEM_mesh_control                                &
!!     &         (id_control, hd_block, Fmesh_ctl, c_buf)
!!      subroutine write_FEM_mesh_control(id_control, Fmesh_ctl, level)
!!      subroutine reset_FEM_mesh_control(Fmesh_ctl)
!!        type(FEM_mesh_control), intent(inout) :: Fmesh_ctl
!!      subroutine copy_FEM_mesh_control(org_Fmesh_c, new_Fmesh_c)
!!        type(FEM_mesh_control), intent(in) :: org_Fmesh_c
!!        type(FEM_mesh_control), intent(inout) :: new_Fmesh_c
!!
!! ------------------------------------------------------------------
!!      Example of control parameters
!!
!!    begin FEM_mesh_ctl
!!      memory_conservation_ctl        'YES'
!!      FEM_mesh_output_switch         'NO'
!!      FEM_surface_output_switch      'NO'
!!      FEM_viewer_mesh_output_switch  'NO'
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
      use t_control_array_character
      use t_control_array_integer
!
      implicit  none
!
!
!>      Structure of mesh IO controls and sleeve informations
      type FEM_mesh_control
!>        Block name
        character(len=kchara) :: block_name = 'FEM_mesh_ctl'
!
        type(read_character_item) :: memory_conservation_ctl
        type(read_character_item) :: FEM_mesh_output_switch
        type(read_character_item) :: FEM_surface_output_switch
        type(read_character_item) :: FEM_viewer_output_switch
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
      if(Fmesh_ctl%i_FEM_mesh .gt. 0) return
      Fmesh_ctl%block_name = hd_block
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_mem_conserve,                &
     &      Fmesh_ctl%memory_conservation_ctl)
        call read_chara_ctl_type(c_buf, hd_FEM_mesh_output,             &
     &      Fmesh_ctl%FEM_mesh_output_switch)
        call read_chara_ctl_type(c_buf, hd_FEM_surf_output,             &
     &      Fmesh_ctl%FEM_surface_output_switch)
        call read_chara_ctl_type(c_buf, hd_FEM_viewer_output,           &
     &      Fmesh_ctl%FEM_viewer_output_switch)
       end do
       Fmesh_ctl%i_FEM_mesh = 1
!
      end subroutine read_FEM_mesh_control
!
!  ---------------------------------------------------------------------
!
      subroutine write_FEM_mesh_control(id_control, Fmesh_ctl, level)
!
      use m_machine_parameter
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(Fmesh_ctl%i_FEM_mesh .le. 0) return
!
      maxlen = len_trim(hd_mem_conserve)
      maxlen = max(maxlen, len_trim(hd_FEM_mesh_output))
      maxlen = max(maxlen, len_trim(hd_FEM_surf_output))
      maxlen = max(maxlen, len_trim(hd_FEM_viewer_output))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 Fmesh_ctl%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    Fmesh_ctl%memory_conservation_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    Fmesh_ctl%FEM_mesh_output_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    Fmesh_ctl%FEM_surface_output_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    Fmesh_ctl%FEM_viewer_output_switch)
!
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                Fmesh_ctl%block_name)
!
      end subroutine write_FEM_mesh_control
!
!  ---------------------------------------------------------------------
!
      subroutine init_FEM_mesh_ctl_label(hd_block, Fmesh_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(FEM_mesh_control), intent(inout) :: Fmesh_ctl
!
!
      Fmesh_ctl%block_name = hd_block
!
        call init_chara_ctl_item_label(hd_mem_conserve,                 &
     &      Fmesh_ctl%memory_conservation_ctl)
        call init_chara_ctl_item_label(hd_FEM_mesh_output,              &
     &      Fmesh_ctl%FEM_mesh_output_switch)
        call init_chara_ctl_item_label(hd_FEM_surf_output,              &
     &      Fmesh_ctl%FEM_surface_output_switch)
        call init_chara_ctl_item_label(hd_FEM_viewer_output,            &
     &      Fmesh_ctl%FEM_viewer_output_switch)
!
      end subroutine init_FEM_mesh_ctl_label
!
!  ---------------------------------------------------------------------
!
      subroutine reset_FEM_mesh_control(Fmesh_ctl)
!
      type(FEM_mesh_control), intent(inout) :: Fmesh_ctl
!
!
      Fmesh_ctl%memory_conservation_ctl%iflag =   0
      Fmesh_ctl%FEM_mesh_output_switch%iflag =    0
      Fmesh_ctl%FEM_surface_output_switch%iflag = 0
      Fmesh_ctl%FEM_viewer_output_switch%iflag =  0
!
      Fmesh_ctl%i_FEM_mesh = 0
!
      end subroutine reset_FEM_mesh_control
!
!  ---------------------------------------------------------------------
!
      subroutine copy_FEM_mesh_control(org_Fmesh_c, new_Fmesh_c)
!
      type(FEM_mesh_control), intent(in) :: org_Fmesh_c
      type(FEM_mesh_control), intent(inout) :: new_Fmesh_c
!
!
      call copy_chara_ctl(org_Fmesh_c%memory_conservation_ctl,          &
     &                    new_Fmesh_c%memory_conservation_ctl)
      call copy_chara_ctl(org_Fmesh_c%FEM_mesh_output_switch,           &
     &                    new_Fmesh_c%FEM_mesh_output_switch)
      call copy_chara_ctl(org_Fmesh_c%FEM_surface_output_switch,        &
     &                    new_Fmesh_c%FEM_surface_output_switch)
      call copy_chara_ctl(org_Fmesh_c%FEM_viewer_output_switch,         &
     &                    new_Fmesh_c%FEM_viewer_output_switch)
!
      new_Fmesh_c%i_FEM_mesh = org_Fmesh_c%i_FEM_mesh
!
      end subroutine copy_FEM_mesh_control
!
!  ---------------------------------------------------------------------
!
      end module  t_ctl_data_4_FEM_mesh
