!>@file   t_ctl_data_mhd_forces.f90
!!        module t_ctl_data_mhd_forces
!!
!!@author H. Matsui
!!@date   Programmed in March, 2006
!!
!!
!> @brief Control data for magnetic field controls
!!
!!@verbatim
!!      subroutine read_forces_ctl(id_control, hd_block, frc_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(forces_control), intent(inout) :: frc_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_forces_ctl                                     &
!!     &         (id_control, hd_block, frc_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(forces_control), intent(in) :: frc_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_name_force_ctl(frc_ctl)
!!        type(forces_control), intent(inout) :: frc_ctl
!!
!!    begin forces_define
!!!!!  define of forces !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  available forces
!!     gravity, Coriolis, Lorentz, Composite_gravity
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array force_ctl      4
!!        force_ctl   gravity              end
!!        force_ctl   Coriolis             end
!!        force_ctl   Lorentz              end
!!        force_ctl   Composite_gravity    end
!!      end array
!!    end  forces_define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_mhd_forces
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_charareal
      use skip_comment_f
!
      implicit  none
!
!
!>      Structure for force list
      type forces_control
!>        Structure for constant force list
!!@n        force_names%c_tbl: Name of force
        type(ctl_array_chara) :: force_names
!
        integer (kind=kint) :: i_forces_ctl =    0
      end type forces_control
!
!   4th level for forces
      character(len=kchara), parameter, private                         &
     &        :: hd_num_forces =  'force_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_forces_ctl(id_control, hd_block, frc_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(forces_control), intent(inout) :: frc_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(frc_ctl%i_forces_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control, hd_num_forces,           &
     &      frc_ctl%force_names, c_buf)
      end do
      frc_ctl%i_forces_ctl = 1
!
      end subroutine read_forces_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_forces_ctl                                       &
     &         (id_control, hd_block, frc_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(forces_control), intent(in) :: frc_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(frc_ctl%i_forces_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c1(id_control, level,                    &
     &                            hd_num_forces, frc_ctl%force_names)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_forces_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_name_force_ctl(frc_ctl)
!
      type(forces_control), intent(inout) :: frc_ctl
!
!
      call dealloc_control_array_chara(frc_ctl%force_names)
      frc_ctl%i_forces_ctl = 0
!
      end subroutine dealloc_name_force_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_mhd_forces
