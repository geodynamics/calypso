!>@file   t_ctl_data_magnetic_scale.f90
!!        module t_ctl_data_magnetic_scale
!!
!!@author H. Matsui
!!@date   Programmed in March, 2006
!!
!!
!> @brief Control data for magnetic field controls
!!
!!@verbatim
!!      subroutine init_magnetic_scale_ctl_label(hd_block, bscale_ctl)
!!      subroutine read_magnetic_scale_ctl                              &
!!     &         (id_control, hd_block, bscale_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_magnetic_scale_ctl                             &
!!     &         (id_control, hd_block, bscale_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(magnetic_field_scale_control), intent(in) :: bscale_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine dealloc_magnetic_scale_ctl(bscale_ctl)
!!        type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!!
!!!!!!!!!!  magnetic field normalization !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      mag_to_kin_energy_ratio ::   coefficients of ration of Em to Ek
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!      begin magnetic_field_scale_ctl
!!        array mag_to_kin_energy_ratio
!!          mag_to_kin_energy_ratio     magnetic_Prandtl     -1.0
!!          mag_to_kin_energy_ratio     Ekman                -1.0
!!        end array mag_to_kin_energy_ratio
!!      end magnetic_field_scale_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_ctl_data_magnetic_scale
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
!>      Structure for magnetic field scaling
      type magnetic_field_scale_control
!>        Block name
        character(len=kchara) :: block_name                             &
     &                          = 'magnetic_field_scale_ctl'
!>        array structure for magnetic energy ratio
!!@n        mag_to_kin_energy_ctl%c_tbl:  name of coefficients
!!@n        mag_to_kin_energy_ctl%vect:   order
        type(ctl_array_cr) :: mag_to_kin_energy_ctl
!
        integer (kind=kint) :: i_bscale_ctl =   0
      end type magnetic_field_scale_control
!
!   4th level for magnetic field scaling
      character(len=kchara), parameter, private                         &
     &        :: hd_mag_to_kin_ratio = 'mag_to_kin_energy_ratio'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_magnetic_scale_ctl                                &
     &         (id_control, hd_block, bscale_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(bscale_ctl%i_bscale_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control, hd_mag_to_kin_ratio,    &
     &      bscale_ctl%mag_to_kin_energy_ctl, c_buf)
      end do
      bscale_ctl%i_bscale_ctl = 1
!
      end subroutine read_magnetic_scale_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_magnetic_scale_ctl                               &
     &         (id_control, hd_block, bscale_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(magnetic_field_scale_control), intent(in) :: bscale_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(bscale_ctl%i_bscale_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c_r(id_control, level,                   &
     &    bscale_ctl%mag_to_kin_energy_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_magnetic_scale_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_magnetic_scale_ctl_label(hd_block, bscale_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!
      bscale_ctl%block_name = hd_block
        call init_c_r_ctl_array_label(hd_mag_to_kin_ratio,              &
     &      bscale_ctl%mag_to_kin_energy_ctl)
      end subroutine init_magnetic_scale_ctl_label
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_magnetic_scale_ctl(bscale_ctl)
!
      type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!
      call dealloc_control_array_c_r(bscale_ctl%mag_to_kin_energy_ctl)
      bscale_ctl%i_bscale_ctl = 0
!
      end subroutine dealloc_magnetic_scale_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_magnetic_scale
