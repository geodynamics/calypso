!>@file   t_ctl_data_mhd_magne.f90
!!        module t_ctl_data_mhd_magne
!!
!!@author H. Matsui
!!@date   Programmed in March, 2006
!!
!!
!> @brief Control data for magnetic field controls
!!
!!@verbatim
!!      subroutine read_magnetic_scale_ctl                              &
!!     &         (id_control, hd_block, bscale_ctl, c_buf)
!!      subroutine bcast_magnetic_scale_ctl(bscale_ctl)
!!      subroutine dealloc_magnetic_scale_ctl(bscale_ctl)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine read_magneto_ctl                                     &
!!     &         (id_control, hd_block, mcv_ctl, c_buf)
!!        type(magneto_convection_control), intent(inout) :: mcv_ctl
!!
!!      subroutine bcast_magneto_ctl(mcv_ctl)
!!      subroutine dealloc_magneto_ctl(mcv_ctl)
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
!!
!!!!!!!!!!  magnetoconvection model!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array ext_magne_vec:   0...off  more than 1...On
!!     ext_magne_vec: external field (constant)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin magnetic_induciton_ctl
!!      filtered_induction_ctl   Off
!!
!!      magneto_cv_ctl    On
!!      array ext_magne_vec   3
!!        ext_magne_vec  x     0.000   end
!!        ext_magne_vec  y     1.000   end
!!        ext_magne_vec  z     0.000   end
!!      end array ext_magne_vec
!!    end  magnetic_induciton_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_ctl_data_mhd_magne
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_charareal
      use calypso_mpi
      use skip_comment_f
      use bcast_control_arrays
!
      implicit  none
!
!
!>      Structure for magnetic field scaling
      type magnetic_field_scale_control
!>        array structure for magnetic energy ratio
!!@n        mag_to_kin_energy_ctl%c_tbl:  name of coefficients
!!@n        mag_to_kin_energy_ctl%vect:   order
        type(ctl_array_cr) :: mag_to_kin_energy_ctl
!
        integer (kind=kint) :: i_bscale_ctl =   0
      end type magnetic_field_scale_control
!
!>      Structure for external magnetic field
      type magneto_convection_control
!>        Structure for filtered induction flag
        type(read_character_item) :: filterd_induction_ctl
!>        Structure for magnetoconvection definition
        type(read_character_item) :: magneto_cv
!
!>        Structure for external magnetic field control
!!@n        ext_magne%c_tbl:  Direction of external magnetic field
!!@n        ext_magne%vect:   Amplitude of external magnetic field
        type(ctl_array_cr) :: ext_magne
!
        integer (kind=kint) :: i_magneto_ctl =   0
      end type magneto_convection_control
!
!   4th level for magnetic field scaling
      character(len=kchara), parameter, private                         &
     &        :: hd_mag_to_kin_ratio = 'mag_to_kin_energy_ratio'
!
!   4th level for external magnetic field
      character(len=kchara), parameter, private                         &
     &        :: hd_filetered_induction = 'filtered_induction_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_magneto_cv = 'magneto_cv_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_magne_vect = 'ext_magne_vec'
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
        call load_one_line_from_control(id_control, c_buf)
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
      subroutine read_magneto_ctl                                       &
     &         (id_control, hd_block, mcv_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(magneto_convection_control), intent(inout) :: mcv_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(mcv_ctl%i_magneto_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control, hd_magne_vect,          &
     &      mcv_ctl%ext_magne, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_magneto_cv, mcv_ctl%magneto_cv)
        call read_chara_ctl_type(c_buf, hd_filetered_induction,         &
     &      mcv_ctl%filterd_induction_ctl)
      end do
      mcv_ctl%i_magneto_ctl = 1
!
      end subroutine read_magneto_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_magnetic_scale_ctl(bscale_ctl)
!
      use calypso_mpi_int
!
      type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!
!
      call bcast_ctl_array_cr(bscale_ctl%mag_to_kin_energy_ctl)
      call calypso_mpi_bcast_one_int(bscale_ctl%i_bscale_ctl, 0)
!
      end subroutine bcast_magnetic_scale_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_magneto_ctl(mcv_ctl)
!
      use calypso_mpi_int
!
      type(magneto_convection_control), intent(inout) :: mcv_ctl
!
!
      call bcast_ctl_array_cr(mcv_ctl%ext_magne)
      call bcast_ctl_type_c1(mcv_ctl%magneto_cv)
      call bcast_ctl_type_c1(mcv_ctl%filterd_induction_ctl)
!
      call calypso_mpi_bcast_one_int(mcv_ctl%i_magneto_ctl, 0)
!
      end subroutine bcast_magneto_ctl
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
      subroutine dealloc_magneto_ctl(mcv_ctl)
!
      type(magneto_convection_control), intent(inout) :: mcv_ctl
!
!
      call dealloc_control_array_c_r(mcv_ctl%ext_magne)
      mcv_ctl%filterd_induction_ctl%iflag = 0
      mcv_ctl%magneto_cv%iflag =            0
      mcv_ctl%i_magneto_ctl =               0
!
      end subroutine dealloc_magneto_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mhd_magne
