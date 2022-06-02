!>@file   t_ctl_data_dimless_numbers.f90
!!@brief  module t_ctl_data_dimless_numbers
!!
!!@author H. Matsui
!>@brief   Control for dimensionless numbers of MHD dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Merch, 2006
!!
!!@verbatim
!!      subroutine read_dimless_ctl                                     &
!!     &         (id_control, hd_block, dless_ctl, c_buf)
!!      subroutine dealloc_dimless_ctl(dless_ctl)
!!        type(dimless_control), intent(inout) :: dless_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!   --------------------------------------------------------------------
!!    example
!!
!!!!!!  dimensionless numbers !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  available numbers
!!     Prandtl_number, magnetic_Prandtl_number
!!     Rayleigh_number, modified_Rayleigh_number
!!     Composit_Rayleigh_number
!!     Reynords_number
!!     Taylor_number, Ekman_number
!!     Elsasser_number
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin dimensionless_ctl
!!      array dimless_ctl
!!        dimless_ctl  Prandtl_number                   1.0e-0
!!        dimless_ctl  modified_Rayleigh_number         1.0E+2
!!        dimless_ctl  Ekman_number                     1.0e-3
!!        dimless_ctl  magnetic_Prandtl_number          5.0e+0
!!        dimless_ctl  Composite_Rayleigh_number        1.0E+2
!!        dimless_ctl  Composite_Prandtl_number         1.0E+2
!!      end array dimless_ctl
!!    end  dimensionless_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_dimless_numbers
!
      use m_precision
!
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_charareal
!
      implicit  none
!
!
!>        Structure for list of dimensionless numbers
      type dimless_control
!>        Structure for list of dimensionless numbers
!!@n        dimless%c_tbl:  Name of each number 
!!@n        dimless%vect:   valus of each number
        type(ctl_array_cr) :: dimless
!
        integer (kind=kint) :: i_dimless_ctl =   0
      end type dimless_control
!
!   4th level for dimensionless numbers
!
      character(len=kchara), parameter :: hd_dimless =  'dimless_ctl'
      private :: hd_dimless
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_dimless_ctl                                       &
     &         (id_control, hd_block, dless_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(dimless_control), intent(inout) :: dless_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(dless_ctl%i_dimless_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control,                         &
     &      hd_dimless, dless_ctl%dimless, c_buf)
      end do
      dless_ctl%i_dimless_ctl = 1
!
      end subroutine read_dimless_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_dimless_ctl(dless_ctl)
!
      type(dimless_control), intent(inout) :: dless_ctl
!
!
      call dealloc_control_array_c_r(dless_ctl%dimless)
      dless_ctl%i_dimless_ctl = 0
!
      end subroutine dealloc_dimless_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_dimless_numbers
