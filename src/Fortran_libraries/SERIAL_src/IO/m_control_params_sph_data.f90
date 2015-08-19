!> @file  m_control_params_sph_data.f90
!!      module m_control_params_sph_data
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2009
!
!> @brief File prefix for spectrum data
!!
!!@verbatim
!!      subroutine set_spectr_prefix_fmt_2_fld_IO(fld_IO)
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module m_control_params_sph_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>      Output flag for spherical harmonics coefficients data
      integer(kind = kint) :: iflag_sph_spec_output
!
!>      file header for spherical harmonics coefficients data
      character(len=kchara) :: spectr_file_head
!>      file header for spherical harmonics coefficients data
      integer(kind = kint) ::  iflag_sph_spectr_fmt =     0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_spectr_prefix_fmt_2_fld_IO(fld_IO)
!
      use t_field_data_IO
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call set_field_file_fmt_prefix                                    &
     &   (iflag_sph_spectr_fmt, spectr_file_head, fld_IO)
!
      end subroutine set_spectr_prefix_fmt_2_fld_IO
!
! ----------------------------------------------------------------------
!
      end module m_control_params_sph_data
