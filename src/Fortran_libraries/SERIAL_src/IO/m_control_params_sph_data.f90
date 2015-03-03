!m_control_params_sph_data.f90
!      module m_control_params_sph_data
!
!        programmed by H.Matsui on Nov., 2009
!
!      subroutine set_spectr_prefix_fmt_2_fld_IO
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
      subroutine set_spectr_prefix_fmt_2_fld_IO
!
      use m_field_data_IO
!
      call set_field_file_fmt_prefix                                    &
     &   (iflag_sph_spectr_fmt, spectr_file_head)
!
      end subroutine set_spectr_prefix_fmt_2_fld_IO
!
! ----------------------------------------------------------------------
!
      end module m_control_params_sph_data
