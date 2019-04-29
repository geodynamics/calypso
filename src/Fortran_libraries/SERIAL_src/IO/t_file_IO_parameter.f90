!> @file  t_file_IO_parameter.f90
!!      module t_file_IO_parameter
!!
!! @author  H. Matsui
!! @date Programmed in June, 2013
!
!> @brief Structure for file information (prefix, format, IO flag)
!!
!!@verbatim
!!      subroutine copy_file_params_type(org_param, new_param)
!!      subroutine copy_mesh_format_and_prefix(prefix, i_fmt, f_param)
!!        type(field_IO_params), intent(inout) :: f_param
!!      subroutine set_file_fmt_prefix(iflag_fmt, file_head, f_param)
!!        type(field_IO_params), intent(inout) :: f_param
!!@endverbatim
!
      module t_file_IO_parameter
!
      use m_precision
!
      implicit none
!
!
!>      Structure for field data IO paramters
      type field_IO_params
!>        Output flag for spherical harmonics coefficients data
        integer(kind = kint) :: iflag_IO = 0
!>        file header for spherical harmonics coefficients data
        character(len=kchara) :: file_prefix
!>        file header for spherical harmonics coefficients data
        integer(kind = kint) ::  iflag_format = 0
      end type field_IO_params
!
!>      FEM mesh IO flags
      type FEM_file_IO_flags
!>        Integer flag to output mesh data
        integer(kind = kint) :: iflag_access_FEM = 0
!>        Integer flag to output surface and edge mesh data
        integer(kind = kint) :: iflag_output_SURF = 0
!>        Integer flag to output mesh data for viewer
        integer(kind = kint) :: iflag_output_VMESH = 0
      end type FEM_file_IO_flags
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_file_params_type(org_param, new_param)
!
      type(field_IO_params), intent(in) :: org_param
      type(field_IO_params), intent(inout) :: new_param
!
!
      new_param%file_prefix =  org_param%file_prefix
      new_param%iflag_format = org_param%iflag_format
      new_param%iflag_IO =     org_param%iflag_IO
!
      end subroutine copy_file_params_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mesh_format_and_prefix(prefix, i_fmt, f_param)
!
      character(len=kchara), intent(in) :: prefix
      integer(kind = kint), intent(in)  :: i_fmt
      type(field_IO_params), intent(inout) :: f_param
!
!
      f_param%file_prefix =  prefix
      f_param%iflag_format = i_fmt
      f_param%iflag_IO =     i_fmt
!
      end subroutine copy_mesh_format_and_prefix
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_file_fmt_prefix(iflag_fmt, file_head, f_param)
!
      integer(kind = kint), intent(in) :: iflag_fmt
      character(len=kchara), intent(in) :: file_head
      type(field_IO_params), intent(inout) :: f_param
!
      f_param%iflag_format = iflag_fmt
      write(f_param%file_prefix,'(a)') trim(file_head)
!
      end subroutine set_file_fmt_prefix
!
! -------------------------------------------------------------------
!
      end module t_file_IO_parameter
