!>@file   collect_fline_position.f90
!!@brief  module collect_fline_position
!!
!!@author  H. Matsui
!!@date Programmed on Aug., 2011
!
!> @brief MPI communication To collect field line data
!!
!!@verbatim
!!      subroutine s_collect_fline_data(istep_fline, fln_prm,           &
!!     &          fline_lc, fline_gl)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(global_fieldline_data), intent(inout) :: fline_gl
!!@endverbatim
!
      module collect_fline_data
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use t_global_fieldline
      use t_local_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_collect_fline_data(istep_fline, fln_prm,             &
     &          fline_lc, fline_gl)
!
      use t_control_params_4_fline
      use m_field_file_format
      use set_ucd_file_names
      use set_parallel_file_name
      use set_ucd_extensions
      use collect_fline_connectivity
      use collect_fline_position
!
      integer(kind = kint), intent(in) :: istep_fline
      type(fieldline_paramter), intent(in) :: fln_prm
      type(local_fieldline), intent(in) :: fline_lc
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      character(len=kchara) :: file_name
!
!
      fline_gl%color_name_gl = fln_prm%name_color_output
      call collect_number_of_fline(fline_lc, fline_gl)
!
      if(fline_gl%ntot_nod_line_gl                                      &
     &        .gt. fline_gl%ntot_nod_line_gl_buf) then
        call raise_global_fline_data(fline_gl)
      end if
!
      if(fline_gl%ntot_ele_line_gl                                      &
     &       .gt. fline_gl%ntot_ele_line_gl_buf) then
        call raise_global_fline_connect(fline_gl)
      end if
!
      call collect_fline_connection(fline_lc, fline_gl)
      call s_collect_fline_position(fline_lc, fline_gl)
      call collect_fline_color(fline_lc, fline_gl)
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'output format ', fln_prm%iformat_file_file
        if(fln_prm%iformat_file_file .eq. iflag_ucd) then
          file_name = set_single_ucd_file_name(fln_prm%fline_prefix,    &
     &               fln_prm%iformat_file_file, istep_fline)
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline_ucd(id_fline_data_code, fline_gl)
          close(id_fline_data_code)
        else if(fln_prm%iformat_file_file .eq. iflag_vtk)               &
     &        then
          file_name = set_single_ucd_file_name(fln_prm%fline_prefix,    &
     &               fln_prm%iformat_file_file, istep_fline)
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline_vtk(id_fline_data_code, fline_gl)
          close(id_fline_data_code)
        else
          file_name = add_int_suffix(istep_fline, fln_prm%fline_prefix)
          file_name = add_dx_extension(file_name)
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline_dx(id_fline_data_code, fline_gl)
          close(id_fline_data_code)
        end if
      end if
!
      end subroutine s_collect_fline_data
!
!  ---------------------------------------------------------------------
!
      end module collect_fline_data
