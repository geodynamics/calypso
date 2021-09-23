!>@file   stop_by_missing_zlib.F90
!!        module stop_by_missing_zlib
!!
!!@author H. Matsui
!!@date Programmed in 2009
!!
!>@brief    Load file definitions from control structures
!!
!!@verbatim
!!      subroutine s_stop_by_missing_zlib(file_prefix, id_file_fmt)
!!        character(len=kchara), intent(in) :: file_prefix
!!        integer(kind= kint), intent(inout) :: id_file_fmt
!!@endverbatim
!!
!!@param id_rank  preocess ID
!
      module stop_by_missing_zlib
!
      use m_precision
      use m_constants
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_stop_by_missing_zlib(file_prefix, id_file_fmt)
!
      use m_machine_parameter
      use m_error_IDs
      use m_file_format_switch
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind= kint), intent(inout) :: id_file_fmt
!
!
#ifndef ZLIB_IO
      if     (id_file_fmt .eq. id_gzip_txt_file_fmt                     &
     &   .or. id_file_fmt .eq. id_gzip_bin_file_fmt                     &
     &   .or. id_file_fmt .eq. (id_gzip_txt_file_fmt+iflag_single)      &
     &   .or. id_file_fmt .eq. (id_gzip_bin_file_fmt+iflag_single))     &
     &   then
        id_file_fmt = id_missing_zlib
        write(*,*) 'Zlib is not linked!'
      end if
#endif
!
      if(id_file_fmt .ne. id_missing_zlib) return
        write(e_message,'(2a)') 'Failed file prefix: ',                 &
     &                         trim(file_prefix)
      stop
!
      end subroutine s_stop_by_missing_zlib
!
! -----------------------------------------------------------------------
!
      subroutine stop_by_no_zlib_in_ucd(file_prefix, id_file_fmt)
!
      use m_machine_parameter
      use m_error_IDs
      use m_file_format_switch
      use m_field_file_format
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind= kint), intent(inout) :: id_file_fmt
!
!
#ifndef ZLIB_IO
      if     (id_file_fmt .eq. (iflag_fld + iflag_gzip)                 &
     &   .or. id_file_fmt .eq. iflag_bin_gz                             &
     &   .or. id_file_fmt .eq. (iflag_udt + iflag_gzip)                 &
     &   .or. id_file_fmt .eq. (iflag_ucd + iflag_gzip)                 &
     &   .or. id_file_fmt .eq. (iflag_vtd + iflag_gzip)                 &
     &   .or. id_file_fmt .eq. (iflag_vtk + iflag_gzip)                 &
     &   .or. id_file_fmt .eq. (iflag_ucd_bin + iflag_gzip)             &
     &   .or. id_file_fmt .eq. (iflag_udt_bin + iflag_gzip)             &
     &   .or. id_file_fmt .eq. iflag_sgl_bin_gz                         &
     &   .or. id_file_fmt .eq. iflag_sgl_udt_gz                         &
     &   .or. id_file_fmt .eq. iflag_sgl_ucd_gz                         &
     &   .or. id_file_fmt .eq. iflag_sgl_vtd_gz                         &
     &   .or. id_file_fmt .eq. iflag_sgl_vtk_gz                         &
     &   .or. id_file_fmt .eq. iflag_sgl_udt_bin_gz                     &
     &   .or. id_file_fmt .eq. iflag_sgl_ucd_bin_gz) then
        id_file_fmt = id_missing_zlib
        write(*,*) 'Zlib is not linked!'
      end if
#endif
!
      if(id_file_fmt .ne. id_missing_zlib) return
        write(e_message,'(2a)') 'Failed file prefix: ',                 &
     &                         trim(file_prefix)
      stop
!
      end subroutine stop_by_no_zlib_in_ucd
!
! -----------------------------------------------------------------------
!
      end module stop_by_missing_zlib
