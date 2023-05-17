!>@file   m_field_file_format.f90
!!@brief  module m_field_file_format
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief Integer flags for field data file format
!!
!!
!!@verbatim
!!      integer(kind = kint) function choose_para_fld_file_format       &
!!     &                            (file_fmt_ctl, i_file_fmt)
!!      integer(kind = kint) function choose_ucd_file_format            &
!!     &                            (file_fmt_ctl, i_file_fmt)
!!
!! ------------------------------------------------------------------
!!   flag lists for field data
!!
!!  Distributed data file
!!   ASCII   data file
!!     UCD file:             ucd
!!     splitted UCD file:    udt
!!     VTK file:             vtk
!!     splitted VTK file:    vtd
!!   BINARY  data file
!!     splitted file:        binary
!!
!!   GZIPPED data file
!!     UCD file:             ucd_gzip
!!     splitted UCD file:    udt_gzip
!!     VTK file:             vtk_gzip
!!     splitted VTK file:    vtd_gzip
!!
!!  Merged data file
!!   ASCII   data file
!!     UCD file:             merged_ucd
!!     splitted UCD file:    merged_udt
!!     VTK file:             merged_vtk
!!     splitted VTK file:    merged_vtd
!!     HDF5 file:            HDF5
!!
!!   GZIPPED data file
!!     UCD file:             merged_ucd_gzip
!!     splitted UCD file:    merged_udt_gzip
!!     VTK file:             merged_vtk_gzip
!!     splitted VTK file:    merged_vtd_gzip
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param file_fmt_ctl   file format name (see above)
!!@n @param i_file_fmt     integer flag if file format is read
!!@n @param id_field_file_format   integer flag for file format
!
      module m_field_file_format
!
      use m_precision
      use m_file_format_switch
      use m_field_file_format_labels
      use m_merged_field_fmt_labels
      use t_multi_flag_labels
!
      implicit none
!
!>      Integer flag for origianl ascii data format
      integer(kind = kint), parameter :: iflag_ascii                    &
     &                     = id_ascii_file_fmt
!>      Integer flag for origianl binary data format
      integer(kind = kint), parameter :: iflag_bin                      &
     &                     = id_binary_file_fmt
!>      Integer flag for origianl gzipped ascii data format
      integer(kind = kint), parameter :: iflag_gzip                     &
     &                     = id_gzip_txt_file_fmt
!>      Integer flag for gzipped binary data
      integer(kind = kint), parameter :: iflag_bin_gz                   &
     &                     = id_gzip_bin_file_fmt
!
!>      Integer flag for origianl ascii data format
      integer(kind = kint), parameter :: iflag_fld =       0
!>      Integer flag for UCD data
      integer(kind = kint), parameter :: iflag_ucd =      10
!>      Integer flag for UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_udt =      20
!>      Integer flag for VTK data
      integer(kind = kint), parameter :: iflag_vtk =      30
!>      Integer flag for VTK data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_vtd =      40
!
!>      Integer flag for binary surface data
      integer(kind = kint), parameter :: iflag_ucd_bin =  70
!>      Integer flag for binary section data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_udt_bin =  80
!
!>      Integer flag for merged binary data
      integer(kind = kint), parameter :: iflag_sgl_bin =  101
!>      Integer flag for merged binary surface data
      integer(kind = kint), parameter :: iflag_sgl_ucd_bin =  170
!>      Integer flag for merged binary section data
      integer(kind = kint), parameter :: iflag_sgl_udt_bin =  180
!
!>      Integer flag for merged UCD data
      integer(kind = kint), parameter :: iflag_sgl_ucd =  110
!>      Integer flag for merged UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_sgl_udt =  120
!>      Integer flag for merged VTK data
      integer(kind = kint), parameter :: iflag_sgl_vtk =  130
!>      Integer flag for merged VTK data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_sgl_vtd =  140
!>      Integer flag for HDF file data
      integer(kind = kint), parameter :: iflag_sgl_hdf5 = 150
!
!>      Integer flag for merged gzipped binary data
      integer(kind = kint), parameter :: iflag_sgl_bin_gz = 102
!>      Integer flag for merged gzipped binary data
      integer(kind = kint), parameter :: iflag_sgl_gz = 103
!
!>      Integer flag for gzipped ascii original data
      integer(kind = kint), parameter :: iflag_fld_gz =       3
!>      Integer flag for gzipped UCD data
      integer(kind = kint), parameter :: iflag_ucd_gz =      13
!>      Integer flag for gzipped UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_udt_gz =      23
!>      Integer flag for gzipped VTK data
      integer(kind = kint), parameter :: iflag_vtk_gz =      33
!>      Integer flag for gzipped VTK data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_vtd_gz =      43
!
!>      Integer flag for gzipped binary surface data
      integer(kind = kint), parameter :: iflag_ucd_bin_gz =  73
!>      Integer flag for gzipped binary section data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_udt_bin_gz =  83
!
!>      Integer flag for gzipped merged binary surface data
      integer(kind = kint), parameter :: iflag_sgl_ucd_bin_gz =  173
!>      Integer flag for gzipped merged binary section data
      integer(kind = kint), parameter :: iflag_sgl_udt_bin_gz =  183
!
!>      Integer flag for merged gzipped UCD data
      integer(kind = kint), parameter :: iflag_sgl_ucd_gz = 113
!>      Integer flag for merged gzipped UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_sgl_udt_gz = 123
!>      Integer flag for merged amd gzipped VTK data
      integer(kind = kint), parameter :: iflag_sgl_vtk_gz = 133
!>      Integer flag for merged amd gzipped VTK data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_sgl_vtd_gz = 143
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function choose_para_fld_file_format         &
     &                            (file_fmt_ctl, i_file_fmt)
!
      use skip_comment_f
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
!
!      character(len = kchara) :: input_flag
!
      call init_mgd_field_type_flags
!
!        input_flag = 'mgd_fld_gz_labels'
!        call write_multi_flags(6, input_flag, mgd_fld_gz_labels)
!        input_flag = 'mgd_fbin_gz_labels'
!        call write_multi_flags(6, input_flag, mgd_fbin_gz_labels)
      if (i_file_fmt .eq. 0) then
        choose_para_fld_file_format = iflag_sgl_vtk
        return
      end if
!
      if     (check_mul_flags(file_fmt_ctl, mgd_fld_ascii_labels)) then
        choose_para_fld_file_format = iflag_single
      else if(check_mul_flags(file_fmt_ctl, mgd_fld_bin_labels)) then
        choose_para_fld_file_format = iflag_sgl_bin
      else if(check_mul_flags(file_fmt_ctl, mgd_fld_gz_labels)) then
        choose_para_fld_file_format = iflag_sgl_gz
      else if(check_mul_flags(file_fmt_ctl, mgd_fbin_gz_labels)) then
        choose_para_fld_file_format = iflag_sgl_bin_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_udt_labels)) then
        choose_para_fld_file_format = iflag_sgl_udt
      else if(check_mul_flags(file_fmt_ctl, mgd_udt_gz_labels)) then
        choose_para_fld_file_format = iflag_sgl_udt_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_ucd_labels)) then
        choose_para_fld_file_format = iflag_sgl_ucd
      else if(check_mul_flags(file_fmt_ctl, mgd_ucd_gz_labels)) then
        choose_para_fld_file_format = iflag_sgl_ucd_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_vtd_labels)) then
        choose_para_fld_file_format = iflag_sgl_vtd
      else if(check_mul_flags(file_fmt_ctl, mgd_vtd_gz_labels)) then
        choose_para_fld_file_format = iflag_sgl_vtd_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_vtk_labels)) then
        choose_para_fld_file_format = iflag_sgl_vtk
      else if(check_mul_flags(file_fmt_ctl, mgd_vtk_gz_labels)) then
        choose_para_fld_file_format = iflag_sgl_vtk_gz
!
!      else if(check_mul_flags(file_fmt_ctl, mgd_iso_labels)) then
!        choose_para_fld_file_format = iflag_sgl_ucd_bin
!      else if(check_mul_flags(file_fmt_ctl, mgd_iso_gz_labels)) then
!        choose_para_fld_file_format = iflag_sgl_ucd_bin_gz
!
!      else if(check_mul_flags(file_fmt_ctl, mgd_psf_labels)) then
!        choose_para_fld_file_format = iflag_sgl_udt_bin
!      else if(check_mul_flags(file_fmt_ctl, mgd_psf_gz_labels)) then
!        choose_para_fld_file_format = iflag_sgl_udt_bin_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_hdf_labels)) then
        choose_para_fld_file_format = iflag_sgl_hdf5
!
      else
        choose_para_fld_file_format                                     &
     &        = choose_ucd_file_format(file_fmt_ctl, i_file_fmt)
      end if
      call dealloc_mgd_field_type_flags
!
      end function choose_para_fld_file_format
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function choose_ucd_file_format              &
     &                            (file_fmt_ctl, i_file_fmt)
!
      use skip_comment_f
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
!
!
      if (i_file_fmt .eq. 0) then
        choose_ucd_file_format = iflag_fld
        return
      end if
!
      if     (check_mul_flags(file_fmt_ctl, field_ascii_labels)) then
           choose_ucd_file_format = iflag_fld
      else if(check_mul_flags(file_fmt_ctl, field_gz_labels)) then
           choose_ucd_file_format = iflag_fld + iflag_gzip
      else if(check_mul_flags(file_fmt_ctl, field_bin_labels)) then
           choose_ucd_file_format = iflag_bin 
      else if(check_mul_flags(file_fmt_ctl, fbin_gz_labels)) then
           choose_ucd_file_format = iflag_bin_gz
!
      else if(check_mul_flags(file_fmt_ctl, udt_flags)) then
           choose_ucd_file_format = iflag_udt
      else if(check_mul_flags(file_fmt_ctl, udt_gz_flags)) then
           choose_ucd_file_format = iflag_udt + iflag_gzip
!
      else if(check_mul_flags(file_fmt_ctl, ucd_flags)) then
           choose_ucd_file_format = iflag_ucd
      else if(check_mul_flags(file_fmt_ctl, ucd_gz_flags)) then
           choose_ucd_file_format = iflag_ucd + iflag_gzip
!
      else if(check_mul_flags(file_fmt_ctl, vtd_flags)) then
           choose_ucd_file_format = iflag_vtd
      else if(check_mul_flags(file_fmt_ctl, vtd_gz_flags)) then
           choose_ucd_file_format = iflag_vtd + iflag_gzip
!
      else if(check_mul_flags(file_fmt_ctl, vtk_flags)) then
           choose_ucd_file_format = iflag_vtk
      else if(check_mul_flags(file_fmt_ctl, vtk_gz_flags)) then
           choose_ucd_file_format = iflag_vtk + iflag_gzip
!
!      else if(check_mul_flags(file_fmt_ctl, iso_flags)) then
!           choose_ucd_file_format = iflag_ucd_bin
!      else if(check_mul_flags(file_fmt_ctl, iso_gz_flags)) then
!           choose_ucd_file_format = iflag_ucd_bin + iflag_gzip
!
!      else if(check_mul_flags(file_fmt_ctl, psf_flags)) then
!           choose_ucd_file_format = iflag_udt_bin
!      else if(check_mul_flags(file_fmt_ctl, psf_gz_flags)) then
!           choose_ucd_file_format = iflag_udt_bin + iflag_gzip
      else
           choose_ucd_file_format = iflag_udt
      end if
!
      end function choose_ucd_file_format
!
! -----------------------------------------------------------------------
!
      end module m_field_file_format
