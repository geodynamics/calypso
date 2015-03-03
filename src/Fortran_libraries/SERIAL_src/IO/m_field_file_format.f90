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
!!      subroutine choose_ucd_file_format(file_fmt_ctl, i_file_fmt,     &
!!     &          id_field_file_format)
!!
!! ------------------------------------------------------------------
!!   flag lists for field data
!!
!!  Distributed data file
!!   ASCII   data file
!!     UCD file:             ucd_ascii
!!     splitted UCD file:    udt_ascii
!!     VTK file:             vtk_ascii
!!     splitted VTK file:    vtd_ascii
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
!!     UCD file:             single_ucd_ascii
!!     splitted UCD file:    single_udt_ascii
!!     VTK file:             single_vtk_ascii
!!     splitted VTK file:    single_vtd_ascii
!!     HDF5 file:            merged_HDF5
!!
!!   GZIPPED data file
!!     UCD file:             single_ucd_gzip
!!     splitted UCD file:    single_udt_gzip
!!     VTK file:             single_vtk_gzip
!!     splitted VTK file:    single_vtd_gzip
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
!
      implicit    none
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
!
!>      Integer flag for distributed data
      integer(kind = kint), parameter :: iflag_para =      0
!>      Integer flag for merged data
      integer(kind = kint), parameter :: iflag_single =  100
!
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
!>      Integer flag for merged amd gzipped UCD data
      integer(kind = kint), parameter :: iflag_sgl_ucd_gz = 113
!>      Integer flag for merged amd gzipped UCD data
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
      subroutine choose_ucd_file_format(file_fmt_ctl, i_file_fmt,       &
     &          id_field_file_format)
!
      use skip_comment_f
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(inout) :: id_field_file_format
!
!
      if (i_file_fmt .eq. 0) then
        id_field_file_format = iflag_fld
        return
      end if
!
      if     (cmp_no_case(file_fmt_ctl, 'binary')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'bin')         ) then
           id_field_file_format = iflag_udt
      else if(cmp_no_case(file_fmt_ctl, 'field_ascii')                  &
     &   .or. cmp_no_case(file_fmt_ctl, 'field')                        &
     &   .or. cmp_no_case(file_fmt_ctl, 'fld_ascii')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'fld')                          &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_field')                  &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_fld')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii')                        &
     &   .or. cmp_no_case(file_fmt_ctl, 'text')        ) then
           id_field_file_format = iflag_fld
      else if(cmp_no_case(file_fmt_ctl, 'field_gzip')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'field_gz')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'fld_gzip')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'fld_gz')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_field')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_field')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_fld')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_fld')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip')                         &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz')         ) then
           id_field_file_format = iflag_fld + iflag_gzip
      else if(cmp_no_case(file_fmt_ctl, 'UDT_ascii')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'UDT')        ) then
           id_field_file_format = iflag_udt
      else if(cmp_no_case(file_fmt_ctl, 'UDT_gzip')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'UDT_gz')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_UDT')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_UDT')     ) then
           id_field_file_format = iflag_udt + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'UCD_ascii')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'UCD')        ) then
           id_field_file_format = iflag_ucd
      else if(cmp_no_case(file_fmt_ctl, 'UCD_gzip')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'UCD_gz')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_UCD')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_UCD')     ) then
           id_field_file_format = iflag_ucd + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'VTD_ascii')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'VTD')        ) then
           id_field_file_format = iflag_vtd
      else if(cmp_no_case(file_fmt_ctl, 'VTD_gzip')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'VTD_gz')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_VTD')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_VTD')     ) then
           id_field_file_format = iflag_vtd + iflag_gzip
!
      else if(cmp_no_case(file_fmt_ctl, 'VTK_ascii')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'VTK')        ) then
           id_field_file_format = iflag_vtk
      else if(cmp_no_case(file_fmt_ctl, 'VTK_gzip')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'VTK_gz')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_VTK')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_VTK')     ) then
           id_field_file_format = iflag_vtk + iflag_gzip
      else
           id_field_file_format = iflag_udt
      end if
!
      end subroutine choose_ucd_file_format
!
! -----------------------------------------------------------------------
!
      end module m_field_file_format
