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
      if(     file_fmt_ctl.eq.'binary'                                  &
     &   .or. file_fmt_ctl.eq.'BINARY'                                  &
     &   .or. file_fmt_ctl.eq.'bin'                                     &
     &   .or. file_fmt_ctl.eq.'BIN') then
           id_field_file_format = iflag_udt
      else if(file_fmt_ctl.eq.'ascii'                                   &
     &   .or. file_fmt_ctl.eq.'ASCII'                                   &
     &   .or. file_fmt_ctl.eq.'fld'                                     &
     &   .or. file_fmt_ctl.eq.'FLD'                                     &
     &   .or. file_fmt_ctl.eq.'fld_ascii'                               &
     &   .or. file_fmt_ctl.eq.'FLD_ASCII'                               &
     &   .or. file_fmt_ctl.eq.'field'                                   &
     &   .or. file_fmt_ctl.eq.'FIELD'                                   &
     &   .or. file_fmt_ctl.eq.'field_ascii'                             &
     &   .or. file_fmt_ctl.eq.'FIELD_ASCII') then
           id_field_file_format = iflag_fld
      else if(file_fmt_ctl.eq.'gzip'                                    &
     &   .or. file_fmt_ctl.eq.'GZIP'                                    &
     &   .or. file_fmt_ctl.eq.'gz'                                      &
     &   .or. file_fmt_ctl.eq.'GZ'                                      &
     &   .or. file_fmt_ctl.eq.'fld_gzip'                                &
     &   .or. file_fmt_ctl.eq.'FLD_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'fld_gz'                                  &
     &   .or. file_fmt_ctl.eq.'FLD_GZ'                                  &
     &   .or. file_fmt_ctl.eq.'field_gzip'                              &
     &   .or. file_fmt_ctl.eq.'FIELD_GZIP'                              &
     &   .or. file_fmt_ctl.eq.'field_gz'                                &
     &   .or. file_fmt_ctl.eq.'FIELD_GZ') then
           id_field_file_format = iflag_fld + iflag_gzip
      else if(file_fmt_ctl.eq.'udt'                                     &
     &   .or. file_fmt_ctl.eq.'UDT'                                     &
     &   .or. file_fmt_ctl.eq.'udt_ascii'                               &
     &   .or. file_fmt_ctl.eq.'UDT_ASCII') then
           id_field_file_format = iflag_udt
      else if(file_fmt_ctl.eq.'udt_gzip'                                &
     &   .or. file_fmt_ctl.eq.'UDT_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'udt_gz'                                  &
     &   .or. file_fmt_ctl.eq.'UDT_GZ') then
           id_field_file_format = iflag_udt + iflag_gzip
!
      else if(file_fmt_ctl.eq.'ucd'                                     &
     &   .or. file_fmt_ctl.eq.'UCD'                                     &
     &   .or. file_fmt_ctl.eq.'ucd_ascii'                               &
     &   .or. file_fmt_ctl.eq.'UCD_ASCII') then
           id_field_file_format = iflag_ucd
      else if(file_fmt_ctl.eq.'ucd_gzip'                                &
     &   .or. file_fmt_ctl.eq.'UCD_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'ucd_gz'                                  &
     &   .or. file_fmt_ctl.eq.'UCD_GZ') then
           id_field_file_format = iflag_ucd + iflag_gzip
!
      else if(file_fmt_ctl.eq.'vtd'                                     &
     &   .or. file_fmt_ctl.eq.'VTD'                                     &
     &   .or. file_fmt_ctl.eq.'vtd_ascii'                               &
     &   .or. file_fmt_ctl.eq.'VTD_ASCII') then
           id_field_file_format = iflag_vtd
      else if(file_fmt_ctl.eq.'vtd_gzip'                                &
     &   .or. file_fmt_ctl.eq.'VTD_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'vtd_gz'                                  &
     &   .or. file_fmt_ctl.eq.'VTD_GZ') then
           id_field_file_format = iflag_vtd + iflag_gzip
!
      else if(file_fmt_ctl.eq.'vtk'                                     &
     &   .or. file_fmt_ctl.eq.'VTK'                                     &
     &   .or. file_fmt_ctl.eq.'vtk_ascii'                               &
     &   .or. file_fmt_ctl.eq.'VTK_ASCII') then
           id_field_file_format = iflag_vtk
      else if(file_fmt_ctl.eq.'vtk_gzip'                                &
     &   .or. file_fmt_ctl.eq.'VTK_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'vtk_gz'                                  &
     &   .or. file_fmt_ctl.eq.'VTK_GZ') then
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
