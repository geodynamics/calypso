!>@file   m_field_file_format_labels.f90
!!@brief  module m_field_file_format_labels
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief Integer flags for field data file format
!!
!!
!!@verbatim
!!      subroutine init_field_type_flags()
!!      subroutine dealloc_field_type_flags()
!!
!! ------------------------------------------------------------------
!!   flag lists for field data
!!
!!  Distributed data file
!!   ASCII   data file
!!     UCD file:                ucd
!!     splitted UCD file:       udt
!!     VTK file:                vtk
!!     splitted VTK file:       vtd
!!     Binary file:             iso
!!     splitted Binary file:    psf
!!
!!   GZIPPED data file
!!     UCD file:             ucd_gzip
!!     splitted UCD file:    udt_gzip
!!     VTK file:             vtk_gzip
!!     splitted VTK file:    vtd_gzip
!!     Binary file:          iso_gzip
!!     splitted Binary file: psf_gzip
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!
      module m_field_file_format_labels
!
      use m_precision
      use m_constants
      use m_file_format_labels
      use t_multi_flag_labels
!
      implicit    none
!
!
!>      flag for UCD
      character(len = kchara), parameter :: ucd_name(1) = (/'UCD'/)
!>      flag for Splitted UCD
      character(len = kchara), parameter :: udt_name(1) = (/'UDT'/)
!>      flag for VTK
      character(len = kchara), parameter :: vtk_name(1) = (/'VTK'/)
!>      flag for Splitted VTK
      character(len = kchara), parameter :: vtd_name(1) = (/'VTD'/)
!>      flag for binary data for Isosurfacing
      character(len = kchara), parameter :: iso_name(1) = (/'ISO'/)
!>      flag for binary data for Sectioning
      character(len = kchara), parameter :: psf_name(1) = (/'PSF'/)
!
!>      flag parts for field data
      character(len = kchara), parameter :: field_flg(2)                &
     &                        = (/'field ', 'fld   '/)
!
!
!>      flag parts for field data format
!!        'field', 'fld ' 
      type(multi_flag_labels), save :: field_labels
!
!>      flag parts for field data format
!!        'field', 'fld', 'ascii', 'text', 'field_ascii', 'field_text',
!!        'fld_ascii', 'fld_text', 'ascii_field', 'ascii_fld', 
!!        'text_field', 'text_fld' 
      type(multi_flag_labels), save :: field_ascii_labels
!>      flag parts for field data format
!!        'field', 'fld', 'ascii', 'text', 'field_ascii', 'field_text',
!!        'fld_ascii', 'fld_text', 'ascii_field', 'ascii_fld', 
!!        'text_field', 'text_fld' 
      type(multi_flag_labels), save :: field_bin_labels
!>      flag parts for field data format
!!        'field', 'fld', 'ascii', 'text', 'field_ascii', 'field_text',
!!        'fld_ascii', 'fld_text', 'ascii_field', 'ascii_fld', 
!!        'text_field', 'text_fld' 
!
!>     Character lables for UCD
!!        'UCD' 
      type(multi_flag_labels), save :: ucd_flags
!>     Character lables for splitted UCD mesh and data
!!        'UDT' 
      type(multi_flag_labels), save :: udt_flags
!>     Character lables for VTK
!!        'VTK' 
      type(multi_flag_labels), save :: vtk_flags
!>     Character lables for splitted VTK mesh and data
!!        'VTD' 
      type(multi_flag_labels), save :: vtd_flags
!>     Character lables for isosurface binary
!!        'ISO' 
      type(multi_flag_labels), save :: iso_flags
!>     Character lables for sections binary
!!        'PSF' 
      type(multi_flag_labels), save :: psf_flags
!
!>      flag parts for gzipped field data format
!!        'gzip', 'gz', 'field_gzip', 'field_gz', 'fld_gzip', 'fld_gz',
!!        'gzip_field', 'gzip_fld', 'gz_field', 'gz_fld' 
      type(multi_flag_labels), save :: field_gz_labels
!>      flag parts for gzipped field data format
      type(multi_flag_labels), save :: fbin_gz_labels
!
!>     Character lables for gzipped UCD
!!       'UCD_gzip', 'UCD_gz', 'gzip_UCD', 'gz_UCD' 
      type(multi_flag_labels), save :: ucd_gz_flags
!
!>     Character lables for gzipped splitted UCD
!!       'UDT_gzip', 'UDT_gz', 'gzip_UDT', 'gz_UDT' 
      type(multi_flag_labels), save :: udt_gz_flags
!
!>     Character lables for gzipped VTK
!!       'VTK_gzip', 'VTK_gz', 'gzip_VTK', 'gz_VTK' 
      type(multi_flag_labels), save :: vtk_gz_flags
!
!>     Character lables for gzipped splitted VTK
!!       'VTD_gzip', 'VTD_gz', 'gzip_VTD', 'gz_VTD' 
      type(multi_flag_labels), save :: vtd_gz_flags
!
!>     Character lables for gzipped isosurface binary
!!       'ISO_gzip', 'ISO_gz', 'gzip_ISO', 'gz_ISO' 
      type(multi_flag_labels), save :: iso_gz_flags
!
!>     Character lables for gzipped sections binary
!!       'PSF_gzip', 'PSF_gz', 'gzip_PSF', 'gz_PSF' 
      type(multi_flag_labels), save :: psf_gz_flags
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_field_type_flags()
!
      integer(kind = kint) :: icou
!
      call init_file_format_flags()
!
      call init_multi_flags_by_labels(itwo, field_flg, field_labels)
!
      call init_from_two_kinds_flags                                    &
     &   (field_labels, ascii_flags, field_ascii_labels, icou)
      call append_multi_flag_labels(field_labels, field_ascii_labels)
      call append_multi_flag_labels(ascii_flags, field_ascii_labels)
!
      call init_from_two_kinds_flags                                    &
     &   (field_labels, binary_flags, field_bin_labels, icou)
      call append_multi_flag_labels(binary_flags, field_bin_labels)
!
      call init_multi_flags_by_labels(ione, ucd_name, ucd_flags)
      call init_multi_flags_by_labels(ione, udt_name, udt_flags)
      call init_multi_flags_by_labels(ione, vtk_name, vtk_flags)
      call init_multi_flags_by_labels(ione, vtd_name, vtd_flags)
      call init_multi_flags_by_labels(ione, iso_name, iso_flags)
      call init_multi_flags_by_labels(ione, psf_name, psf_flags)
!
      call init_from_two_kinds_flags                                    &
     &   (field_labels, gzip_flags, field_gz_labels, icou)
      call append_multi_flag_labels(gzip_flags, field_gz_labels)
!
      call init_from_two_kinds_flags                                    &
     &   (field_bin_labels, gzip_flags, fbin_gz_labels, icou)
      call append_multi_flag_labels(gzip_bin_flags, fbin_gz_labels)
!
      call init_from_two_kinds_flags                                    &
     &   (ucd_flags, gzip_flags, ucd_gz_flags, icou)
      call init_from_two_kinds_flags                                    &
     &   (udt_flags, gzip_flags, udt_gz_flags, icou)
      call init_from_two_kinds_flags                                    &
     &   (vtk_flags, gzip_flags, vtk_gz_flags, icou)
      call init_from_two_kinds_flags                                    &
     &   (vtd_flags, gzip_flags, vtd_gz_flags, icou)
      call init_from_two_kinds_flags                                    &
     &   (iso_flags, gzip_flags, iso_gz_flags, icou)
      call init_from_two_kinds_flags                                    &
     &   (psf_flags, gzip_flags, psf_gz_flags, icou)
!
      end subroutine init_field_type_flags
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_field_type_flags()
!
      call dealloc_file_format_flags()
!
      call dealloc_multi_flags(field_labels)
!
      call dealloc_multi_flags(field_ascii_labels)
      call dealloc_multi_flags(field_bin_labels)
!
      call dealloc_multi_flags(ucd_flags)
      call dealloc_multi_flags(udt_flags)
      call dealloc_multi_flags(vtk_flags)
      call dealloc_multi_flags(vtd_flags)
      call dealloc_multi_flags(iso_flags)
      call dealloc_multi_flags(psf_flags)
!
      call dealloc_multi_flags(field_gz_labels)
      call dealloc_multi_flags(fbin_gz_labels)
!
      call dealloc_multi_flags(ucd_gz_flags)
      call dealloc_multi_flags(udt_gz_flags)
      call dealloc_multi_flags(vtk_gz_flags)
      call dealloc_multi_flags(vtd_gz_flags)
      call dealloc_multi_flags(iso_gz_flags)
      call dealloc_multi_flags(psf_gz_flags)
!
      end subroutine dealloc_field_type_flags
!
! -----------------------------------------------------------------------
!
      end module m_field_file_format_labels
