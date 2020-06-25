!>@file   m_merged_field_fmt_labels.f90
!!@brief  module m_merged_field_fmt_labels
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief Integer flags for field data file format
!!
!!
!!@verbatim
!!      subroutine init_mgd_field_type_flags()
!!      subroutine dealloc_mgd_field_type_flags()
!!@endverbatim
!!
!
      module m_merged_field_fmt_labels
!
      use m_precision
      use m_file_format_labels
      use m_field_file_format_labels
      use t_multi_flag_labels
!
      implicit    none
!
!
!>      flag parts for HDF data
      character(len = kchara), parameter :: hdf5_names(3)               &
     &            = (/'HDF5        ', 'merged_HDF5 ', 'single_HDF5 '/)
!
!>     Character lables for merged UCD
!!       'merged_UCD', 'single_UCD', 'UCD_merged', 'UCD_single' 
      type(multi_flag_labels), save :: mgd_ucd_labels
!
!>     Character lables for merged splitted UCD into mesh and data
!!      'merged_UDT', 'single_UDT', 'UDT_merged', 'UDT_single',
!!      'merged', 'single' 
      type(multi_flag_labels), save :: mgd_udt_labels
!
!>     Character lables for merged VTK
!!       'merged_VTK', 'single_VTK', 'VTK_merged', 'VTK_single' 
      type(multi_flag_labels), save :: mgd_vtk_labels
!
!>     Character lables for merged splitted VTK mesh and data
!!       'merged_VTD', 'single_VTD', 'VTD_merged', 'VTD_single' 
      type(multi_flag_labels), save :: mgd_vtd_labels
!
!>     Character lables for merged isosurface binary
!!       'merged_ISO', 'single_ISO', 'ISO_merged', 'ISO_single' 
      type(multi_flag_labels), save :: mgd_iso_labels
!
!>     Character lables for merged sections binary
!!       'merged_PSF', 'single_PSF', 'PSF_merged', 'PSF_single' 
      type(multi_flag_labels), save :: mgd_psf_labels
!
!
!>     Character lables for gzipped merged splitted UCD 
!!       into mesh and data
!!       'merged_UDT_gzip', 'merged_UDT_gz',   'single_UDT_gzip',
!!       'single_UDT_gz',   'UDT_merged_gzip', 'UDT_merged_gz',
!!       'UDT_single_gzip', 'UDT_single_gz',   'merged_gzip',
!!       'merged_gz', 'single_gzip', 'single_gz', 'gzip_merged_UDT',
!!       'gzip_single_UDT', 'gzip_UDT_merged', 'gzip_UDT_single',
!!       'gzip_merged',     'gzip_single',     'gz_merged_UDT',
!!       'gz_single_UDT',   'gz_UDT_merged',   'gz_UDT_single',
!!       'gz_merged', 'gz_single' 
      type(multi_flag_labels), save :: mgd_udt_gz_labels
!
!>     Character lables for gzipped merged UCD
!!       'merged_UCD_gzip', 'merged_UCD_gz',   'single_UCD_gzip',
!!       'single_UCD_gz',   'UCD_merged_gzip', 'UCD_merged_gz',
!!       'UCD_single_gzip', 'UCD_single_gz',   'gzip_merged_UCD',
!!       'gzip_single_UCD', 'gzip_UCD_merged', 'gzip_UCD_single',
!!       'gz_merged_UCD',   'gz_single_UCD',   'gz_UCD_merged',
!!       'gz_UCD_single' 
      type(multi_flag_labels), save :: mgd_ucd_gz_labels
!
!>     Character lables for gzipped merged VTK
!!       'merged_VTK_gzip', 'merged_VTK_gz',   'single_VTK_gzip',
!!       'single_VTK_gz',   'VTK_merged_gzip', 'VTK_merged_gz',
!!       'VTK_single_gzip', 'VTK_single_gz',   'gzip_merged_VTK',
!!       'gzip_single_VTK', 'gzip_VTK_merged', 'gzip_VTK_single',
!!       'gz_merged_VTK',   'gz_single_VTK',   'gz_VTK_merged',
!!       'gz_VTK_single' 
      type(multi_flag_labels), save :: mgd_vtk_gz_labels
!
!>     Character lables for gzipped merged splitted VTK mesh and data
!!       'merged_VTD_gzip', 'merged_VTD_gz',   'single_VTD_gzip',
!!       'single_VTD_gz',   'VTD_merged_gzip', 'VTD_merged_gz',
!!       'VTD_single_gzip', 'VTD_single_gz',   'gzip_merged_VTD',
!!       'gzip_single_VTD', 'gzip_VTD_merged', 'gzip_VTD_single',
!!       'gz_merged_VTD',   'gz_single_VTD',   'gz_VTD_merged',
!!       'gz_VTD_single'
      type(multi_flag_labels), save :: mgd_vtd_gz_labels
!
!>     Character lables for gzipped merged isosurface binary
!!       'merged_ISO_gzip', 'merged_ISO_gz',   'single_ISO_gzip',
!!       'single_ISO_gz',   'ISO_merged_gzip', 'ISO_merged_gz',
!!       'ISO_single_gzip', 'ISO_single_gz',   'gzip_merged_ISO',
!!       'gzip_single_ISO', 'gzip_ISO_merged', 'gzip_ISO_single',
!!       'gz_merged_ISO',   'gz_single_ISO',   'gz_ISO_merged',
!!       'gz_ISO_single' 
      type(multi_flag_labels), save :: mgd_iso_gz_labels
!
!>     Character lables for gzipped merged sections binary
!!       'merged_PSF_gzip', 'merged_PSF_gz',   'single_PSF_gzip',
!!       'single_PSF_gz',   'PSF_merged_gzip', 'PSF_merged_gz',
!!       'PSF_single_gzip', 'PSF_single_gz',   'gzip_merged_PSF',
!!       'gzip_single_PSF', 'gzip_PSF_merged', 'gzip_PSF_single',
!!       'gz_merged_PSF',   'gz_single_PSF',   'gz_PSF_merged',
!!       'gz_PSF_single' 
      type(multi_flag_labels), save :: mgd_psf_gz_labels
!
!
!>     Character lables for parallel HDF5
!!       'HDF5', 'merged_HDF5', 'single_HDF5' 
      type(multi_flag_labels), save :: mgd_hdf_labels
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_mgd_field_type_flags()
!
      integer(kind = kint) :: icou
!
      call init_field_type_flags()
!
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, udt_flags, mgd_udt_labels, icou)
      call append_multi_flag_labels(merged_flags, mgd_udt_labels)
!
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, ucd_flags, mgd_ucd_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, vtk_flags, mgd_vtk_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, vtd_flags, mgd_vtd_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, iso_flags, mgd_iso_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, psf_flags, mgd_psf_labels, icou)
!
      call init_from_two_kinds_flags                                    &
     &   (mgd_udt_labels, gzip_flags, mgd_udt_gz_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (mgd_ucd_labels, gzip_flags, mgd_ucd_gz_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (mgd_vtk_labels, gzip_flags, mgd_vtk_gz_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (mgd_vtd_labels, gzip_flags, mgd_vtd_gz_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (mgd_iso_labels, gzip_flags, mgd_iso_gz_labels, icou)
      call init_from_two_kinds_flags                                    &
     &   (mgd_psf_labels, gzip_flags, mgd_psf_gz_labels, icou)
!
      call init_multi_flags_by_labels                                   &
     &   (ithree, hdf5_names, mgd_hdf_labels)
!
      end subroutine init_mgd_field_type_flags
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_mgd_field_type_flags()
!
!
      call dealloc_field_type_flags()
!
      call dealloc_multi_flags(mgd_ucd_labels)
      call dealloc_multi_flags(mgd_udt_labels)
      call dealloc_multi_flags(mgd_vtk_labels)
      call dealloc_multi_flags(mgd_vtd_labels)
      call dealloc_multi_flags(mgd_iso_labels)
      call dealloc_multi_flags(mgd_psf_labels)
      call dealloc_multi_flags(mgd_hdf_labels)
!
      call dealloc_multi_flags(mgd_ucd_gz_labels)
      call dealloc_multi_flags(mgd_udt_gz_labels)
      call dealloc_multi_flags(mgd_vtk_gz_labels)
      call dealloc_multi_flags(mgd_vtd_gz_labels)
      call dealloc_multi_flags(mgd_iso_gz_labels)
      call dealloc_multi_flags(mgd_psf_gz_labels)
!
      end subroutine dealloc_mgd_field_type_flags
!
! -----------------------------------------------------------------------
!
      end module m_merged_field_fmt_labels
