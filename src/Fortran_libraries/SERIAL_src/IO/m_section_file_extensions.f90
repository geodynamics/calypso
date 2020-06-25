!>@file   m_section_file_extensions.f90
!!@brief  module m_section_file_extensions
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief Integer flags for field data file format
!!
!!
!!@verbatim
!!      integer(kind = kint) function                                   &
!!     &                    section_format_id_from_input()
!!
!!      function psf_extension_list()
!!        character(len= 6*(3+6+5)) :: psf_extension_list
!!      integer(kind = kint) function                                   &
!!     &                    section_format_id_from_ext(file_ext)
!!@endverbatim
      module m_section_file_extensions
!
      use m_precision
      use m_field_file_format
!
      implicit none
!
      character(len = kchara), parameter :: ext_vtk =    'vtk'
      character(len = kchara), parameter :: ext_vtd =    'vtd'
      character(len = kchara), parameter :: ext_inp =    'inp'
      character(len = kchara), parameter :: ext_udt =    'udt'
      character(len = kchara), parameter :: ext_psf =    'psf'
      character(len = kchara), parameter :: ext_sdt =    'sdt'
      character(len = kchara), parameter :: ext_vtk_gz = 'vtk.gz'
      character(len = kchara), parameter :: ext_vtd_gz = 'vtd.gz'
      character(len = kchara), parameter :: ext_inp_gz = 'inp.gz'
      character(len = kchara), parameter :: ext_udt_gz = 'udt.gz'
      character(len = kchara), parameter :: ext_psf_gz = 'psf.gz'
      character(len = kchara), parameter :: ext_sdt_gz = 'sdt.gz'
!
      private ::  ext_vtk, ext_vtk_gz, ext_vtd, ext_vtd_gz,             &
     &            ext_inp, ext_inp_gz, ext_udt, ext_udt_gz,             &
     &            ext_psf, ext_psf_gz, ext_sdt, ext_sdt_gz
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    section_format_id_from_input()
!
      character(len = kchara) :: file_ext
!
!
      write(*,*) 'Input file extension from following:'
      write(*,*) psf_extension_list()
      read(*,*) file_ext
      section_format_id_from_input                                      &
     &       = section_format_id_from_ext(file_ext)
!
      end function section_format_id_from_input
!
! -----------------------------------------------------------------------
!
      function psf_extension_list()
!
      character(len= 6*(3+6+4)) :: psf_extension_list
!
      psf_extension_list                                                &
     &            =  trim(ext_vtk) // ', ' // trim(ext_vtk_gz) // ', '  &
     &            // trim(ext_vtd) // ', ' // trim(ext_vtd_gz) // ', '  &
     &            // trim(ext_inp) // ', ' // trim(ext_inp_gz) // ', '  &
     &            // trim(ext_udt) // ', ' // trim(ext_udt_gz) // ', '  &
     &            // trim(ext_psf) // ', ' // trim(ext_psf_gz) // ', '  &
     &            // trim(ext_sdt) // ', ' // trim(ext_sdt_gz) // '  '
!
      end function psf_extension_list
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    section_format_id_from_ext(file_ext)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: file_ext
!
      section_format_id_from_ext = iflag_vtk
      if     (cmp_no_case(file_ext, ext_vtk)) then
        section_format_id_from_ext = iflag_vtk
      else if(cmp_no_case(file_ext, ext_vtd)) then
        section_format_id_from_ext = iflag_vtd
      else if(cmp_no_case(file_ext, ext_inp)) then
        section_format_id_from_ext = iflag_ucd
      else if(cmp_no_case(file_ext, ext_udt)) then
        section_format_id_from_ext = iflag_udt
      else if(cmp_no_case(file_ext, ext_psf)) then
        section_format_id_from_ext = iflag_ucd_bin
      else if(cmp_no_case(file_ext, ext_sdt)) then
        section_format_id_from_ext = iflag_udt_bin
!
      else if(cmp_no_case(file_ext, ext_vtk_gz)) then
        section_format_id_from_ext = iflag_vtk_gz
      else if(cmp_no_case(file_ext, ext_vtd_gz)) then
        section_format_id_from_ext = iflag_vtd_gz
      else if(cmp_no_case(file_ext, ext_inp_gz)) then
        section_format_id_from_ext = iflag_ucd_gz
      else if(cmp_no_case(file_ext, ext_udt_gz)) then
        section_format_id_from_ext = iflag_udt_gz
      else if(cmp_no_case(file_ext, ext_psf_gz)) then
        section_format_id_from_ext = iflag_ucd_bin_gz
      else if(cmp_no_case(file_ext, ext_sdt_gz)) then
        section_format_id_from_ext = iflag_udt_bin_gz
      end if
!
      end function section_format_id_from_ext
!
! -----------------------------------------------------------------------
!
      end module m_section_file_extensions

