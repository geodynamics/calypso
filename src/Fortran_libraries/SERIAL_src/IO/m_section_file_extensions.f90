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
!!      integer(kind = kint) function                                   &
!!     &                    psf_to_vtk_format_id_from_input()
!!
!!      function psf_extension_list()
!!        character(len= 6*(3+6+5)) :: psf_extension_list
!!      integer(kind = kint) function                                   &
!!     &                    section_format_id_from_ext(file_ext)
!!      subroutine set_psf_extensions(psf_exts)
!!        type(psf_extensions), intent(inout) :: psf_exts
!!@endverbatim
      module m_section_file_extensions
!
      use m_precision
      use m_field_file_format
!
      implicit none
!
      type psf_extensions
        character(len = kchara) :: vtk
        character(len = kchara) :: vtd
        character(len = kchara) :: inp
        character(len = kchara) :: udt
        character(len = kchara) :: sfm
        character(len = kchara) :: sdt
        character(len = kchara) :: vtk_gz
        character(len = kchara) :: vtd_gz
        character(len = kchara) :: inp_gz
        character(len = kchara) :: udt_gz
        character(len = kchara) :: sfm_gz
        character(len = kchara) :: sdt_gz
      end type psf_extensions
!
      private :: set_psf_extensions, section_format_id_from_ext
      private :: psf_to_vtk_format_list
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
      type(psf_extensions) :: psf_exts
!
!
      write(*,*) 'Input file extension from following:'
      write(*,*) psf_extension_list()
      read(*,*) file_ext
!
      call set_psf_extensions(psf_exts)
      section_format_id_from_input                                      &
     &       = section_format_id_from_ext(file_ext, psf_exts)
!
      end function section_format_id_from_input
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    psf_to_vtk_format_id_from_input()
!
      character(len = kchara) :: file_ext
      type(psf_extensions) :: psf_exts
!
!
      write(*,*) 'Input file extension from following:'
      write(*,*) psf_to_vtk_format_list()
      read(*,*) file_ext
!
      call set_psf_extensions(psf_exts)
      psf_to_vtk_format_id_from_input                                   &
     &       = section_format_id_from_ext(file_ext, psf_exts)
!
      end function psf_to_vtk_format_id_from_input
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function psf_extension_list()
!
      character(len= 6*(3+6+4)) :: psf_extension_list
!
      type(psf_extensions) :: psf_exts
!
!
      call set_psf_extensions(psf_exts)
!
      psf_extension_list                                                &
     &  =  trim(psf_exts%vtk) // ', ' // trim(psf_exts%vtk_gz) // ', '  &
     &  // trim(psf_exts%vtd) // ', ' // trim(psf_exts%vtd_gz) // ', '  &
     &  // trim(psf_exts%inp) // ', ' // trim(psf_exts%inp_gz) // ', '  &
     &  // trim(psf_exts%udt) // ', ' // trim(psf_exts%udt_gz) // ', '  &
     &  // trim(psf_exts%sfm) // ', ' // trim(psf_exts%sfm_gz) // ', '  &
     &  // trim(psf_exts%sdt) // ', ' // trim(psf_exts%sdt_gz) // '  '
!
      end function psf_extension_list
!
! -----------------------------------------------------------------------
!
      function psf_to_vtk_format_list()
!
      character(len= 4*(3+6+4)) :: psf_to_vtk_format_list
!
      type(psf_extensions) :: psf_exts
!
!
      call set_psf_extensions(psf_exts)
!
      psf_to_vtk_format_list                                            &
     &  =  trim(psf_exts%inp) // ', ' // trim(psf_exts%inp_gz) // ', '  &
     &  // trim(psf_exts%udt) // ', ' // trim(psf_exts%udt_gz) // ', '  &
     &  // trim(psf_exts%sfm) // ', ' // trim(psf_exts%sfm_gz) // ', '  &
     &  // trim(psf_exts%sdt) // ', ' // trim(psf_exts%sdt_gz) // '  '
!
      end function psf_to_vtk_format_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function section_format_id_from_ext          &
     &                            (file_ext, psf_exts)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: file_ext
      type(psf_extensions), intent(in) :: psf_exts
!
!
      section_format_id_from_ext = iflag_vtk
      if     (cmp_no_case(file_ext, psf_exts%vtk)) then
        section_format_id_from_ext = iflag_vtk
      else if(cmp_no_case(file_ext, psf_exts%vtd)) then
        section_format_id_from_ext = iflag_vtd
      else if(cmp_no_case(file_ext, psf_exts%inp)) then
        section_format_id_from_ext = iflag_ucd
      else if(cmp_no_case(file_ext, psf_exts%udt)) then
        section_format_id_from_ext = iflag_udt
      else if(cmp_no_case(file_ext, psf_exts%sfm)) then
        section_format_id_from_ext = iflag_ucd_bin
      else if(cmp_no_case(file_ext, psf_exts%sdt)) then
        section_format_id_from_ext = iflag_udt_bin
!
      else if(cmp_no_case(file_ext, psf_exts%vtk_gz)) then
        section_format_id_from_ext = iflag_vtk_gz
      else if(cmp_no_case(file_ext, psf_exts%vtd_gz)) then
        section_format_id_from_ext = iflag_vtd_gz
      else if(cmp_no_case(file_ext, psf_exts%inp_gz)) then
        section_format_id_from_ext = iflag_ucd_gz
      else if(cmp_no_case(file_ext, psf_exts%udt_gz)) then
        section_format_id_from_ext = iflag_udt_gz
      else if(cmp_no_case(file_ext, psf_exts%sfm_gz)) then
        section_format_id_from_ext = iflag_ucd_bin_gz
      else if(cmp_no_case(file_ext, psf_exts%sdt_gz)) then
        section_format_id_from_ext = iflag_udt_bin_gz
      end if
!
      end function section_format_id_from_ext
!
! -----------------------------------------------------------------------
!
      subroutine set_psf_extensions(psf_exts)
!
      use set_parallel_file_name
      use set_ucd_extensions
!
      type(psf_extensions), intent(inout) :: psf_exts
!
      write(psf_exts%vtk,'(a3)') vtk_ext
      write(psf_exts%vtd,'(a3)') vtd_ext
!
      write(psf_exts%inp,'(a3)') inp_ext
      write(psf_exts%udt,'(a3)') udt_ext
!
      write(psf_exts%sdt,'(a3)') sdt_ext
      write(psf_exts%sfm,'(a3)') sfm_ext
!
!
      write(psf_exts%vtk_gz,'(a3,a1,a2)') vtk_ext, '.', gz_ext
      write(psf_exts%vtd_gz,'(a3,a1,a2)') vtd_ext, '.', gz_ext
!
      write(psf_exts%inp_gz,'(a3,a1,a2)') inp_ext, '.', gz_ext
      write(psf_exts%udt_gz,'(a3,a1,a2)') udt_ext, '.', gz_ext
!
      write(psf_exts%sdt_gz,'(a3,a1,a2)') sdt_ext, '.', gz_ext
      write(psf_exts%sfm_gz,'(a3,a1,a2)') sfm_ext, '.', gz_ext
!
      end subroutine set_psf_extensions
!
! -----------------------------------------------------------------------
!
      end module m_section_file_extensions

