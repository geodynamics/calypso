!>@file  udt_data_IO.f90
!!       module udt_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for UCD data segments
!!
!!@verbatim
!!      subroutine write_udt_field_header(ifile_psf, num_output,        &
!!     &          ncomp_out, name_out)
!!      subroutine write_ucd_field_data(ifile_psf, ntot_out, ncomp_dat, &
!!     &          nnod, inod_out, dat_out)
!!
!!      subroutine read_udt_field_header(ifile_psf, num_input,          &
!!     &          ncomp_in, name_in)
!!      subroutine read_ucd_field_data(ifile_psf, nnod_in,              &
!!     &          ncomp_dat, inod_in, dat_in)
!!      subroutine read_ucd_mesh_header(ifile_psf, nnod_in, nele_in,    &
!!     &          ntot_comp)
!!      subroutine read_ucd_mesh_data(ifile_psf, nnod_in, nele_in,      &
!!     &          nnod_ele, inod_gl, iele_gl, xx_in, ie_in)
!!
!!      subroutine write_udt_mesh_header(ifile_psf, nnod_output,        &
!!     &          nele_out, ncomp_output)
!!      subroutine write_ucd_mesh_connect(ifile_psf, ntot_ele,          &
!!     &          nnod_ele, nele, iele_gl, ie_gl)
!!@endverbatim
!
      module udt_data_IO
!
      use m_precision
      use m_constants
      use ucd_data_to_buffer
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_udt_field_header(ifile_psf, num_output,          &
     &          ncomp_out, name_out)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint), intent(in) :: num_output
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
!
      integer(kind = kint) :: j
!
!
      write(ifile_psf,'(a)',advance='NO')                               &
     &         ucd_num_comps(num_output, ncomp_out)
      do j = 1, num_output
        write(ifile_psf,'(a)',advance='NO') ucd_field_name(name_out(j))
      end do
!
      end subroutine write_udt_field_header
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_ucd_field_data(ifile_psf, ntot_out, ncomp_dat,   &
     &          nnod, inod_out, dat_out)
!
      integer(kind = kint), intent(in) :: ifile_psf, ncomp_dat
      integer(kind = kint_gl), intent(in) :: ntot_out
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint_gl), intent(in) :: inod_out(ntot_out)
      real(kind = kreal), intent(in) :: dat_out(ntot_out, ncomp_dat)
!
      integer(kind = kint_gl) :: inod
!
      character(len=kchara) :: fmt_txt
!
      write(fmt_txt,'(a5,i4,a13)')                                      &
     &                '(i16,', ncomp_dat, '(1pE23.12e3))'
      do inod = 1, nnod
        write(ifile_psf,fmt_txt)                                        &
     &                  inod_out(inod), dat_out(inod,1:ncomp_dat)
      end do
!
      end subroutine  write_ucd_field_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_udt_field_header(ifile_psf, num_input,            &
     &          ncomp_in, name_in)
!
      integer(kind = kint), intent(in) :: ifile_psf
!
      integer(kind = kint), intent(inout) :: num_input
      integer(kind = kint), intent(inout) :: ncomp_in(num_input)
      character(len = kchara), intent(inout) :: name_in(num_input)
!
      integer(kind = kint) :: j
!
!
      read(ifile_psf,*) num_input, ncomp_in(1:num_input)
      do j = 1, num_input
        read(ifile_psf,*) name_in(j)
      end do
!
      end subroutine read_udt_field_header
!
! ----------------------------------------------------------------------
!
      subroutine read_ucd_field_data(ifile_psf, nnod_in,                &
     &          ncomp_dat, dat_in)
!
      integer(kind = kint), intent(in) :: ifile_psf, ncomp_dat
      integer(kind = kint_gl), intent(in) :: nnod_in
      real(kind = kreal), intent(inout) :: dat_in(nnod_in, ncomp_dat)
!
      integer(kind = kint_gl) :: inod, itmp
!
!
      do inod = 1, nnod_in
        read(ifile_psf,*) itmp, dat_in(inod,1:ncomp_dat)
      end do
!
      end subroutine read_ucd_field_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_ucd_mesh_header(ifile_psf, nnod_in, nele_in,      &
     &          ntot_comp)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint_gl), intent(inout) :: nnod_in, nele_in
      integer(kind = kint), intent(inout) :: ntot_comp
!
      integer(kind = kint) :: itmp
!
      read(ifile_psf,*) nnod_in, nele_in, ntot_comp, itmp, itmp
!
      end subroutine read_ucd_mesh_header
!
!-----------------------------------------------------------------------
!
      subroutine read_ucd_mesh_data(ifile_psf, nnod_in, nele_in,        &
     &          nnod_ele, inod_gl, iele_gl, xx_in, ie_in)
!
      integer(kind=kint), intent(in) :: ifile_psf, nnod_ele
      integer(kind=kint_gl), intent(in) :: nnod_in, nele_in
      integer(kind=kint_gl), intent(inout) :: iele_gl(nele_in)
      integer(kind=kint_gl), intent(inout) :: ie_in(nele_in,nnod_ele)
      integer(kind=kint_gl), intent(inout) :: inod_gl(nnod_in)
      real(kind = kreal), intent(inout) :: xx_in(nnod_in,3)
!
      integer(kind = kint_gl) :: inod, iele
      integer(kind = kint) :: itmp
      character(len=6) :: eleflag
!
!
      do inod = 1, nnod_in
        read(ifile_psf,*) inod_gl(inod), xx_in(inod,1:3)
      end do
!
      do iele = 1, nele_in
        read(ifile_psf,*) iele_gl(iele), itmp,                          &
     &       eleflag, ie_in(iele,1:nnod_ele)
      end do
!
      end subroutine  read_ucd_mesh_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_udt_mesh_header(ifile_psf, nnod_output,          &
     &          nele_out, ncomp_output)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint_gl), intent(in) :: nnod_output, nele_out
      integer(kind = kint), intent(in) :: ncomp_output
!
!
      write(ifile_psf,'(3i16,2i5,a1)')                                  &
     &           nnod_output, nele_out, ncomp_output, izero, izero
!
      end subroutine write_udt_mesh_header
!
! ----------------------------------------------------------------------
!
      subroutine write_ucd_mesh_connect(ifile_psf, ntot_ele,            &
     &          nnod_ele, nele, iele_gl, ie_gl)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: ifile_psf, nnod_ele
      integer(kind = kint_gl), intent(in) :: ntot_ele, nele
      integer(kind = kint_gl), intent(in) :: iele_gl(ntot_ele)
      integer(kind = kint_gl), intent(in) :: ie_gl(ntot_ele,nnod_ele)
!
      integer(kind = kint_gl) :: iele
!
      character(len=6) :: eleflag
      character(len=kchara) :: fmt_txt
!
      if(nnod_ele.eq.num_t_linear)    write(eleflag,'(a6)') UCD_HEX
      if(nnod_ele.eq.num_triangle)    write(eleflag,'(a6)') UCD_TRI
      if(nnod_ele.eq.num_linear_edge) write(eleflag,'(a6)') UCD_LNE
!
      write(fmt_txt,'(a11,i3,a6)')                                      &
     &                      '(i16,i3,a6,', nnod_ele, '(i16))'
!
      do iele = 1, nele
        write(ifile_psf,fmt_txt) iele_gl(iele), ione,                   &
     &       eleflag, ie_gl(iele,1:nnod_ele)
      end do
!
      end subroutine  write_ucd_mesh_connect
!
! ----------------------------------------------------------------------
!
      end module udt_data_IO
