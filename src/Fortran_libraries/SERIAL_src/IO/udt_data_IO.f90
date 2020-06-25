!>@file  udt_data_IO.f90
!!       module udt_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for UCD data segments
!!
!!@verbatim
!!      subroutine write_udt_field_header                               &
!!     &         (id_ucd, num_output, ncomp_out, name_out)
!!      subroutine write_ucd_field_data                                 &
!!     &         (id_ucd, ntot_out, ncomp_dat, nnod, inod_out, dat_out)
!!      subroutine write_udt_mesh_header                                &
!!     &         (id_ucd, nnod_output, nele_out, ncomp_output)
!!      subroutine write_ucd_mesh_connect                               &
!!     &         (id_ucd, ntot_ele, nnod_ele, nele, iele_gl, ie_gl)
!!
!!      subroutine read_udt_field_num(id_ucd, num_input)
!!      subroutine read_udt_field_name                                  &
!!     &         (id_ucd, num_input, ncomp_in, name_in)
!!      subroutine read_ucd_field_data                                  &
!!     &         (id_ucd, nnod_in, ncomp_dat, dat_in)
!!      subroutine read_ucd_mesh_header                                 &
!!     &         (id_ucd, nnod_in, nele_in, ntot_comp)
!!      subroutine read_ucd_node_data(id_ucd, nnod_in, inod_gl, xx_in)
!!      subroutine read_ucd_ele_connect                                 &
!!     &         (id_ucd, nele_in, nnod_ele, iele_gl, ie_in)
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
      subroutine write_udt_field_header                                 &
     &         (id_ucd, num_output, ncomp_out, name_out)
!
      integer(kind = kint), intent(in) :: id_ucd
      integer(kind = kint), intent(in) :: num_output
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
!
      integer(kind = kint) :: j
!
!
      write(id_ucd,'(a)',advance='NO')                                  &
     &         ucd_num_comps(num_output, ncomp_out)
      do j = 1, num_output
        write(id_ucd,'(a)',advance='NO') ucd_field_name(name_out(j))
      end do
!
      end subroutine write_udt_field_header
!
! ----------------------------------------------------------------------
!
      subroutine write_ucd_field_data                                   &
     &         (id_ucd, ntot_out, ncomp_dat, nnod, inod_out, dat_out)
!
      integer(kind = kint), intent(in) :: id_ucd, ncomp_dat
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
        write(id_ucd,fmt_txt) inod_out(inod), dat_out(inod,1:ncomp_dat)
      end do
!
      end subroutine  write_ucd_field_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_udt_mesh_header                                  &
     &         (id_ucd, nnod_output, nele_out, ncomp_output)
!
      integer(kind = kint), intent(in) :: id_ucd
      integer(kind = kint_gl), intent(in) :: nnod_output, nele_out
      integer(kind = kint), intent(in) :: ncomp_output
!
!
      write(id_ucd,'(3i16,2i5,a1)')                                     &
     &           nnod_output, nele_out, ncomp_output, izero, izero
!
      end subroutine write_udt_mesh_header
!
! ----------------------------------------------------------------------
!
      subroutine write_ucd_mesh_connect                                 &
     &         (id_ucd, ntot_ele, nnod_ele, nele, iele_gl, ie_gl)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: id_ucd, nnod_ele
      integer(kind = kint_gl), intent(in) :: ntot_ele, nele
      integer(kind = kint_gl), intent(in) :: iele_gl(ntot_ele)
      integer(kind = kint_gl), intent(in) :: ie_gl(ntot_ele,nnod_ele)
!
      integer(kind = kint_gl) :: iele
      character(len=kchara) :: fmt_txt
!
      write(fmt_txt,'(a11,i3,a6)')                                      &
     &                      '(i16,i3,a6,', nnod_ele, '(i16))'
!
      do iele = 1, nele
        write(id_ucd,fmt_txt) iele_gl(iele), ione,                      &
     &       ucd_eletype(nnod_ele), ie_gl(iele,1:nnod_ele)
      end do
!
      end subroutine  write_ucd_mesh_connect
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_udt_field_num(id_ucd, num_input)
!
      integer(kind = kint), intent(in) :: id_ucd
!
      integer(kind = kint), intent(inout) :: num_input
!
!
      read(id_ucd,*) num_input
      backspace(id_ucd)
!
      end subroutine read_udt_field_num
!
! ----------------------------------------------------------------------
!
      subroutine read_udt_field_name                                    &
     &         (id_ucd, num_input, ncomp_in, name_in)
!
      integer(kind = kint), intent(in) :: id_ucd
!
      integer(kind = kint), intent(in) :: num_input
      integer(kind = kint), intent(inout) :: ncomp_in(num_input)
      character(len = kchara), intent(inout) :: name_in(num_input)
!
      integer(kind = kint) :: num, j
!
!
      read(id_ucd,*) num, ncomp_in(1:num_input)
      if(num .ne. num_input) write(*,*) 'Error in number of field'
!
      do j = 1, num_input
        read(id_ucd,*) name_in(j)
      end do
!
      end subroutine read_udt_field_name
!
! ----------------------------------------------------------------------
!
      subroutine read_ucd_field_data                                    &
     &         (id_ucd, nnod_in, ncomp_dat, dat_in)
!
      integer(kind = kint), intent(in) :: id_ucd, ncomp_dat
      integer(kind = kint_gl), intent(in) :: nnod_in
      real(kind = kreal), intent(inout) :: dat_in(nnod_in, ncomp_dat)
!
      integer(kind = kint_gl) :: inod, itmp
!
!
      do inod = 1, nnod_in
        read(id_ucd,*) itmp, dat_in(inod,1:ncomp_dat)
      end do
!
      end subroutine read_ucd_field_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_ucd_mesh_header                                   &
     &         (id_ucd, nnod_in, nele_in, ntot_comp)
!
      integer(kind = kint), intent(in) :: id_ucd
      integer(kind = kint_gl), intent(inout) :: nnod_in, nele_in
      integer(kind = kint), intent(inout) :: ntot_comp
!
      integer(kind = kint) :: itmp
!
      read(id_ucd,*) nnod_in, nele_in, ntot_comp, itmp, itmp
!
      end subroutine read_ucd_mesh_header
!
!-----------------------------------------------------------------------
!
      subroutine read_ucd_node_data(id_ucd, nnod_in, inod_gl, xx_in)
!
      integer(kind=kint), intent(in) :: id_ucd
      integer(kind=kint_gl), intent(in) :: nnod_in
      integer(kind=kint_gl), intent(inout) :: inod_gl(nnod_in)
      real(kind = kreal), intent(inout) :: xx_in(nnod_in,3)
!
      integer(kind = kint_gl) :: inod
!
!
      do inod = 1, nnod_in
        read(id_ucd,*) inod_gl(inod), xx_in(inod,1:3)
      end do
!
      end subroutine  read_ucd_node_data
!
! ----------------------------------------------------------------------
!
      subroutine read_ucd_ele_connect                                   &
     &         (id_ucd, nele_in, nnod_ele, iele_gl, ie_in)
!
      integer(kind=kint), intent(in) :: id_ucd, nnod_ele
      integer(kind=kint_gl), intent(in) :: nele_in
      integer(kind=kint_gl), intent(inout) :: iele_gl(nele_in)
      integer(kind=kint_gl), intent(inout) :: ie_in(nele_in,nnod_ele)
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint) :: itmp
      character(len = kchara) :: eleflag
!
!
      do iele = 1, nele_in
        read(id_ucd,*) iele_gl(iele), itmp,                             &
     &                   eleflag, ie_in(iele,1:nnod_ele)
      end do
!
      if((nnod_ele_by_ucd_eletype(eleflag) .ne. nnod_ele)) then
        write(*,*) 'Error in element type'
      end if
!
      end subroutine  read_ucd_ele_connect
!
! ----------------------------------------------------------------------
!
      end module udt_data_IO
