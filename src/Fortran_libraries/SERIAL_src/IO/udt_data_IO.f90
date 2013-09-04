!udt_data_IO.f90
!      module udt_data_IO
!
!      Written by H. Matsui on July, 2006
!
!      subroutine write_udt_field_header(ifile_psf, num_output,         &
!     &          ncomp_out, name_out)
!      subroutine write_single_udt_data(ifile_psf, nnod_output,         &
!     &          ncomp_dat, inod_out, dat_out)
!      subroutine write_multi_udt_data(ifile_psf, ntot_out, ist, ied,   &
!     &          ncomp_dat, inod_out, dat_out)
!
!      subroutine read_udt_field_header(ifile_psf, num_input,           &
!     &          ncomp_in, name_in)
!      subroutine read_single_udt_data(ifile_psf, nnod_in,              &
!     &          ncomp_dat, dat_in)
!
!
!      subroutine write_udt_mesh_header(ifile_psf, nnod_output,         &
!     &          nele_out, ncomp_output)
!      subroutine write_single_grd_connect(ifile_psf, nnod_4_ele,       &
!     &          nele_out, iele_gl, ie_out)
!      subroutine write_multi_grd_connect(ifile_psf, nnod_4_ele,        &
!     &          ntot_ele, ist, ied, iele_gl, ie_out)
!
      module udt_data_IO
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
      subroutine write_udt_field_header(ifile_psf, num_output,          &
     &          ncomp_out, name_out)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint), intent(in) :: num_output
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len = kchara), intent(in) :: name_out(num_output)
!
      integer(kind = kint) :: j
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a7,i3,a5)')                                       &
     &                    '(i8,a2,', num_output, '(i4))'
!
      write(ifile_psf,fmt_txt) num_output, '  ',                        &
     &                            ncomp_out(1:num_output)
      do j = 1, num_output
        write(ifile_psf,'(a,a1)') trim(name_out(j)), ","
      end do
!
      end subroutine write_udt_field_header
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_single_udt_data(ifile_psf, nnod_output,          &
     &          ncomp_dat, inod_out, dat_out)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint), intent(in) :: nnod_output, ncomp_dat
      integer(kind = kint), intent(in) :: inod_out(nnod_output)
      real(kind = kreal), intent(in) :: dat_out(nnod_output, ncomp_dat)
!
      call write_multi_udt_data(ifile_psf, nnod_output,                 &
     &    ione, nnod_output, ncomp_dat, inod_out, dat_out)
!
      end subroutine  write_single_udt_data
!
! ----------------------------------------------------------------------
!
      subroutine write_multi_udt_data(ifile_psf, ntot_out, ist, ied,    &
     &          ncomp_dat, inod_out, dat_out)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint), intent(in) :: ntot_out, ncomp_dat
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: inod_out(ntot_out)
      real(kind = kreal), intent(in) :: dat_out(ntot_out, ncomp_dat)
!
      integer(kind = kint) :: inod
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a5,i3,a13)')                                      &
     &                '(i10,', ncomp_dat, '(1pE25.15e3))'
      do inod = ist, ied
        write(ifile_psf,fmt_txt)                                        &
     &                  inod_out(inod), dat_out(inod,1:ncomp_dat)
      end do
!
      end subroutine  write_multi_udt_data
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
      subroutine read_single_udt_data(ifile_psf, nnod_in,               &
     &          ncomp_dat, dat_in)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint), intent(in) :: nnod_in, ncomp_dat
      real(kind = kreal), intent(inout) :: dat_in(nnod_in, ncomp_dat)
!
      integer(kind = kint) :: inod, itmp
!
!
      do inod = 1, nnod_in
        read(ifile_psf,*) itmp, dat_in(inod,1:ncomp_dat)
      end do
!
      end subroutine read_single_udt_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_udt_mesh_header(ifile_psf, nnod_output,          &
     &          nele_out, ncomp_output)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint), intent(in) :: nnod_output, nele_out
      integer(kind = kint), intent(in) :: ncomp_output
!
!
      write(ifile_psf,'(3i10,2i5)')                                     &
     &           nnod_output, nele_out, ncomp_output, izero, izero
!
      end subroutine write_udt_mesh_header
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_single_grd_connect(ifile_psf, nnod_4_ele,        &
     &          nele_out, iele_gl, ie_out)
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint), intent(in) :: nele_out, nnod_4_ele
      integer(kind = kint), intent(in) :: iele_gl(nele_out)
      integer(kind = kint), intent(in) :: ie_out(nele_out,nnod_4_ele)
!
!
      call write_multi_grd_connect(ifile_psf, nnod_4_ele,               &
     &    nele_out, ione, nele_out, iele_gl, ie_out)
!
      end subroutine  write_single_grd_connect
!
! ----------------------------------------------------------------------
!
      subroutine write_multi_grd_connect(ifile_psf, nnod_4_ele,         &
     &          ntot_ele, ist, ied, iele_gl, ie_out)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: ifile_psf
      integer(kind = kint), intent(in) :: ntot_ele, nnod_4_ele
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: iele_gl(ntot_ele)
      integer(kind = kint), intent(in) :: ie_out(ntot_ele,nnod_4_ele)
!
      integer(kind = kint) :: iele
      character(len=6) :: eleflag
!
!
      if(nnod_4_ele.eq.num_t_linear)    write(eleflag,'(a6)') '  hex '
      if(nnod_4_ele.eq.num_triangle)    write(eleflag,'(a6)') '  tri '
      if(nnod_4_ele.eq.num_linear_edge) write(eleflag,'(a6)') ' line '
!
      do iele = ist, ied
        write(ifile_psf,'(i10,i3,a6,30i10)') iele_gl(iele), ione,       &
     &       eleflag, ie_out(iele,1:nnod_4_ele)
      end do
!
      end subroutine  write_multi_grd_connect
!
! ----------------------------------------------------------------------
!
      end module udt_data_IO
