!>@file  ucd_data_to_buffer.f90
!!       module ucd_data_to_buffer
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for UCD data segments
!!
!!@verbatim
!!      function ucd_num_comps(num_output, ncomp_out)
!!      function ucd_field_name(field_name)
!!      function ucd_each_field(inod_gl, ncomp_dat, dat_out)
!!
!!      function ucd_connect_head(nnod, nele, ncomp)
!!      function ucd_each_connect(iele_gl, nnod_ele, ie0)
!!@endverbatim
!
      module ucd_data_to_buffer
!
      use m_precision
      use m_constants
!
      implicit  none
!
      character(len=*), parameter :: UCD_HEX = '  hex '
      character(len=*), parameter :: UCD_TRI = '  tri '
      character(len=*), parameter :: UCD_LNE = ' line '
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      function ucd_num_comps(num_output, ncomp_out)
!
      integer(kind = kint), intent(in) :: num_output
      integer(kind = kint), intent(in) :: ncomp_out(num_output)
      character(len=8+2+num_output*4+1) :: ucd_num_comps
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a7,i3,a8)')                                       &
     &                    '(i8,a2,', num_output, '(i4),a1)'
!
      write(ucd_num_comps,fmt_txt) num_output, '  ',                    &
     &                            ncomp_out(1:num_output), char(10)
! 
      end function ucd_num_comps
!
! -----------------------------------------------------------------------
!
      function ucd_field_name(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
      character(len=len_trim(field_name) +1+1) :: ucd_field_name
!
!
      ucd_field_name = trim(field_name) // ',' // char(10)
!
      end function ucd_field_name
!
! ----------------------------------------------------------------------
!
      function ucd_each_field(inod_gl, ncomp_dat, dat_out)
!
      integer(kind = kint), intent(in) :: ncomp_dat
      integer(kind = kint_gl), intent(in) :: inod_gl
      real(kind = kreal), intent(in) :: dat_out(ncomp_dat)
!
      character(len=16+ncomp_dat*23+1) :: ucd_each_field
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a5,i4,a16)')                                      &
     &                '(i16,', ncomp_dat, '(1pE23.12e3),a1)'
      write(ucd_each_field,fmt_txt)                                     &
     &                  inod_gl, dat_out(1:ncomp_dat), char(10)
!
      end function  ucd_each_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      function ucd_connect_head(nnod, nele, ncomp)
!
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: ncomp
!
      character(len=3*16+2*5+1) :: ucd_connect_head
!
!
      write(ucd_connect_head,'(3i16,2i5,a1)')                           &
     &           nnod, nele, ncomp, izero, izero, char(10)
!
      end function ucd_connect_head
!
! ----------------------------------------------------------------------
!
      function ucd_each_connect(iele_gl, nnod_ele, ie0)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: iele_gl
      integer(kind = kint_gl), intent(in) :: ie0(nnod_ele)
!
      character(len=16+3+6+16*nnod_ele+1) :: ucd_each_connect
      character(len=6) :: eleflag
      character(len=kchara) :: fmt_txt
!
!
      if(nnod_ele.eq.num_t_linear)    write(eleflag,'(a6)') UCD_HEX
      if(nnod_ele.eq.num_triangle)    write(eleflag,'(a6)') UCD_TRI
      if(nnod_ele.eq.num_linear_edge) write(eleflag,'(a6)') UCD_LNE
!
      write(fmt_txt,'(a11,i3,a9)')                                      &
     &                      '(i16,i3,a6,', nnod_ele, '(i16),a1)'
!
      write(ucd_each_connect,fmt_txt) iele_gl, ione,                    &
     &       eleflag, ie0(1:nnod_ele), char(10)
!
      end function ucd_each_connect
!
! ----------------------------------------------------------------------
!
      end module ucd_data_to_buffer
