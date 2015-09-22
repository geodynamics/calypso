!>@file  field_data_IO.f90
!!       module field_data_IO
!!
!!@author H. Matsui
!!@date   Programmed 2007
!!@date   modified in May, 2015
!
!> @brief Data IO rountines for field data IO
!!
!!@verbatim
!!      function field_istack_nod_buffer(nprocs, istack_nod)
!!      function buffer_istack_nod_buffer(nprocs, istack_nod)
!!      function field_num_buffer(num_field)
!!      function field_comp_buffer(num_field, ncomp_field)
!!      function each_field_name_buffer(field_name)
!!      function each_field_data_buffer(ncomp, vect)
!!      integer(kind = kint) function len_each_field_data_buf(ncomp)
!!
!!      subroutine read_field_num_buffer(textbuf, nnod, num_field)
!!      subroutine read_bufer_istack_nod_buffer                         &
!!     &         (textbuf, nprocs, istack_nod)
!!      subroutine read_field_comp_buffer                               &
!!     &         (textbuf, num_field, ncomp_field)
!!      subroutine read_each_field_name_buffer(textbuf, field_name)
!!      subroutine read_each_field_data_buffer(textbuf ncomp, vect)
!!
!!      subroutine write_field_data(id_file, nnod, num_field, ntot_comp,&
!!     &          ncomp_field, field_name, field_data)
!!      subroutine read_field_data(id_file, nnod, num_field, ntot_comp, &
!!     &          ncomp_field, field_name, field_data)
!!
!!      subroutine write_field_data_b(id_file, nnod, num_field,         &
!!     &          ntot_comp, ncomp_field, field_name, field_data)
!!      subroutine read_field_data_b(id_file,                           &
!!     &          nnod, num_field, ntot_comp, field_name, field_data)
!!@endverbatim
!
      module field_data_IO
!
      use m_precision
!
      implicit none
!
      character(len=25), parameter                                      &
     &            :: FLD_HD0 = '! List of Number of nodes'
      character(len=31), parameter                                      &
     &            :: FLD_HD1 = '! Number of field and component'
!
      private :: FLD_HD1
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      function field_istack_nod_buffer(nprocs, istack_nod)
!
      integer(kind = kint), intent(in) ::    nprocs
      integer(kind = kint_gl), intent(in) :: istack_nod(0:nprocs)
      character(len=25+1+nprocs*16+1) :: field_istack_nod_buffer
!
!
      field_istack_nod_buffer = FLD_HD0  // char(10)                    &
     &         // buffer_istack_nod_buffer(nprocs, istack_nod)
!
      end function field_istack_nod_buffer
!
! -------------------------------------------------------------------
!
      function buffer_istack_nod_buffer(nprocs, istack_nod)
!
      integer(kind = kint), intent(in) ::    nprocs
      integer(kind = kint_gl), intent(in) :: istack_nod(0:nprocs)
      character(len=nprocs*16+1) :: buffer_istack_nod_buffer
!
      character(len=nprocs*16) :: buf_nfld
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i7,a9)') '(', nprocs, '(i16),a1)'
      write(buf_nfld,fmt_txt) istack_nod(1:nprocs)
!
      buffer_istack_nod_buffer = buf_nfld // char(10)
!
      end function buffer_istack_nod_buffer
!
! -------------------------------------------------------------------
!
      function field_num_buffer(num_field)
!
      integer(kind = kint), intent(in) ::    num_field
      character(len=31+1+16+1) :: field_num_buffer
!
      character(len=16) :: buf_nfld
!
      write(buf_nfld,'(i16,a1)') num_field
!
      field_num_buffer = FLD_HD1  // char(10)                           &
     &               //  buf_nfld // char(10)
!
      end function field_num_buffer
!
! -------------------------------------------------------------------
!
      function field_comp_buffer(num_field, ncomp_field)
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len=num_field*5+1) :: field_comp_buffer
!
      character(len=kchara) :: fmt_txt
!
      write(fmt_txt,'(a1,i5,a8)') '(', num_field, '(i5),a1)'
!
      write(field_comp_buffer,fmt_txt)                                  &
     &            ncomp_field(1:num_field), char(10)
!
      end function field_comp_buffer
!
! -------------------------------------------------------------------
!
      function each_field_name_buffer(field_name)
!
      character(len=kchara), intent(in) :: field_name
!
      character(len=len_trim(field_name)+1) :: each_field_name_buffer
!
!
      each_field_name_buffer = trim(field_name) // char(10)
!
      end function each_field_name_buffer
!
! -------------------------------------------------------------------
!
      function each_field_data_buffer(ncomp, vect)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: vect(ncomp)
!
      character(ncomp*25+1) :: each_field_data_buffer
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i1,a16)') '(', ncomp, '(1pE25.15e3),a1)'
!
      write(each_field_data_buffer,fmt_txt) vect(1:ncomp), char(10)
!
      end function each_field_data_buffer
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function len_each_field_data_buf(ncomp)
!
      integer(kind = kint), intent(in) :: ncomp
!
      len_each_field_data_buf = ncomp*25+1
!
      end function len_each_field_data_buf
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine read_field_istack_nod_buffer                           &
     &         (textbuf, nprocs, istack_nod)
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=25+1+nprocs*16+1), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: istack_nod(0:nprocs)
!
      character(len=nprocs*16) ::    tmp1
!
      tmp1 = textbuf(27:27+nprocs*16)
      istack_nod(0) = 0
      read(tmp1,*) istack_nod(1:nprocs)
!
      end subroutine read_field_istack_nod_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_bufer_istack_nod_buffer                           &
     &         (textbuf, nprocs, istack_nod)
!
      integer(kind = kint), intent(in) :: nprocs
      character(nprocs*16+1), intent(in) :: textbuf
      integer(kind = kint_gl), intent(inout) :: istack_nod(0:nprocs)
!
      character(len=nprocs*16) ::    tmp1
!
      tmp1 = textbuf(1:nprocs*16)
      istack_nod(0) = 0
      read(tmp1,*) istack_nod(1:nprocs)
!
      end subroutine read_bufer_istack_nod_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_field_num_buffer(textbuf, num_field)
!
      character(len=49), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: num_field
!
      character(len=16) ::    tmp1
!
      tmp1 = textbuf(33:48)
      read(tmp1,*) num_field
!
      end subroutine read_field_num_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_field_comp_buffer                                 &
     &         (textbuf, num_field, ncomp_field)
!
      integer(kind = kint), intent(in) :: num_field
      character(len=num_field*5+1), intent(in) :: textbuf
      integer(kind = kint), intent(inout) :: ncomp_field(num_field)
!
      character(len=num_field*5) ::    tmp1
!
!
      tmp1 = textbuf(1:num_field*5)
      read(tmp1,*) ncomp_field(1:num_field)
!
      end subroutine read_field_comp_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_each_field_name_buffer(textbuf, field_name)
!
      character(len=kchara), intent(in) :: textbuf
      character(len=kchara), intent(inout) :: field_name
!
!
      read(textbuf,*) field_name
!
      end subroutine read_each_field_name_buffer
!
! -------------------------------------------------------------------
!
      subroutine read_each_field_data_buffer(textbuf, ncomp, vect)
!
      integer(kind = kint), intent(in) :: ncomp
      character(len=ncomp*25+1), intent(in) :: textbuf
      real(kind = kreal), intent(inout) :: vect(ncomp)
!
      character(len=ncomp*25) ::    tmp1
!
      tmp1 = textbuf(1:ncomp*25)
      read(tmp1,*) vect(1:ncomp)
!
      end subroutine read_each_field_data_buffer
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_field_data(id_file, nnod, num_field, ntot_comp,  &
     &          ncomp_field, field_name, field_data)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: field_data(nnod, ntot_comp)
!
      integer(kind = kint) :: i_fld, icou, ist, inod
      character(len=kchara) :: fmt_txt
!
!
      write(id_file,'(a)'   ) FLD_HD1
      write(id_file,'(2i16)') nnod, num_field
      write(id_file,'(10i5)') ncomp_field(1:num_field)
!
      icou = 0
      do i_fld = 1, num_field
        write(id_file,'(a)') trim(field_name(i_fld))
!
        ist = icou + 1
        icou = icou + ncomp_field(i_fld)
        write(fmt_txt,'(a1,i1,a16)')                                    &
     &                   '(', ncomp_field(i_fld), '(1pE25.15e3),a1)'
        do inod = 1, nnod
          write(id_file,fmt_txt) field_data(inod,ist:icou)
        end do
      end do
!
      end subroutine write_field_data
!
! -------------------------------------------------------------------
!
      subroutine read_field_data(id_file, nnod, num_field, ntot_comp,   &
     &          ncomp_field, field_name, field_data)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
!
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: field_data(nnod, ntot_comp)
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: i_fld, icou, ist, inod
!
!
      icou = 0
      do i_fld = 1, num_field
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) field_name(i_fld)
!
        ist = icou + 1
        icou = icou + ncomp_field(i_fld)
        do inod = 1, nnod
          read(id_file,*)  field_data(inod,ist:icou)
        end do
      end do
!
      end subroutine read_field_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_field_data_b(id_file, nnod, num_field,           &
     &          ntot_comp, ncomp_field, field_name, field_data)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: field_data(nnod, ntot_comp)
!
      integer(kind = kint) :: i
!
!
      write(id_file) nnod, num_field
      write(id_file) ncomp_field(1:num_field)
      write(id_file) field_name(1:num_field)
!
      do i = 1, ntot_comp
        write(id_file) field_data(1:nnod,i)
      end do
!
      end subroutine write_field_data_b
!
! -------------------------------------------------------------------
!
      subroutine read_field_data_b(id_file,                             &
     &          nnod, num_field, ntot_comp, field_name, field_data)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ntot_comp
!
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: field_data(nnod, ntot_comp)
!
      integer(kind = kint) :: i
!
!
      read(id_file) field_name(1:num_field)
!
      do i = 1, ntot_comp
        read(id_file) field_data(1:nnod,i)
      end do
!
      end subroutine read_field_data_b
!
! -------------------------------------------------------------------
!
      end module field_data_IO
