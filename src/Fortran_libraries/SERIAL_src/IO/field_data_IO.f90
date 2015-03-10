!field_data_IO.f90
!      module field_data_IO
!
!      Written by H. Matsui on Oct., 2007
!
!      subroutine write_field_data(id_file, nnod, num_field, ntot_comp, &
!     &          ncomp_field, field_name, field_data)
!      subroutine read_field_data(id_file, nnod, num_field, ntot_comp,  &
!     &          ncomp_field, field_name, field_data)
!
!      subroutine write_field_data_b(id_file, nnod, num_field,          &
!     &          ntot_comp, ncomp_field, field_name, field_data)
!      subroutine read_field_data_b(id_file,                            &
!     &          nnod, num_field, ntot_comp, field_name, field_data)
!
      module field_data_IO
!
      use m_precision
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
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
      write(id_file,'(a)'   )  '! number of field and component'
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
